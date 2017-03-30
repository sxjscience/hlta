package tm.hlta

import org.latlab.model.LTM
import org.latlab.reasoner.CliqueTreePropagation
import org.latlab.util.Variable
import org.latlab.model.BeliefNode
import org.latlab.util.Function
import tm.util.Tree
import collection.JavaConversions._
import tm.util.Data
import org.latlab.util.DataSet
import org.latlab.learner.ParallelEmLearner
import scala.annotation.tailrec
import java.util.ArrayList
import java.io.PrintWriter
import org.slf4j.LoggerFactory
import java.text.DecimalFormat

object AssignNarrowTopics {
  /**
   *  Holds a topic variable and a list of document indices that belong to that
   *  topic.  The indices should be sorted in ascending order.
   */
  val MAX_NUMBER_OF_WORDS = 7

  def main(args: Array[String]) {
    if (args.length < 3)
      printUsage()
    else
      run(args(0), args(1), args(2))
  }

  case class Context(marginals: Map[Variable, Function], data: Data, hlcmData: DataSet)

  def printUsage() = {
    println("FindNarrowlyDefinedTopics model_file data_file output")
    println
    println("e.g. FindNarrowlyDefinedTopics model.bif data.arff output")
    println("The output file will be output.narrow.json and output.narrow.arff")
  }
  
  val logger = LoggerFactory.getLogger(AssignNarrowTopics.getClass)

  def run(modelFile: String, dataFile: String, outputName: String) = {
    val topicDataFile = outputName + ".narrow.arff";
    //TODO: if file exists, use existing file    
    
    val (model, data) = Reader.readLTMAndARFFData(modelFile, dataFile)
    val binaryData = data.binary
    val hlcmData = data.toHLCMData
    val trees = HLTA.buildTopicTree(model)

    // Computes the marginal probability of each latent variable.
    logger.info("Computing marginals of latent variables")
    val ctp = new CliqueTreePropagation(model)
    ctp.propagate
    val marginals = model.getInternalVars.map(v => (v, ctp.computeBelief(v))).toMap

    logger.info("Compute topic assignhment by subtree")
    implicit val c = Context(marginals, binaryData, hlcmData)
    val assignments = trees.map(processSubTree)

    logger.info("Saving topic assignments")
    val assignmentsData = new ColumnwiseData(assignments.flatMap(_.toList), binaryData.instances.map(_.weight)).toData()
    assignmentsData.saveAsArff(outputName + "-topics", topicDataFile, new DecimalFormat("#0.##"))
    
    logger.info("Generating topic map")
    val map = generateTopicToDocumentMap(assignmentsData, 0.5)
    
    logger.info("Saving topic map")
    writeTopicMap(map, outputName + ".narrow.json")
  }
  
  /**
   * Processes subtree of topic variables (latent variables) from level = 1..root
   */
  def processSubTree(tree: Tree[BeliefNode])(implicit c: Context): Tree[Column] = {
    val children = tree.children.filterNot(_.value.isLeaf).par.map(processSubTree).toList

    //process the root of this subtree
    val lowerLevel = projectData(tree, c.data, children.map(_.value))
    val variable = tree.value.getVariable
    var lcm = extractLCM(tree, c.marginals(variable)) //build subtree
    val baseData = new ColumnwiseData(lowerLevel, c.data.instances.map(_.weight)).toData()
    lcm = estimate(variable, lcm, baseData.toHLCMData)
    lcm = reorder(variable, lcm) //reorder status
    val assignment = assign(variable, lcm, baseData)
    Tree.node(assignment, children.toSeq)
  }

  def projectData(tree: Tree[BeliefNode], data: Data, childColumns: List[Column]): List[Column] = {
    val childVariables = tree.children.map(_.value.getVariable)

    // if children are observed variables, construct data from the original data
    // otherwise construct data from next level assignment
    if (tree.children.head.value.isLeaf)
      childVariables.map(BinaryColumn.fromData(data, _))
    else {
      // reorder the next level columns
      val m = childColumns.map { x => (x.getVariable, x) }.toMap[Variable, Column]
      childVariables.flatMap(m.get(_))
    }
  }

  /**
   * Parameter re-learning
   */
  def estimate(root: Variable, model: LTM, data: DataSet): LTM = {
    // the model is irregular is it has fewer than 3 leaf nodes
    if (model.getLeafVars.size < 3) {
      estimateByCounting(root, model, data)
    } else {
      runEM(model, data, 100, 64)
    }
  }

  def toArrayList[A](as: A*): ArrayList[A] = new ArrayList(as)

  def estimateByCounting(root: Variable, model: LTM, data: DataSet): LTM = {
    val m = model.clone
    val rootNode = m.getNode(root)
    val leafNodes = rootNode.getChildren.map(_.asInstanceOf[BeliefNode])

    val projected = data.project(new ArrayList(leafNodes.map(_.getVariable)))//count

    def sumWeights[T <: { def getWeight(): Double }](ds: Seq[T]) =
      ds.map(_.getWeight).sum

    val (zeros, others) = projected.getData.partition(_.getStates.forall(_ == 0))//y01[0], y01[1]
    val numberOfZeros = sumWeights(zeros)

    rootNode.getCpt.getCells()(0) = numberOfZeros
    rootNode.getCpt.getCells()(1) = sumWeights(others)
    rootNode.getCpt.normalize()

    leafNodes.foreach { l =>
      val i = projected.getVariables.indexOf(l.getVariable)
      val vs = new ArrayList(Seq(root, l.getVariable))
      val cpt = Function.createFunction(vs)
      cpt.setCell(vs, new ArrayList(Seq[Integer](0, 0)), numberOfZeros)//cell[0] before normalize
      cpt.setCell(vs, new ArrayList(Seq[Integer](0, 1)), 0)//cell[2] before normalize

      val (d0, d1) = others.partition(_.getStates()(i) == 0)
      cpt.setCell(vs, new ArrayList(Seq[Integer](1, 0)), sumWeights(d0))//cell[1], [count(0,-)]/[totalcount-count(0,0)]
      cpt.setCell(vs, new ArrayList(Seq[Integer](1, 1)), sumWeights(d1))//cell[3], [count(1,-)]/[totalcount-count(0,0)]
      cpt.normalize(l.getVariable)
      
      l.setCpt(cpt)
    }

    m
  }

  def runEM(model: LTM, data: DataSet, maxSteps: Int, restarts: Int): LTM = {
    val l = new ParallelEmLearner()
    l.setLocalMaximaEscapeMethod("ChickeringHeckerman")
    l.setMaxNumberOfSteps(maxSteps)
    l.setNumberOfRestarts(restarts)
    l.setReuseFlag(false)
    l.setThreshold(0.01)

    l.em(model, data).asInstanceOf[LTM]
  }
  
  def extractLCM(tree: Tree[BeliefNode], marginal: Function): LTM = {
    def convert(node: BeliefNode): (Variable, Option[Function]) =
      (node.getVariable, Some(node.getCpt))

    buildLCM((tree.value.getVariable, Some(marginal)),
      tree.children.map(c => convert(c.value)))
  }

  def buildLCM(parent: (Variable, Option[Function]),
    children: Seq[(Variable, Option[Function])]): LTM = {
    val m = new LTM
    val root = m.addNode(parent._1)
    parent._2.foreach(root.setCpt)

    children.foreach { c =>
      val cn = m.addNode(c._1)
      m.addEdge(cn, root)
      c._2.map(_.clone).foreach(cn.setCpt)
    }

    m
  }

  def reorder(v: Variable, m: LTM): LTM = {
    val model = m.clone

    val node = model.getNode(v)
    val children = node.getChildren.map(_.asInstanceOf[BeliefNode])
    val sums = (0 until v.getCardinality).map { i =>
      val s = children.map { c =>
        HLTA.getValue(c.getCpt)(IndexedSeq(v, c.getVariable), Array(i, 1))
      }.sum

      (i, s)
    }

    val order = sums.sortBy(_._2).map(_._1)
    node.reorderStates(order.toArray)

    model
  }

  def assign(variable: Variable, model: LTM, data: Data): DoubleColumn = {
    val ctp = new CliqueTreePropagation(model)
    // grouping instances with same value to reduce computation
    val list = data.instances.toVector.map { p =>
        ctp.setEvidence(data.variables.toArray, p.values.map(_.toInt).toArray)
        ctp.propagate
        // return the state that has the highest probability
        ctp.computeBelief(variable).getCells()(1)
      }
    new DoubleColumn(variable, list)
  }
  
  /**
   * Generates a list of documents for each topic.
   *
   * Each map value is a sequence of pairs where first element indicates
   * the probability and second element the document index.
   * The sequence is sorted in descending order of probability.
   */
  def generateTopicToDocumentMap(data: Data, threshold: Double) = {
    (0 until data.variables.size).map { v =>
      val documents = data.instances.view.map(_.values(v)) // map to value of v
        .zipWithIndex
        .filter(_._1 >= threshold).force // keep only those values >= threshold
        .sortBy(-_._1) // sort by descending values
      (data.variables(v), documents)
    }.toMap
  }
  
  def writeTopicMap(map: Map[Variable, Seq[(Double, Int)]], outputFile: String) = {
    val writer = new PrintWriter(outputFile)

    writer.println("[")

    writer.println(map.map { p =>
      val variable = p._1
      val documents = p._2.map(p => "["+s"${p._2}"+","+f"${p._1}%.2f]").mkString(",")
      "{\"topic\":\""+variable.getName+"\",\"doc\":["+documents+"]}"
    }.mkString(",\n"))

    writer.println("]")

    writer.close
  }
}