package tm.hlta

import tm.util.Data
import org.latlab.util.Variable
import scala.annotation.tailrec
import org.latlab.model.BeliefNode
import tm.util.Tree
import java.util.ArrayList

class ColumnwiseData(columns: List[Column], weights: IndexedSeq[Double])  {
  
  private val m = columns.map(c => (c.getVariable, c)).toMap
  
  def getColumn(variable:Variable): Option[Column] = m.get(variable)
  def getWeights: IndexedSeq[Double] = weights
  
  def filter(variables: List[Variable]): ColumnwiseData = {
    val list = variables.flatMap(v => getColumn(v))
    new ColumnwiseData(list, weights)
  }
  
  def toData(): Data = {
    if(columns(0).isInstanceOf[BinaryColumn])
      toData_binary(columns.asInstanceOf[List[BinaryColumn]])
    else
      toData_double(columns.asInstanceOf[List[DoubleColumn]])
  }
  
  private def toData_binary(columns:List[BinaryColumn]): Data = {
    @tailrec
    def loop(i: Int, instances: Vector[Data.Instance], columnsValues: Array[List[Int]]): IndexedSeq[Data.Instance] = {
      if (i >= weights.length) return instances

      val heads = columnsValues.map(_.headOption.getOrElse(weights.size))
      val next = heads.min

      def zeros = Array.fill(columnsValues.length)(0.0)

      if (i == next) {
        // generate an instance with non-zero elements 
        val minIndices = heads.zipWithIndex.filter(_._1 == next).map(_._2)
        val values = zeros
        minIndices.foreach(values(_) = 1)
        val newColumns = minIndices.foldLeft(columnsValues)((cs, i) => cs.updated(i, cs(i).tail))

        loop(i + 1, instances :+ Data.Instance(values, weights(i)), newColumns)
      } else {
        // generate instances with all zero elements and skip to the next
        // instance with non-zero element
        val allZero = for (j <- (i until next))
          yield Data.Instance(zeros, weights(j))
        loop(next, instances ++ allZero, columnsValues)
      }
    }
    
    Data(columns.map(_.getVariable).toIndexedSeq, loop(0, Vector.empty, columns.map(_.getValues).toArray).toIndexedSeq)
  }
  
  def toData_double(columns: List[DoubleColumn]): Data = {
    val transposedLists = columns.map(_.getValues).transpose
    val instances = new Array[Data.Instance](weights.length)
    for (i <- (0 until weights.length)){
      instances(i) = new Data.Instance(transposedLists(i).toArray, weights(i))
    }
    Data(columns.map(_.getVariable).toIndexedSeq, instances.toIndexedSeq)
  }
}

object ColumnwiseData{
  def main(args:Array[String]){
    val states = new ArrayList[String]()
    states.add("s0")
    states.add("s1")
    val c = new BinaryColumn(new Variable("hi", states), List(2, 3, 8))
    val d = new ColumnwiseData(List(c), Seq.fill(10000)(1.0).toIndexedSeq)
    val data = d.toData()
    data.instances.map(_.values.foreach(println))
  }
}