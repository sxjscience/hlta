package tm.hlta

import tm.util.Data
import org.latlab.util.Variable

abstract class Column(variable: Variable){
  
  type T <: Any
  
  def getVariable:Variable = variable
  
  def getValues:T
  
}

object BinaryColumn{  
  def fromData(data: Data, variable: Variable): BinaryColumn = {
    val index = data.variables.indexOf(variable)
    val instances = data.instances    
    val list = (0 until instances.size)
      .filter(d => instances(d).values(index) > 0).toList //binary index type sparse vector
    new BinaryColumn(variable, list)
  }
}

class BinaryColumn(variable: Variable, values: List[Int]) extends Column(variable){
  type T = List[Int]
  override def getValues = values
}

class DoubleColumn(variable: Variable, values: Vector[Double]) extends Column(variable){
  type T = Vector[Double]
  override def getValues = values 
}