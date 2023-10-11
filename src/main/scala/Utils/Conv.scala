package Utils

import scala.collection.mutable._

object Conv{

  /* the conv calculate using scala */
  def convCalculate(filter:Array[Array[BigInt]],featuremap:Array[Array[BigInt]],logout:Boolean = true) = {
    val filterHeight = filter.length
    val filterWidth = filter(0).length
    val inputHeight = featuremap.length
    val inputWidth = featuremap(0).length

    val result = Array.ofDim[BigInt](inputHeight - filterHeight + 1, inputWidth - filterWidth + 1)

    for (i <- 0 until inputHeight - filterHeight + 1) {
      for (j <- 0 until inputWidth - filterWidth + 1) {
        var sum = BigInt(0)
        for (x <- 0 until filterHeight) {
          for (y <- 0 until filterWidth) {
            sum += filter(x)(y) * featuremap(i + x)(j + y)
          }
        }
        result(i)(j) = sum
      }
    }
    if(logout){
      println("Convolution Result:")
      for (row <- result) {
        println(row.mkString(" "))
      }
    }
   result
  }

}