package tensil.common

import org.scalatest._

import scala.reflect.ClassTag
import org.scalatest.flatspec.AnyFlatSpec
import matchers.should._

import Numeric.Implicits._
import scala.util.Random

 /*
  * test about the common until of the numeric
  * the common until show a relax way to represent the number
  * copy from : https://github.com/tensil-ai/tensil/tree/main/common
 */

object Simulation extends App{
  /* the float 32 */
  val randomValue = Random.nextFloat() + Random.nextInt(10)
  println("Random Value : " + randomValue)
  val bytes = Float32.toBytes(randomValue)  /* convert float32 to bytes */

  /* the fixed value */
  val fixed = Fixed8bp4.fromFloat(randomValue)
  println(fixed.toFloat())

}

class Simulation extends AnyFlatSpec{
  behavior of "Fixed"

  implicit val numeric = FloatAsIfIntegralWithMAC

  it should "Fixed16bp8 matrix multiplication within error" in {
    val a =
      Array(
        Array(1.0f, 2.0f, 3.0f),
        Array(4.0f, 5.0f, 6.0f),
        Array(7.0f, 8.0f, 9.0f)
      )
    val b =
      Array(Array(.1f, .2f, .3f), Array(.4f, .5f, .6f), Array(.7f, .8f, .9f))

    val yExpected = Ops.matMul(a, b).flatten

    val af = a.map(_.map(Fixed16bp8.fromDouble(_)))
    val bf = b.map(_.map(Fixed16bp8.fromDouble(_)))

    val yf = Ops.matMul(af, bf)

    val y = yf.map(_.map(_.toDouble())).flatten

    val e = 0.01f

    def equalE(y: Double, yExpected: Double) = {
      println(s"expected = $yExpected, actual = $y")
      y < (yExpected + e) && y > (yExpected - e)
    }

    for (i <- 0 until yExpected.size)
      assert(equalE(y(i), yExpected(i)))
  }

  it should "Fixed18bp10 to bytes and from bytes" in {
    val e = 0.0001f

    def equalE(y: Double, yExpected: Double) = {
      println(s"expected = $yExpected, actual = $y")
      y < (yExpected + e) && y > (yExpected - e)
    }

    val fs = List(
      Fixed18bp10.MaxValue.toDouble(),
      Fixed18bp10.MinValue.toDouble(),
      0.9110825,
      -0.9110825
    )

    for (f <- fs) {
      val bytes = Fixed18bp10.toBytes(Fixed18bp10.fromDouble(f))
      assert(equalE(Fixed18bp10.fromBytes(bytes).toDouble(), f))
    }
  }


}