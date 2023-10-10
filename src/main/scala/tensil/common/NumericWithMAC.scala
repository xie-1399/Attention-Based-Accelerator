package tensil.common

trait NumericWithMAC[T] extends Numeric[T] {
  def mac(x: T, y: T, z: T): T
}
