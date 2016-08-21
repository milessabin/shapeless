package shapeless
package examples

trait Zero[T] {
  def zero: T
}

object Zero extends ProductTypeClassIsomorphismCompanion[Zero] {
  def point[T](t: T): Zero[T] = new Zero[T] {
    def zero: T = t
  }
  def unwrap[T](ct: Zero[T]): T = ct.zero

  implicit val int: Zero[Int] = point(0)
  implicit val double: Zero[Double] = point(0.0)
  implicit val boolean: Zero[Boolean] = point(false)
  implicit val string: Zero[String] = point("")
}

object ZeroExample extends App {
  case class Foo(i: Int, b: Boolean, s: String)
  case class Bar(d: Double, f: Foo)
  val bar = Bar(0.0, Foo(0, false, ""))
  assert(Zero[Bar].zero == bar)
}
