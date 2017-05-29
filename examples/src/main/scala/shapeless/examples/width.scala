package shapeless
package examples

trait Width[T] {
  def width(t: T): Int
}

object Width extends TypeClassCompanion1[Width, Int] {
  def point[T](f: (T) => Int): Width[T] = new Width[T] {
    def width(t: T): Int = f(t)
  }
  def applyInstance[T](instance: => Width[T], t: T): Int = instance.width(t)
  val zero: Int = 0
  def append(left: Int, right: Int): Int = left + right

  implicit val int: Width[Int] = point(_ => 1)
  implicit val boolean: Width[Boolean] = point(_ => 1)
  implicit val double: Width[Double] = point(_ => 1)
  implicit val string: Width[String] = point(_.length)
}

trait WidthSyntax {
  def width: Int
}

object WidthSyntax {
  implicit def ToWidthSyntax[T](t: T)(implicit ev: Width[T]) = new WidthSyntax {
    def width = ev.width(t)
  }
}

object WidthExample extends App {
  import WidthSyntax._
  case class Foo(i: Int, d: Double, b: Boolean, s: String)
  val foo = Foo(12, 13.0, false, "hello")
  assert(foo.width == 8)
}
