package shapeless
package examples

trait Increment[T] {
  def increment(t: T): T
}

object Increment extends ProductTypeClassApplyInstanceCompanion[Increment] {
  override def point[T](f: (T) => T): Increment[T] = new Increment[T] {
    override def increment(t: T): T = f(t)
  }
  override def applyInstance[T](instance: => Increment[T], t: T): T = instance.increment(t)

  implicit val int: Increment[Int] = point(_ + 1)
  implicit val double: Increment[Double] = point(_ + 1.0)
  implicit val boolean: Increment[Boolean] = point(_ || true)
}

trait IncrementSyntax[T] {
  def increment: T
}

object IncrementSyntax {
  implicit def ToIncrementSyntax[T](t: T)(implicit ev: Increment[T]) = new IncrementSyntax[T] {
    def increment: T = ev.increment(t)
  }
}

object IncrementExample extends App {
  import IncrementSyntax._
  case class Foo(i: Int, d: Double, b: Boolean)
  val foo = Foo(12, 100.32, false)
  val foo2 = Foo(13, 101.32, true)
  assert(foo.increment == foo2)
}
