package shapeless
package examples

case class Name(override val toString: String) extends AnyVal

trait HasName[T] {
  def name(t: T): Name
}

object HasName extends CoproductTypeClassApplyInstanceCompanion[HasName, Name] {
  def point[T](f: (T) => Name): HasName[T] = new HasName[T] {
    def name(t: T): Name = f(t)
  }
  def applyInstance[T](instance: => HasName[T], t: T): Name = instance.name(t)

  implicit val int: HasName[Int] = point(_ => Name("Int"))
  implicit val double: HasName[Double] = point(_ => Name("Double"))
  implicit val boolean: HasName[Boolean] = point(_ => Name("Boolean"))
  implicit val string: HasName[String] = point(_ => Name("String"))
}

trait HasNameSyntax {
  def getName: Name
}

object HasNameSyntax {
  implicit def toHasNameSyntax[T](t: T)(implicit ev: HasName[T]) = new HasNameSyntax {
    def getName: Name = ev.name(t)
  }
}

object NameExample extends App {
  import HasNameSyntax._
  type A = Int :+: Boolean :+: Double :+: String :+: CNil
  val i = Coproduct[A](12)
  val d = Coproduct[A](101.12)
  val b = Coproduct[A](true)
  val s = Coproduct[A]("abcd")

  assert(i.getName == Name("Int"))
  assert(d.getName == Name("Double"))
  assert(b.getName == Name("Boolean"))
  assert(s.getName == Name("String"))
}
