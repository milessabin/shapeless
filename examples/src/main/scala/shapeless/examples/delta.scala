package shapeless.examples

import shapeless._

object DeltaExamples extends App {
  // A pair of arbitrary case classes
  case class Foo(i : Int, s : String)
  case class Bar(b : Boolean, s : String, of: Option[Bar])

  import DeltaSyntax._

  println("int delta: " + 2.delta(8))
  println("string delta: " + "foo".delta("bar"))
  println("hlist delta: " + (2 :: "foo" :: HNil).delta(8 :: "bar" :: HNil))
  println("foo delta: " + Foo(2, "foo").delta(Foo(8, "food")))

  // Not possible because of diverging implicit expansion (of course, I haven't used Lazy), but I _can't_ use Lazy
  // Any use of Lazy makes Foo.delta blow the stack.

  // println("bar delta: " + Bar(true, "bar", Some(Bar(false, "barb", None))).delta(Bar(false, "bard", None)))
}

trait Delta[In] {
  type Out

  def apply(before: In, after: In): Out
}

object Delta {
  def apply[In](implicit delta: Delta[In]): Delta.Aux[In, delta.Out] = delta

  type Aux[In, Out0] = Delta[In] { type Out = Out0 }

  implicit val intDelta: Delta.Aux[Int, Int] = new Delta[Int] {
    type Out = Int

    def apply(before: Int, after: Int): Out = after - before
  }

  implicit def stringDelta: Delta.Aux[String, (String, String)] = new Delta[String] {
    type Out = (String, String)

    def apply(before: String, after: String): (String, String) = (before, after)
  }

  implicit def deriveHNil: Delta.Aux[HNil, HNil] = new Delta[HNil] {
    type Out = HNil

    def apply(before: HNil, after: HNil): HNil = HNil
  }

  implicit def deriveHCons[H, T <: HList, HOut, TOut <: HList](
    implicit deltaH: Delta.Aux[H, HOut], deltaT: Delta.Aux[T, TOut]
  ): Delta.Aux[H :: T, HOut :: TOut] = new Delta[H :: T] {
    type Out = HOut :: TOut

    def apply(before: H :: T, after: H :: T): Out = {
      deltaH(before.head, after.head) :: deltaT(before.tail, after.tail)
    }
  }

  implicit def generic[F, G, H](
    implicit gen: Generic.Aux[F, G], genDelta: Delta.Aux[G, H]
  ): Delta.Aux[F, H] = new Delta[F] {
    type Out = H

    def apply(before: F, after: F): Out = genDelta.apply(gen.to(before), gen.to(after))
  }
}

object DeltaSyntax {
  implicit class DeltaOps[In](val before: In) extends AnyVal {
    def delta(after: In)(implicit delta: Delta[In]): delta.Out = delta(before, after)
  }
}