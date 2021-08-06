package shapeless

import shapeless.nat._

import scala.deriving.Mirror
import scala.annotation.Annotation as saAnnotation
import shapeless.NotContainsConstraint.NotContains
import shapeless.labelled.->>
import shapeless.ops.coproduct.{ExtendRight, ExtendRightBy}
import shapeless.ops.hlist.{SelectAll, ToSized}
import shapeless.ops.nat.Sum
import shapeless.ops.record.{Extractor, Remover}
import shapeless.test.{illTyped, typed}

@main def testing: Unit = {
  type R = ("a" ->> Int) :: ("b" ->> String) :: HNil
  /*
  type RM1 = ("a" ->> Int) :: HNil

  val r = Remover[R, "a"]
  val e1 = Extractor[R, RM1](
    Extractor.extract(
      summon,
      summon,
      summon,
      summon,
    )
  )
  */

  /*
  val e2 = ExtendRightBy[Boolean :+: CNil, String :+: Boolean :+: CNil](
    ExtendRightBy.extendRightByCoproduct(
      summon[ExtendRight[Boolean :+: CNil, String]],
      summon
    )
  )
  */

  //SelectAll[R, ("a" ->> Int) :: HNil]
  //SelectAll[("a" ->> Int) :: ("b" ->> Boolean) :: ("c" ->> String) :: HNil]

  /*
  object toInt extends Poly1 {
    implicit def default[N <: Nat](implicit toi: ops.nat.ToInt[N]): Case.Aux[N, Int] = at[N](_ => toi())
  }


  def range[R <: HList](a: Nat, b: Nat)(implicit
                                        range: ops.nat.Range.Aux[a.N, b.N, R],
                                        mapper: ops.hlist.Mapper[toInt.type, R]
  ) = mapper(range())

  (0, 1) ::(2, 3) ::(4, 'a') :: HNil == range(0, 5).group(2, 2, 'a' :: HNil)
  (0, 1) ::(2, 3) ::(4, 'a') :: HNil == range(0, 5).group(2, 2, 'a' :: 'b' :: 'c' :: HNil)
  */

  /*
  trait MyNat
  case class Succ[P <: MyNat]() extends MyNat
  class _0 extends MyNat

  trait MySum[A <: MyNat, B <: MyNat] extends Serializable { type Out <: MyNat }
  object MySum {
    def apply[A <: MyNat, B <: MyNat](implicit sum: MySum[A, B]): Aux[A, B, sum.Out] = sum

    type Aux[A <: MyNat, B <: MyNat, C <: MyNat] = MySum[A, B] { type Out = C }

    implicit def sum1[B <: MyNat]: Aux[_0, B, B] = new MySum[_0, B] { type Out = B }
    implicit def sum2[A <: MyNat, B <: MyNat, C <: MyNat]
    (implicit sum : MySum.Aux[A, Succ[B], C]): MySum.Aux[Succ[A], B, C] = new MySum[Succ[A], B] { type Out = C }
  }
  import MySum.{sum1, sum2}

  //implicitly[MySum.Aux[Succ[Succ[_0]], Succ[_0], Succ[Succ[Succ[_0]]]]]

  import Sum.{sum1, sum2}
  scala.compiletime.ops.int

  implicitly[Sum.Aux[_2, _1, _3]]
  //implicitly[Sum.Aux[_2, _3, _5]]
  */
}
