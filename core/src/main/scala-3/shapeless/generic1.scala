/*
 * Copyright (c) 2015-18 Miles Sabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package shapeless

import scala.deriving.Mirror

type Apply10[FR[_[_], _[_]], U[_]] = [t[_]] =>> FR[t, U]
type Apply11[FR[_[_], _[_]], U[_]] = [t[_]] =>> FR[U, t]

//Type to make application more explicit in the presence of Apply10 and Apply11
type ApplyF[F[_[_]], A[_]] = F[A]

type ElemTypesToHList[ElemTypes[_] <: scala.Tuple] = [Z] =>> HList.TupleToHList[ElemTypes[Z]]
type ElemTypesToCoproduct[ElemTypes[_] <: scala.Tuple] = [Z] =>> Coproduct.TupleToCoproduct[ElemTypes[Z]]

type MirrorOf1Product[A[_] <: Product] = Mirror.Product {
  type MirroredType[Z] = A[Z]
  type MirroredMonoType = A[Any]
  type MirroredElemTypes[Z] <: scala.Tuple
}

type MirrorOf1Sum[A[_]] = Mirror.Sum {
  type MirroredType[Z] = A[Z]
  type MirroredMonoType = A[Any]
  type MirroredElemTypes[Z] <: scala.Tuple
}

trait Generic1ScalaCompat extends Generic10{

  given mkGeneric10Product[T[_] <: Product, U[_], FR[_[_], _[_]]](
    using m: MirrorOf1Product[T],
    frr: => ApplyF[Apply10[FR, U], ElemTypesToHList[m.MirroredElemTypes]]
  ): Generic1.Aux[T, Apply10[FR, U], ElemTypesToHList[m.MirroredElemTypes]] =
    Generic1.fromProduct[T, Apply10[FR, U]]

  given mkGeneric11Product[T[_] <: Product, U[_], FR[_[_], _[_]]](
    using m: MirrorOf1Product[T],
    frr: => ApplyF[Apply11[FR, U], ElemTypesToHList[m.MirroredElemTypes]]
  ): Generic1.Aux[T, Apply11[FR, U], ElemTypesToHList[m.MirroredElemTypes]] =
    Generic1.fromProduct[T, Apply11[FR, U]]

  given mkGeneric10Sum[T[_], U[_], FR[_[_], _[_]]](
    using m: MirrorOf1Sum[T],
    frr: => ApplyF[Apply10[FR, U], ElemTypesToCoproduct[m.MirroredElemTypes]]
  ): Generic1.Aux[T, Apply10[FR, U], ElemTypesToCoproduct[m.MirroredElemTypes]] =
    Generic1.fromSum[T, Apply10[FR, U]]

  given mkGeneric11Sum[T[_], U[_], FR[_[_], _[_]]](
    using m: MirrorOf1Sum[T],
    frr: => ApplyF[Apply11[FR, U], ElemTypesToCoproduct[m.MirroredElemTypes]]
  ): Generic1.Aux[T, Apply11[FR, U], ElemTypesToCoproduct[m.MirroredElemTypes]] =
    Generic1.fromSum[T, Apply11[FR, U]]
}

trait Generic10ScalaCompat extends Generic10ScalaCompatLowPriority {

  def apply[T[_], FR[_[_]]](implicit gen: Generic1[T, FR]): Generic1.Aux[T, FR, gen.R] = gen

  given[FR[_[_]]](using frr: => FR[Const[HNil]#λ]): Generic1.Aux[Const[Unit]#λ, FR, Const[HNil]#λ] =
    new Generic1[Const[Unit]#λ, FR] {
      override type R[Z] = HNil

      override def to[T](ft: Unit): HNil = HNil

      override def from[T](rt: HNil): Unit = ()

      override def mkFrr: FR[Const[HNil]#λ] = frr
    }

  given[A <: AnyRef & Singleton, FR[_[_]]](
    using v: ValueOf[A], 
    frr: => FR[Const[HNil]#λ]
  ): Generic1.Aux[Const[A]#λ, FR, Const[HNil]#λ] = new Generic1[Const[A]#λ, FR] {
    override type R[Z] = HNil

    override def to[T](ft: A): HNil = HNil

    override def from[T](rt: HNil): A = v.value

    override def mkFrr: FR[Const[HNil]#λ] = frr
  }
}

trait Generic10ScalaCompatLowPriority {

  given fromProduct[A[_] <: Product, FR[_[_]]](
    using m: MirrorOf1Product[A],
    frr: => ApplyF[FR, ElemTypesToHList[m.MirroredElemTypes]]
  ): Generic1.Aux[A, FR, ElemTypesToHList[m.MirroredElemTypes]] = new Generic1[A, FR] {
    override type R[Z] = HList.TupleToHList[m.MirroredElemTypes[Z]]

    override def to[Z](ft: A[Z]): R[Z] =
      HList.tupleToHList(scala.Tuple.fromProduct(ft).asInstanceOf[m.MirroredElemTypes[Z]])

    override def from[Z](rt: R[Z]): A[Z] =
      m.fromProduct(HList.hListToTuple(rt)).asInstanceOf[A[Z]]

    override def mkFrr: FR[R] = frr
  }

  given fromSum[A[_], FR[_[_]]](
    using m: MirrorOf1Sum[A],
    frr: => ApplyF[FR, ElemTypesToCoproduct[m.MirroredElemTypes]]
  ): Generic1.Aux[A, FR, ElemTypesToCoproduct[m.MirroredElemTypes]] = new Generic1[A, FR] {
    override type R[Z] = Coproduct.TupleToCoproduct[m.MirroredElemTypes[Z]]

    override def to[Z](ft: A[Z]): R[Z] =
      Coproduct.coproductFromOrdinal(ft.asInstanceOf[scala.Tuple.Union[m.MirroredElemTypes[Z]]], m.ordinal(ft.asInstanceOf[m.MirroredMonoType]))

    override def from[Z](rt: R[Z]): A[Z] =
      Coproduct.extractCoproduct(rt).asInstanceOf[A[Z]]

    override def mkFrr: FR[R] = frr
  }
}

trait IsHCons1ScalaCompat extends IsHCons10 {

  given mkIsHCons10[L[_], H[_], T[_] <: HList, FH[_[_], _[_]], U[_], FT[_[_]]](
    using splitCons: SplitCons.Aux[L, H, T],
    fhh: => ApplyF[Apply10[FH, U], H],
    ftt: => ApplyF[FT, T],
  ): IsHCons1.Aux[L, Apply10[FH, U], FT, H, T] =
    IsHCons1.fromProduct[L, H, T, Apply10[FH, U], FT]

  given mkIsHCons11[L[_], H[_], T[_] <: HList, FH[_[_], _[_]], U[_], FT[_[_]]](
    using splitCons: SplitCons.Aux[L, H, T],
    fhh: => ApplyF[Apply11[FH, U], H],
    ftt: => ApplyF[FT, T],
  ): IsHCons1.Aux[L, Apply11[FH, U], FT, H, T] =
    IsHCons1.fromProduct[L, H, T, Apply11[FH, U], FT]

  given mkIsHCons12[L[_], H[_], T[_] <: HList, FH[_[_]], FT[_[_], _[_]], U[_]](
    using splitCons: SplitCons.Aux[L, H, T],
    fhh: => ApplyF[FH, H],
    ftt: => ApplyF[Apply10[FT, U], T],
  ): IsHCons1.Aux[L, FH, Apply10[FT, U], H, T] =
    IsHCons1.fromProduct[L, H, T, FH, Apply10[FT, U]]

  given mkIsHCons13[L[_], H[_], T[_] <: HList, FH[_[_]], FT[_[_], _[_]], U[_]](
    using splitCons: SplitCons.Aux[L, H, T],
    fhh: => ApplyF[FH, H],
    ftt: => ApplyF[Apply11[FT, U], T],
  ): IsHCons1.Aux[L, FH, Apply11[FT, U], H, T] =
    IsHCons1.fromProduct[L, H, T, FH, Apply11[FT, U]]
}

trait IsHCons10ScalaCompat {
  def apply[L[_], FH[_[_]], FT[_[_]]](using ev: IsHCons1[L, FH, FT]): IsHCons1.Aux[L, FH, FT, ev.H, ev.T] =
    ev

  given fromProduct[L[_], H0[_], T0[_] <: HList, FH[_[_]], FT[_[_]]](
    using splitCons: SplitCons.Aux[L, H0, T0],
    fhh: => ApplyF[FH, H0],
    ftt: => ApplyF[FT, T0],
  ): IsHCons1.Aux[L, FH, FT, H0, T0] = new IsHCons1[L, FH, FT] {
    override type H[Z] = H0[Z]
    override type T[Z] = T0[Z]

    override def pack[Z](u: (H[Z], T[Z])): L[Z] =
      (u._1 :: u._2).asInstanceOf[L[Z]]

    override def unpack[Z](p: L[Z]): (H[Z], T[Z]) = {
      val c = p.asInstanceOf[H[Z] :: T[Z]]
      (c.head, c.tail)
    }

    override def mkFhh: FH[H] = fhh
    override def mkFtt: FT[T] = ftt
  }
}

trait IsCCons1ScalaCompat extends IsCCons10 {

  implicit def mkIsCCons10[L[_], H[_], T[_] <: Coproduct, FH[_[_], _[_]], U[_], FT[_[_]]](
    using splitCons: SplitCons.Aux[L, H, T],
    fhh: => ApplyF[Apply10[FH, U], H],
    ftt: => ApplyF[FT, T],
  ): IsCCons1.Aux[L, Apply10[FH, U], FT, H, T] =
    IsCCons1.fromSum[L, H, T, Apply10[FH, U], FT]

  implicit def mkIsCCons11[L[_], H[_], T[_] <: Coproduct, FH[_[_], _[_]], U[_], FT[_[_]]](
    using splitCons: SplitCons.Aux[L, H, T],
    fhh: => ApplyF[Apply11[FH, U], H],
    ftt: => ApplyF[FT, T],
  ): IsCCons1.Aux[L, Apply11[FH, U], FT, H, T] =
    IsCCons1.fromSum[L, H, T, Apply11[FH, U], FT]

  implicit def mkIsCCons12[L[_], H[_], T[_] <: Coproduct, FH[_[_]], FT[_[_], _[_]], U[_]](
    using splitCons: SplitCons.Aux[L, H, T],
    fhh: => ApplyF[FH, H],
    ftt: => ApplyF[Apply10[FT, U], T],
  ): IsCCons1.Aux[L, FH, Apply10[FT, U], H, T] =
    IsCCons1.fromSum[L, H, T, FH, Apply10[FT, U]]

  implicit def mkIsCCons13[L[_], H[_], T[_] <: Coproduct, FH[_[_]], FT[_[_], _[_]], U[_]](
    using splitCons: SplitCons.Aux[L, H, T],
    fhh: => ApplyF[FH, H],
    ftt: => ApplyF[Apply11[FT, U], T],
  ): IsCCons1.Aux[L, FH, Apply11[FT, U], H, T] =
    IsCCons1.fromSum[L, H, T, FH, Apply11[FT, U]]
}

trait IsCCons10ScalaCompat {

  def apply[L[_], FH[_[_]], FT[_[_]]](using ev: IsCCons1[L, FH, FT]): IsCCons1.Aux[L, FH, FT, ev.H, ev.T] =
    ev

  given fromSum[L[_], H0[_], T0[_] <: Coproduct, FH[_[_]], FT[_[_]]](
    using splitCons: SplitCons.Aux[L, H0, T0],
    fhh: => ApplyF[FH, H0],
    ftt: => ApplyF[FT, T0],
  ): IsCCons1.Aux[L, FH, FT, H0, T0] = new IsCCons1[L, FH, FT] {
    override type H[Z] = H0[Z]
    override type T[Z] = T0[Z]

    override def pack[Z](u: Either[H[Z], T[Z]]): L[Z] = u match {
      case Left(h) => Inl(h).asInstanceOf[L[Z]]
      case Right(t) => Inr(t).asInstanceOf[L[Z]]
    }

    override def unpack[Z](p: L[Z]): Either[H[Z], T[Z]] = {
      val c = p.asInstanceOf[H[Z] :+: T[Z]]
      c match {
        case Inl(head) => Left(head)
        case Inr(tail) => Right(tail)
      }
    }

    override def mkFhh: FH[H] = fhh
    override def mkFtt: FT[T] = ftt
  }
}

trait Split1ScalaCompat extends Split10 {

  implicit def mkSplit10[L[_], FO[_[_], _[_]], U[_], FI[_[_]]]: Split1[L, Apply10[FO, U], FI] = ???

  implicit def mkSplit11[L[_], FO[_[_], _[_]], U[_], FI[_[_]]]: Split1[L, Apply11[FO, U], FI] = ???

  implicit def mkSplit12[L[_], FO[_[_]], FI[_[_], _[_]], U[_]]: Split1[L, FO, Apply10[FI, U]] = ???

  implicit def mkSplit13[L[_], FO[_[_]], FI[_[_], _[_]], U[_]]: Split1[L, FO, Apply11[FI, U]] = ???
}

trait Split10ScalaCompat {
  implicit def apply[L[_], FO[_[_]], FI[_[_]]]: Split1[L, FO, FI] = ???
}

trait SplitCons[L[_]] {
  type H[_]
  type T[_]
}
object SplitCons {
  type Aux[L[_], H0[_], T0[_]] = SplitCons[L] { type H[A] = H0[A]; type T[A] = T0[A] }

  /*
  given [H0[_], T0[_] <: HList]: SplitCons.Aux[[A] =>> H0[A] :: T0[A], H0, T0] = new SplitCons.Aux[[A] =>> H0[A] :: T0[A]] {
    type H[A] = H0[A]
    type T[A] = T0[A]
  }
  */

  /*
  import scala.quoted.*

  transparent inline given[L[_]]: SplitCons[L] = ${splitConsImpl[L]}

  private def splitConsImpl[L[_]: Type](using quotes: Quotes): Expr[SplitCons[L]] = {
    import quotes.reflect.*

    Type.of[L] match {
      case '[h :: t] =>
        '{
          new SplitCons[L] {
            type H[X] = h[X]
            type T[Y] = t[Y]
          }
        }
      case '[h :+: t] =>
        '{
          new SplitCons[L] {
            type H[X] = h[X]
            type T[Y] = t[Y]
          }
        }
      case _ =>
        report.throwError("Can't split higher kinded cons")
    }
  }
  */
}
