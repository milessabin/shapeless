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

trait Generic1ScalaCompat {

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

trait Generic10ScalaCompat {

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

type Head[T <: scala.Tuple] = T match {
  case h *: _ => h
}

type Tail[T <: scala.Tuple] <: scala.Tuple = T match {
  case _ *: t => t
}

type HeadElemTypes[ElemTypes[_] <: scala.Tuple] = [Z] =>> Head[ElemTypes[Z]]

type TailElemTypesHCons[ElemTypes[_] <: scala.Tuple] = [Z] =>> HList.TupleToHList[Tail[ElemTypes[Z]]]

trait IsHCons1ScalaCompat {

  given mkIsHCons10[L[_] <: Product, FH[_[_], _[_]], U[_], FT[_[_]]](
    using m: MirrorOf1Product[L],
    fhh: => ApplyF[Apply10[FH, U], HeadElemTypes[m.MirroredElemTypes]],
    ftt: => ApplyF[FT, TailElemTypesHCons[m.MirroredElemTypes]],
  ): IsHCons1.Aux[L, Apply10[FH, U], FT, HeadElemTypes[m.MirroredElemTypes], TailElemTypesHCons[m.MirroredElemTypes]] =
    IsHCons1.fromProduct[L, Apply10[FH, U], FT]

  given mkIsHCons11[L[_] <: Product, FH[_[_], _[_]], U[_], FT[_[_]]](
    using m: MirrorOf1Product[L],
    fhh: => ApplyF[Apply11[FH, U], HeadElemTypes[m.MirroredElemTypes]],
    ftt: => ApplyF[FT, TailElemTypesHCons[m.MirroredElemTypes]],
  ): IsHCons1.Aux[L, Apply11[FH, U], FT, HeadElemTypes[m.MirroredElemTypes], TailElemTypesHCons[m.MirroredElemTypes]] =
    IsHCons1.fromProduct[L, Apply11[FH, U], FT]

  given mkIsHCons12[L[_] <: Product, FH[_[_]], FT[_[_], _[_]], U[_]](
    using m: MirrorOf1Product[L],
    fhh: => ApplyF[FH, HeadElemTypes[m.MirroredElemTypes]],
    ftt: => ApplyF[Apply10[FT, U], TailElemTypesHCons[m.MirroredElemTypes]],
  ): IsHCons1.Aux[L, FH, Apply10[FT, U], HeadElemTypes[m.MirroredElemTypes], TailElemTypesHCons[m.MirroredElemTypes]] =
    IsHCons1.fromProduct[L, FH, Apply10[FT, U]]

  given mkIsHCons13[L[_] <: Product, FH[_[_]], FT[_[_], _[_]], U[_]](
    using m: MirrorOf1Product[L],
    fhh: => ApplyF[FH, HeadElemTypes[m.MirroredElemTypes]],
    ftt: => ApplyF[Apply11[FT, U], TailElemTypesHCons[m.MirroredElemTypes]],
  ): IsHCons1.Aux[L, FH, Apply11[FT, U], HeadElemTypes[m.MirroredElemTypes], TailElemTypesHCons[m.MirroredElemTypes]] =
    IsHCons1.fromProduct[L, FH, Apply11[FT, U]]
}

trait IsHCons10ScalaCompat {
  given fromProduct[L[_] <: Product, FH[_[_]], FT[_[_]]](
    using m: MirrorOf1Product[L],
    fhh: => ApplyF[FH, HeadElemTypes[m.MirroredElemTypes]],
    ftt: => ApplyF[FT, TailElemTypesHCons[m.MirroredElemTypes]],
  ): IsHCons1.Aux[L, FH, FT, HeadElemTypes[m.MirroredElemTypes], TailElemTypesHCons[m.MirroredElemTypes]] = new IsHCons1[L, FH, FT] {
    override type H[Z] = Head[m.MirroredElemTypes[Z]]
    override type T[Z] = HList.TupleToHList[Tail[m.MirroredElemTypes[Z]]]

    override def pack[A](u: (H[A], T[A])): L[A] = {
      val t = u._1 *: HList.hListToTuple(u._2)
      m.fromProduct(t).asInstanceOf[L[A]]
    }

    override def unpack[Z](p: L[Z]): (H[Z], T[Z]) =
      //This is safe as the ftt won't be typed correctly if L produces an empty tuple
      (scala.Tuple.fromProduct(p): @unchecked) match {
        case h *: t => (h.asInstanceOf[H[Z]], HList.tupleToHList(t).asInstanceOf[T[Z]])
      }

    override def mkFhh: FH[H] = fhh
    override def mkFtt: FT[T] = ftt
  }
}

type TailElemTypesCCons[ElemTypes[_] <: scala.Tuple] = [Z] =>> Coproduct.TupleToCoproduct[Tail[ElemTypes[Z]]]

trait IsCCons1ScalaCompat {

  implicit def mkIsCCons10[L[_], FH[_[_], _[_]], U[_], FT[_[_]]](
    using m: MirrorOf1Sum[L],
    fhh: => ApplyF[Apply10[FH, U], HeadElemTypes[m.MirroredElemTypes]],
    ftt: => ApplyF[FT, TailElemTypesCCons[m.MirroredElemTypes]],
  ): IsCCons1.Aux[L, Apply10[FH, U], FT, HeadElemTypes[m.MirroredElemTypes], TailElemTypesCCons[m.MirroredElemTypes]] =
    IsCCons1.fromSum[L, Apply10[FH, U], FT]

  implicit def mkIsCCons11[L[_], FH[_[_], _[_]], U[_], FT[_[_]]](
    using m: MirrorOf1Sum[L],
    fhh: => ApplyF[Apply11[FH, U], HeadElemTypes[m.MirroredElemTypes]],
    ftt: => ApplyF[FT, TailElemTypesCCons[m.MirroredElemTypes]],
  ): IsCCons1.Aux[L, Apply11[FH, U], FT, HeadElemTypes[m.MirroredElemTypes], TailElemTypesCCons[m.MirroredElemTypes]] =
    IsCCons1.fromSum[L, Apply11[FH, U], FT]

  implicit def mkIsCCons12[L[_], FH[_[_]], FT[_[_], _[_]], U[_]](
    using m: MirrorOf1Sum[L],
    fhh: => ApplyF[FH, HeadElemTypes[m.MirroredElemTypes]],
    ftt: => ApplyF[Apply10[FT, U], TailElemTypesCCons[m.MirroredElemTypes]],
  ): IsCCons1.Aux[L, FH, Apply10[FT, U], HeadElemTypes[m.MirroredElemTypes], TailElemTypesCCons[m.MirroredElemTypes]] =
    IsCCons1.fromSum[L, FH, Apply10[FT, U]]

  implicit def mkIsCCons13[L[_], FH[_[_]], FT[_[_], _[_]], U[_]](
    using m: MirrorOf1Sum[L],
    fhh: => ApplyF[FH, HeadElemTypes[m.MirroredElemTypes]],
    ftt: => ApplyF[Apply11[FT, U], TailElemTypesCCons[m.MirroredElemTypes]],
  ): IsCCons1.Aux[L, FH, Apply11[FT, U], HeadElemTypes[m.MirroredElemTypes], TailElemTypesCCons[m.MirroredElemTypes]] =
    IsCCons1.fromSum[L, FH, Apply11[FT, U]]
}

trait IsCCons10ScalaCompat {
  given fromSum[L[_], FH[_[_]], FT[_[_]]](
    using m: MirrorOf1Sum[L],
    fhh: => ApplyF[FH, HeadElemTypes[m.MirroredElemTypes]],
    ftt: => ApplyF[FT, TailElemTypesCCons[m.MirroredElemTypes]],
  ): IsCCons1.Aux[L, FH, FT, HeadElemTypes[m.MirroredElemTypes], TailElemTypesCCons[m.MirroredElemTypes]] = new IsCCons1[L, FH, FT] {
    override type H[Z] = Head[m.MirroredElemTypes[Z]]
    override type T[Z] = Coproduct.TupleToCoproduct[Tail[m.MirroredElemTypes[Z]]]

    override def pack[Z](u: Either[H[Z], T[Z]]): L[Z] = u match {
      case Left(hz) => hz.asInstanceOf[L[Z]]
      case Right(tz) => Coproduct.extractCoproduct(tz).asInstanceOf[L[Z]]
    }

    override def unpack[Z](p: L[Z]): Either[H[Z], T[Z]] = {
      //TODO: Make sure this is correct
      val ordinal = m.ordinal(p.asInstanceOf[m.MirroredMonoType])
      if ordinal == 0 then Left(p.asInstanceOf[H[Z]])
      else Right(Coproduct.coproductFromOrdinal(p.asInstanceOf[scala.Tuple.Union[m.MirroredElemTypes[Z]]], ordinal - 1).asInstanceOf[T[Z]])
    }

    override def mkFhh: FH[H] = fhh
    override def mkFtt: FT[T] = ftt
  }
}

trait Split1ScalaCompat {

  implicit def mkSplit10[L[_], FO[_[_], _[_]], U[_], FI[_[_]]]: Split1[L, Apply10[FO, U], FI] = ???

  implicit def mkSplit11[L[_], FO[_[_], _[_]], U[_], FI[_[_]]]: Split1[L, Apply11[FO, U], FI] = ???

  implicit def mkSplit12[L[_], FO[_[_]], FI[_[_], _[_]], U[_]]: Split1[L, FO, Apply10[FI, U]] = ???

  implicit def mkSplit13[L[_], FO[_[_]], FI[_[_], _[_]], U[_]]: Split1[L, FO, Apply11[FI, U]] = ???
}

trait Split10ScalaCompat {
  implicit def apply[L[_], FO[_[_]], FI[_[_]]]: Split1[L, FO, FI] = ???
}
