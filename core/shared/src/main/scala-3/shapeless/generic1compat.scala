/*
 * Copyright (c) 2015 Miles Sabin
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

import scala.quoted.*

/*
 * On Scala 3 the instances of `Generic1`, `IsHCons1`, `IsCCons1` and `Split1` are built in two steps: a macro computes
 * the representation (eg. `Generic1Repr`), then an ordinary implicit definition resolves the type class instances for
 * the components as `Lazy` implicits. This lets recursive derivations (eg. a `Functor` for `List` via `::`) tie the
 * knot, as the Scala 2 implementation does by leaving these implicits to the typer, and lets nested derivations (eg. a
 * `Functor` for `Option[List[_]]`) avoid being deemed divergent.
 */

trait Generic1ScalaCompat {
  implicit def apply[T[_], FR[_[_]]](implicit mk: Generic1Builder[T, FR]): Generic1.Aux[T, FR, mk.R] = mk.instance
}

trait IsHCons1ScalaCompat {
  implicit def apply[L[_], FH[_[_]], FT[_[_]]](implicit mk: IsHCons1Builder[L, FH, FT]): IsHCons1.Aux[L, FH, FT, mk.H, mk.T] =
    mk.instance
}

trait IsCCons1ScalaCompat {
  implicit def apply[L[_], FH[_[_]], FT[_[_]]](implicit mk: IsCCons1Builder[L, FH, FT]): IsCCons1.Aux[L, FH, FT, mk.H, mk.T] =
    mk.instance
}

trait Split1ScalaCompat {
  implicit def apply[L[_], FO[_[_]], FI[_[_]]](implicit mk: Split1Builder[L, FO, FI]): Split1.Aux[L, FO, FI, mk.O, mk.I] =
    mk.instance
}

/*
 * The builders take the components of the representation as type parameters, so that the type class instances for the
 * components are requested at types which don't depend on the representation value (eg. `Functor[List]` rather than
 * `Functor[repr.H]`). This lets the implicit search recognize recursive requests and tie the knot.
 */

/** Builds `Generic1` instances. An implementation detail of Scala 3 `Generic1`. */
trait Generic1Builder[T[_], FR[_[_]]] extends Serializable {
  type R[t]
  def instance: Generic1.Aux[T, FR, R]
}

object Generic1Builder {
  implicit def mk[T[_], FR[_[_]], R0[_]](using repr: Generic1Repr.Aux[T, R0])(using fr: Lazy[FR[R0]]): Generic1Builder[T, FR] { type R[t] = R0[t] } =
    new Generic1Builder[T, FR] {
      type R[t] = R0[t]
      def instance: Generic1.Aux[T, FR, R] = Generic1.unsafeInstance[T, FR, R0](repr.to, repr.from)(fr.value)
    }
}

/** Builds `IsHCons1` instances. An implementation detail of Scala 3 `IsHCons1`. */
trait IsHCons1Builder[L[_], FH[_[_]], FT[_[_]]] extends Serializable {
  type H[t]
  type T[t] <: HList
  def instance: IsHCons1.Aux[L, FH, FT, H, T]
}

object IsHCons1Builder {
  implicit def mk[L[_], FH[_[_]], FT[_[_]], H0[_], T0[_] <: HList](using repr: IsHCons1Repr.Aux[L, H0, T0])(
    using lazyFhh: Lazy[FH[H0]], lazyFtt: Lazy[FT[T0]]
  ): IsHCons1Builder[L, FH, FT] { type H[t] = H0[t]; type T[t] = T0[t] } =
    new IsHCons1Builder[L, FH, FT] {
      type H[t] = H0[t]
      type T[t] = T0[t]
      def instance: IsHCons1.Aux[L, FH, FT, H, T] =
        new IsHCons1[L, FH, FT] {
          type H[x] = H0[x]
          type T[x] = T0[x]
          def mkFhh: FH[H] = lazyFhh.value
          def mkFtt: FT[T] = lazyFtt.value
          def pack[A](u: (H[A], T[A])): L[A] = repr.pack(u.asInstanceOf[(H0[Any], T0[Any])]).asInstanceOf[L[A]]
          def unpack[A](p: L[A]): (H[A], T[A]) = repr.unpack(p.asInstanceOf[L[Any]]).asInstanceOf[(H[A], T[A])]
        }
    }
}

/** Builds `IsCCons1` instances. An implementation detail of Scala 3 `IsCCons1`. */
trait IsCCons1Builder[L[_], FH[_[_]], FT[_[_]]] extends Serializable {
  type H[t]
  type T[t] <: Coproduct
  def instance: IsCCons1.Aux[L, FH, FT, H, T]
}

object IsCCons1Builder {
  implicit def mk[L[_], FH[_[_]], FT[_[_]], H0[_], T0[_] <: Coproduct](using repr: IsCCons1Repr.Aux[L, H0, T0])(
    using lazyFhh: Lazy[FH[H0]], lazyFtt: Lazy[FT[T0]]
  ): IsCCons1Builder[L, FH, FT] { type H[t] = H0[t]; type T[t] = T0[t] } =
    new IsCCons1Builder[L, FH, FT] {
      type H[t] = H0[t]
      type T[t] = T0[t]
      def instance: IsCCons1.Aux[L, FH, FT, H, T] =
        new IsCCons1[L, FH, FT] {
          type H[x] = H0[x]
          type T[x] = T0[x]
          def mkFhh: FH[H] = lazyFhh.value
          def mkFtt: FT[T] = lazyFtt.value
          def pack[A](u: Either[H[A], T[A]]): L[A] =
            repr.pack(u.asInstanceOf[Either[H0[Any], T0[Any]]]).asInstanceOf[L[A]]
          def unpack[A](p: L[A]): Either[H[A], T[A]] =
            repr.unpack(p.asInstanceOf[L[Any]]).asInstanceOf[Either[H[A], T[A]]]
        }
    }
}

/** Builds `Split1` instances. An implementation detail of Scala 3 `Split1`. */
trait Split1Builder[L[_], FO[_[_]], FI[_[_]]] extends Serializable {
  type O[t]
  type I[t]
  def instance: Split1.Aux[L, FO, FI, O, I]
}

object Split1Builder {
  implicit def mk[L[_], FO[_[_]], FI[_[_]], O0[_], I0[_]](using repr: Split1Repr.Aux[L, O0, I0])(
    using fo: Lazy[FO[O0]], fi: Lazy[FI[I0]]
  ): Split1Builder[L, FO, FI] { type O[t] = O0[t]; type I[t] = I0[t] } =
    new Split1Builder[L, FO, FI] {
      type O[t] = O0[t]
      type I[t] = I0[t]
      def instance: Split1.Aux[L, FO, FI, O, I] =
        Split1.instance[FO, FI, O0, I0](fo.value, fi.value).asInstanceOf[Split1.Aux[L, FO, FI, O, I]]
    }
}

/** The representation of a type constructor for `Generic1`. An implementation detail of Scala 3 `Generic1`. */
trait Generic1Repr[F[_]] extends Serializable {
  type R[t]
  def to(ft: F[Any]): R[Any]
  def from(rt: R[Any]): F[Any]
}

// See `Materialized` for why each of these has two macros

object Generic1Repr extends Generic1Repr0 {
  type Aux[F[_], R0[_]] = Generic1Repr[F] { type R[t] = R0[t] }

  transparent inline implicit def materializeRepr[F[_]]: Materialized[Generic1Repr[F]] =
    ${ Generic1Macros.materializedGeneric1Repr[F] }

  def instance[F[_], R0[_]](f: F[Any] => R0[Any], g: R0[Any] => F[Any]): Aux[F, R0] =
    new Generic1Repr[F] {
      type R[t] = R0[t]
      def to(ft: F[Any]): R[Any] = f(ft)
      def from(rt: R[Any]): F[Any] = g(rt)
    }
}

trait Generic1Repr0 extends Generic1Repr1 {
  transparent inline implicit def materialize[F[_], R0[_]]: Generic1Repr.Aux[F, R0] =
    ${ Generic1Macros.mkGeneric1ReprAux[F, R0] }
}

trait Generic1Repr1 {
  implicit def materializeAux[F[_], R0[_]](
    implicit repr: Materialized[Generic1Repr[F]], ev: TypeConstructorEq[repr.value.R, R0]
  ): Generic1Repr.Aux[F, R0] =
    ev.substitute[[r[_]] =>> Generic1Repr.Aux[F, r]](repr.value)
}

/** Evidence that the type constructors `F` and `G` are the same: `=:=` for type constructors. */
sealed abstract class TypeConstructorEq[F[_], G[_]] extends Serializable {
  def substitute[C[_[_]]](c: C[F]): C[G]
}

object TypeConstructorEq {
  implicit def refl[F[_]]: TypeConstructorEq[F, F] =
    new TypeConstructorEq[F, F] {
      def substitute[C[_[_]]](c: C[F]): C[F] = c
    }
}

/** The decomposition `L[t] = H[t] :: T[t]`. An implementation detail of Scala 3 `IsHCons1`. */
trait IsHCons1Repr[L[_]] extends Serializable {
  type H[t]
  type T[t] <: HList
  def pack(u: (H[Any], T[Any])): L[Any]
  def unpack(p: L[Any]): (H[Any], T[Any])
}

object IsHCons1Repr extends IsHCons1Repr0 {
  type Aux[L[_], H0[_], T0[_] <: HList] = IsHCons1Repr[L] { type H[t] = H0[t]; type T[t] = T0[t] }

  transparent inline implicit def materializeRepr[L[_]]: Materialized[IsHCons1Repr[L]] =
    ${ Generic1Macros.materializedIsHCons1Repr[L] }

  def instance[L[_], H0[_], T0[_] <: HList]: Aux[L, H0, T0] =
    new IsHCons1Repr[L] {
      type H[t] = H0[t]
      type T[t] = T0[t]
      def pack(u: (H[Any], T[Any])): L[Any] = (u._1 :: u._2).asInstanceOf[L[Any]]
      def unpack(p: L[Any]): (H[Any], T[Any]) = {
        val c = p.asInstanceOf[H[Any] :: T[Any]]
        (c.head, c.tail)
      }
    }
}

/** The decomposition `L[t] = H[t] :+: T[t]`. An implementation detail of Scala 3 `IsCCons1`. */
trait IsCCons1Repr[L[_]] extends Serializable {
  type H[t]
  type T[t] <: Coproduct
  def pack(u: Either[H[Any], T[Any]]): L[Any]
  def unpack(p: L[Any]): Either[H[Any], T[Any]]
}

object IsCCons1Repr extends IsCCons1Repr0 {
  type Aux[L[_], H0[_], T0[_] <: Coproduct] = IsCCons1Repr[L] { type H[t] = H0[t]; type T[t] = T0[t] }

  transparent inline implicit def materializeRepr[L[_]]: Materialized[IsCCons1Repr[L]] =
    ${ Generic1Macros.materializedIsCCons1Repr[L] }

  def instance[L[_], H0[_], T0[_] <: Coproduct]: Aux[L, H0, T0] =
    new IsCCons1Repr[L] {
      type H[t] = H0[t]
      type T[t] = T0[t]
      def pack(u: Either[H[Any], T[Any]]): L[Any] = (u match {
        case Left(hd) => Inl[H[Any], T[Any]](hd)
        case Right(tl) => Inr[H[Any], T[Any]](tl)
      }).asInstanceOf[L[Any]]
      def unpack(p: L[Any]): Either[H[Any], T[Any]] = p.asInstanceOf[H[Any] :+: T[Any]] match {
        case Inl(hd) => Left(hd)
        case Inr(tl) => Right(tl)
      }
    }
}

/** The decomposition `L[t] = O[I[t]]`. An implementation detail of Scala 3 `Split1`. */
trait Split1Repr[L[_]] extends Serializable {
  type O[t]
  type I[t]
}

object Split1Repr extends Split1Repr0 {
  type Aux[L[_], O0[_], I0[_]] = Split1Repr[L] { type O[t] = O0[t]; type I[t] = I0[t] }

  transparent inline implicit def materializeRepr[L[_]]: Materialized[Split1Repr[L]] =
    ${ Generic1Macros.materializedSplit1Repr[L] }

  def instance[L[_], O0[_], I0[_]]: Aux[L, O0, I0] =
    new Split1Repr[L] {
      type O[t] = O0[t]
      type I[t] = I0[t]
    }
}

trait IsHCons1Repr0 extends IsHCons1Repr1 {
  transparent inline implicit def materialize[L[_], H0[_], T0[_] <: HList]: IsHCons1Repr.Aux[L, H0, T0] =
    ${ Generic1Macros.mkIsHCons1ReprAux[L, H0, T0] }
}

trait IsHCons1Repr1 {
  implicit def materializeAux[L[_], H0[_], T0[_] <: HList](
    implicit repr: Materialized[IsHCons1Repr[L]], evH: TypeConstructorEq[repr.value.H, H0],
    evT: TypeConstructorEq[repr.value.T, T0]
  ): IsHCons1Repr.Aux[L, H0, T0] =
    evT.substitute[[t[_]] =>> IsHCons1Repr[L] { type H[x] = H0[x]; type T[x] = t[x] }](
      evH.substitute[[h[_]] =>> IsHCons1Repr[L] { type H[x] = h[x]; type T[x] = repr.value.T[x] }](repr.value)
    )
}

trait IsCCons1Repr0 extends IsCCons1Repr1 {
  transparent inline implicit def materialize[L[_], H0[_], T0[_] <: Coproduct]: IsCCons1Repr.Aux[L, H0, T0] =
    ${ Generic1Macros.mkIsCCons1ReprAux[L, H0, T0] }
}

trait IsCCons1Repr1 {
  implicit def materializeAux[L[_], H0[_], T0[_] <: Coproduct](
    implicit repr: Materialized[IsCCons1Repr[L]], evH: TypeConstructorEq[repr.value.H, H0],
    evT: TypeConstructorEq[repr.value.T, T0]
  ): IsCCons1Repr.Aux[L, H0, T0] =
    evT.substitute[[t[_]] =>> IsCCons1Repr[L] { type H[x] = H0[x]; type T[x] = t[x] }](
      evH.substitute[[h[_]] =>> IsCCons1Repr[L] { type H[x] = h[x]; type T[x] = repr.value.T[x] }](repr.value)
    )
}

trait Split1Repr0 extends Split1Repr1 {
  transparent inline implicit def materialize[L[_], O0[_], I0[_]]: Split1Repr.Aux[L, O0, I0] =
    ${ Generic1Macros.mkSplit1ReprAux[L, O0, I0] }
}

trait Split1Repr1 {
  implicit def materializeAux[L[_], O0[_], I0[_]](
    implicit repr: Materialized[Split1Repr[L]], evO: TypeConstructorEq[repr.value.O, O0],
    evI: TypeConstructorEq[repr.value.I, I0]
  ): Split1Repr.Aux[L, O0, I0] =
    evI.substitute[[i[_]] =>> Split1Repr[L] { type O[x] = O0[x]; type I[x] = i[x] }](
      evO.substitute[[o[_]] =>> Split1Repr[L] { type O[x] = o[x]; type I[x] = repr.value.I[x] }](repr.value)
    )
}

object Generic1Macros {
  def materializedGeneric1Repr[F[_]: Type](using Quotes): Expr[Materialized[Generic1Repr[F]]] =
    MacroUtils.materialized(mkGeneric1Repr[F])

  def materializedIsHCons1Repr[L[_]: Type](using Quotes): Expr[Materialized[IsHCons1Repr[L]]] =
    MacroUtils.materialized(mkIsHCons1Repr[L])

  def materializedIsCCons1Repr[L[_]: Type](using Quotes): Expr[Materialized[IsCCons1Repr[L]]] =
    MacroUtils.materialized(mkIsCCons1Repr[L])

  def materializedSplit1Repr[L[_]: Type](using Quotes): Expr[Materialized[Split1Repr[L]]] =
    MacroUtils.materialized(mkSplit1Repr[L])

  def mkGeneric1ReprAux[F[_]: Type, R0[_]: Type](using Quotes): Expr[Generic1Repr.Aux[F, R0]] =
    mkGeneric1Repr[F].asInstanceOf[Expr[Generic1Repr.Aux[F, R0]]]

  def mkIsHCons1ReprAux[L[_]: Type, H0[_]: Type, T0[_] <: HList: Type](using Quotes): Expr[IsHCons1Repr.Aux[L, H0, T0]] =
    mkIsHCons1Repr[L].asInstanceOf[Expr[IsHCons1Repr.Aux[L, H0, T0]]]

  def mkIsCCons1ReprAux[L[_]: Type, H0[_]: Type, T0[_] <: Coproduct: Type](using Quotes): Expr[IsCCons1Repr.Aux[L, H0, T0]] =
    mkIsCCons1Repr[L].asInstanceOf[Expr[IsCCons1Repr.Aux[L, H0, T0]]]

  def mkSplit1ReprAux[L[_]: Type, O0[_]: Type, I0[_]: Type](using Quotes): Expr[Split1Repr.Aux[L, O0, I0]] =
    mkSplit1Repr[L].asInstanceOf[Expr[Split1Repr.Aux[L, O0, I0]]]

  def mkGeneric1Repr[T[_]: Type](using Quotes): Expr[Generic1Repr[T]] = {
    import quotes.reflect.*
    val tTpe = TypeRepr.of[T]
    val anyT = tTpe.appliedTo(TypeRepr.of[Any]).dealias

    if (GenericMacros.isReprType(anyT)) report.errorAndAbort("No Generic1 instance available for HList or Coproduct")

    val reprLambda = lambda1(p => GenericMacros.reprOf(tTpe.appliedTo(p).dealias))
    val anyRepr = GenericMacros.reprOf(anyT)

    val (to, from) = GenericMacros.productInfo(anyT) match {
      case Some((fields, deconstruct, construct)) =>
        (
          MacroUtils.lambda(anyT, anyRepr)(t => MacroUtils.mkHList(deconstruct(t))),
          MacroUtils.lambda(anyRepr, anyT)(r => construct(GenericMacros.hlistElems(r, fields.size)))
        )
      case None =>
        val ctors = GenericMacros.ctorsOf(anyT)
        val to = MacroUtils.lambda(anyT, anyRepr) { ft =>
          val index = ctors.zipWithIndex.foldRight('{ -1 }: Expr[Int]) { case ((ctor, i), acc) =>
            val test = GenericMacros.instanceTest(ft, ctor)
            '{ if ($test) ${ Expr(i) } else $acc }
          }
          anyRepr.asType match {
            case '[r] => '{ Coproduct.unsafeMkCoproduct($index, ${ ft.asExpr }).asInstanceOf[r] }.asTerm
          }
        }
        val from = MacroUtils.lambda(anyRepr, anyT) { rt =>
          anyT.asType match {
            case '[t] => '{ Coproduct.unsafeGet(${ rt.asExpr }.asInstanceOf[Coproduct]).asInstanceOf[t] }.asTerm
          }
        }
        (to, from)
    }

    Select.unique(Ref(TypeRepr.of[Generic1Repr.type].termSymbol), "instance")
      .appliedToTypes(List(tTpe, reprLambda))
      .appliedTo(to, from)
      .asExprOf[Generic1Repr[T]]
  }

  def mkIsHCons1Repr[L[_]: Type](using Quotes): Expr[IsHCons1Repr[L]] = {
    import quotes.reflect.*
    val (hd, tl) = splitCons(TypeRepr.of[L], TypeRepr.of[::], "Not H/CCons")
    Select.unique(Ref(TypeRepr.of[IsHCons1Repr.type].termSymbol), "instance")
      .appliedToTypes(List(TypeRepr.of[L], hd, tl))
      .asExprOf[IsHCons1Repr[L]]
  }

  def mkIsCCons1Repr[L[_]: Type](using Quotes): Expr[IsCCons1Repr[L]] = {
    import quotes.reflect.*
    val (hd, tl) = splitCons(TypeRepr.of[L], TypeRepr.of[:+:], "Not H/CCons")
    Select.unique(Ref(TypeRepr.of[IsCCons1Repr.type].termSymbol), "instance")
      .appliedToTypes(List(TypeRepr.of[L], hd, tl))
      .asExprOf[IsCCons1Repr[L]]
  }

  def mkSplit1Repr[L[_]: Type](using Quotes): Expr[Split1Repr[L]] = {
    import quotes.reflect.*
    val lTpe = TypeRepr.of[L]
    if (GenericMacros.isReprType(lTpe.appliedTo(TypeRepr.of[Any]).dealias))
      report.errorAndAbort("No Split1 instance available for HList or Coproduct")

    // Splits L[p] = TC[.., I[p], ..] into an outer TC[.., *, ..] and an inner I, given a parameter p.
    def split(p: TypeRepr): Option[(TypeRepr, List[TypeRepr], TypeRepr)] =
      lTpe.appliedTo(p).dealias match {
        case AppliedType(tycon, args) =>
          args.find(a => contains(a, p)).filter { pivot =>
            !(pivot =:= p) && args.forall(a => a =:= pivot || !contains(a, p))
          }.map(pivot => (tycon, args, pivot))
        case _ => None
      }

    if (split(TypeRepr.of[Generic1Macros.Probe]).isEmpty)
      report.errorAndAbort(s"Can't split ${lTpe.show} into a non-trivial outer and inner type constructor")

    def parts(p: TypeRepr): (TypeRepr, List[TypeRepr], TypeRepr) =
      split(p).getOrElse(report.errorAndAbort(s"Can't split ${lTpe.show}"))
    val outer = lambda1 { t =>
      val (tycon, args, pivot) = parts(TypeRepr.of[Generic1Macros.Probe])
      tycon.appliedTo(args.map(a => if (a =:= pivot) t else a))
    }
    val inner = lambda1(t => parts(t)._3)

    Select.unique(Ref(TypeRepr.of[Split1Repr.type].termSymbol), "instance")
      .appliedToTypes(List(lTpe, outer, inner))
      .asExprOf[Split1Repr[L]]
  }

  /** An abstract type used to probe the shape of a type constructor. */
  type Probe

  /** Decomposes `L[p]` as `cons[H[p], T[p]]`, yielding the lambdas `H` and `T`. */
  def splitCons(using Quotes)(
    lTpe: quotes.reflect.TypeRepr, cons: quotes.reflect.TypeRepr, msg: String
  ): (quotes.reflect.TypeRepr, quotes.reflect.TypeRepr) = {
    import quotes.reflect.*
    def parts(p: TypeRepr): (TypeRepr, TypeRepr) = lTpe.appliedTo(p).dealias match {
      case AppliedType(tycon, List(h, t)) if tycon =:= cons => (h, t)
      case _ => report.errorAndAbort(msg)
    }
    parts(TypeRepr.of[Any])
    (lambda1(p => parts(p)._1), lambda1(p => parts(p)._2))
  }

  /** A unary type lambda `[t] =>> body(t)`. */
  def lambda1(using Quotes)(body: quotes.reflect.TypeRepr => quotes.reflect.TypeRepr): quotes.reflect.TypeRepr = {
    import quotes.reflect.*
    TypeLambda(List("t"), _ => List(TypeBounds.empty), tl => body(tl.param(0)))
  }

  def contains(using Quotes)(tpe: quotes.reflect.TypeRepr, p: quotes.reflect.TypeRepr): Boolean = {
    import quotes.reflect.*
    tpe =:= p || (tpe match {
      case AppliedType(tycon, args) => contains(tycon, p) || args.exists(contains(_, p))
      case AndType(l, r) => contains(l, p) || contains(r, p)
      case OrType(l, r) => contains(l, p) || contains(r, p)
      case TypeBounds(lo, hi) => contains(lo, p) || contains(hi, p)
      case _ => false
    })
  }
}
