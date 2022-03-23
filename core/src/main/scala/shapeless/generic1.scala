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

import scala.language.experimental.macros

import scala.reflect.macros.whitebox

trait Generic1[F[_], FR[_[_]]] extends Serializable {
  type R[t]

  lazy val fr: FR[R] = mkFrr

  def to[T](ft: F[T]): R[T]
  def from[T](rt: R[T]): F[T]

  def mkFrr: FR[R]
}

object Generic1 extends Generic10 {
  type Aux[F[_], FR[_[_]], R0[_]] = Generic1[F, FR] { type R[t] = R0[t] }

  implicit def mkGeneric10[T[_], U[_], FR[_[_], _[_]]]: Generic1[T, ({ type λ[t[_]] = FR[t, U] })#λ] =
    macro Generic1Macros.mkGeneric1Impl[T, ({ type λ[t[_]] = FR[t, U] })#λ]

  implicit def mkGeneric11[T[_], U[_], FR[_[_], _[_]]]: Generic1[T, ({ type λ[t[_]] = FR[U, t] })#λ] =
    macro Generic1Macros.mkGeneric1Impl[T, ({ type λ[t[_]] = FR[U, t] })#λ]

  def unsafeInstance[F[_], FR[_[_]], R0[_]](f: F[Any] => R0[Any], g: R0[Any] => F[Any])(implicit lazyFr: Lazy[FR[R0]]): Aux[F, FR, R0] = {
    new Generic1[F, FR] {
      type R[t] = R0[t]
      def mkFrr: FR[R] = lazyFr.value
      def to[T](ft: F[T]): R[T] = f(ft.asInstanceOf[F[Any]]).asInstanceOf[R[T]]
      def from[T](rt: R[T]): F[T] = g(rt.asInstanceOf[R[Any]]).asInstanceOf[F[T]]
    }
  }
}

trait Generic10 {
  implicit def apply[T[_], FR[_[_]]]: Generic1[T, FR] = macro Generic1Macros.mkGeneric1Impl[T, FR]
}

trait IsHCons1[L[_], FH[_[_]], FT[_[_]]] extends Serializable {
  type H[_]
  type T[_] <: HList

  lazy val fh: FH[H] = mkFhh
  lazy val ft: FT[T] = mkFtt

  def pack[A](u: (H[A], T[A])): L[A]
  def unpack[A](p: L[A]): (H[A], T[A])

  def mkFhh: FH[H]
  def mkFtt: FT[T]
}

object IsHCons1 extends IsHCons10 {
  type Aux[L[_], FH[_[_]], FT[_[_]], H0[_], T0[_] <: HList] = IsHCons1[L, FH, FT] { type H[t] = H0[t] ; type T[t] = T0[t] }

  implicit def mkIsHCons10[L[_], FH[_[_], _[_]], U[_], FT[_[_]]]: IsHCons1[L, ({ type λ[t[_]] = FH[t, U] })#λ, FT] =
    macro IsHCons1Macros.mkIsHCons1Impl[L, ({ type λ[t[_]] = FH[t, U] })#λ, FT]

  implicit def mkIsHCons11[L[_], FH[_[_], _[_]], U[_], FT[_[_]]]: IsHCons1[L, ({ type λ[t[_]] = FH[U, t] })#λ, FT] =
    macro IsHCons1Macros.mkIsHCons1Impl[L, ({ type λ[t[_]] = FH[U, t] })#λ, FT]

  implicit def mkIsHCons12[L[_], FH[_[_]], FT[_[_], _[_]], U[_]]: IsHCons1[L, FH, ({ type λ[t[_]] = FT[t, U] })#λ] =
    macro IsHCons1Macros.mkIsHCons1Impl[L, FH, ({ type λ[t[_]] = FT[t, U] })#λ]

  implicit def mkIsHCons13[L[_], FH[_[_]], FT[_[_], _[_]], U[_]]: IsHCons1[L, FH, ({ type λ[t[_]] = FT[U, t] })#λ] =
    macro IsHCons1Macros.mkIsHCons1Impl[L, FH, ({ type λ[t[_]] = FT[U, t] })#λ]

  def unsafeInstance[L[_] <: HList, FH[_[_]], FT[_[_]], H0[_], T0[_] <: HList](
    f: (H0[Any], T0[Any]) => L[Any],
    g: L[Any] => (H0[Any], T0[Any])
  )(implicit lazyFhh: Lazy[FH[H0]], lazyFtt: Lazy[FT[T0]]): Aux[L, FH, FT, H0, T0] =
    new IsHCons1[L, FH, FT] {
      type H[x] = H0[x]
      type T[x] = T0[x]
      def mkFhh: FH[H] = lazyFhh.value
      def mkFtt: FT[T] = lazyFtt.value
      def pack[A](u: (H[A], T[A])): L[A] =
        f(u._1.asInstanceOf[H[Any]], u._2.asInstanceOf[T[Any]]).asInstanceOf[L[A]]
      def unpack[A](p: L[A]): (H[A], T[A]) =
        g(p.asInstanceOf[L[Any]]).asInstanceOf[(H[A], T[A])]
    }
}

trait IsHCons10 {
  implicit def apply[L[_], FH[_[_]], FT[_[_]]]: IsHCons1[L, FH, FT] = macro IsHCons1Macros.mkIsHCons1Impl[L, FH, FT]
}

trait IsCCons1[L[_], FH[_[_]], FT[_[_]]] extends Serializable {
  type H[_]
  type T[_] <: Coproduct

  lazy val fh: FH[H] = mkFhh
  lazy val ft: FT[T] = mkFtt

  def pack[A](u: Either[H[A], T[A]]): L[A]
  def unpack[A](p: L[A]): Either[H[A], T[A]]

  def mkFhh: FH[H]
  def mkFtt: FT[T]
}

object IsCCons1 extends IsCCons10 {
  type Aux[L[_], FH[_[_]], FT[_[_]], H0[_], T0[_] <: Coproduct] = IsCCons1[L, FH, FT] { type H[t] = H0[t] ; type T[t] = T0[t] }

  implicit def mkIsCCons10[L[_], FH[_[_], _[_]], U[_], FT[_[_]]]: IsCCons1[L, ({ type λ[t[_]] = FH[t, U] })#λ, FT] =
    macro IsCCons1Macros.mkIsCCons1Impl[L, ({ type λ[t[_]] = FH[t, U] })#λ, FT]

  implicit def mkIsCCons11[L[_], FH[_[_], _[_]], U[_], FT[_[_]]]: IsCCons1[L, ({ type λ[t[_]] = FH[U, t] })#λ, FT] =
    macro IsCCons1Macros.mkIsCCons1Impl[L, ({ type λ[t[_]] = FH[U, t] })#λ, FT]

  implicit def mkIsCCons12[L[_], FH[_[_]], FT[_[_], _[_]], U[_]]: IsCCons1[L, FH, ({ type λ[t[_]] = FT[t, U] })#λ] =
    macro IsCCons1Macros.mkIsCCons1Impl[L, FH, ({ type λ[t[_]] = FT[t, U] })#λ]

  implicit def mkIsCCons13[L[_], FH[_[_]], FT[_[_], _[_]], U[_]]: IsCCons1[L, FH, ({ type λ[t[_]] = FT[U, t] })#λ] =
    macro IsCCons1Macros.mkIsCCons1Impl[L, FH, ({ type λ[t[_]] = FT[U, t] })#λ]

  def unsafeInstance[L[_] <: Coproduct, FH[_[_]], FT[_[_]], H0[_], T0[_] <: Coproduct](
    f: Either[H0[Any], T0[Any]] => L[Any],
    g: L[Any] => Either[H0[Any], T0[Any]]
  )(implicit lazyFhh: Lazy[FH[H0]], lazyFtt: Lazy[FT[T0]]): Aux[L, FH, FT, H0, T0] =
    new IsCCons1[L, FH, FT] {
      type H[x] = H0[x]
      type T[x] = T0[x]
      def mkFhh: FH[H] = lazyFhh.value
      def mkFtt: FT[T] = lazyFtt.value
      def pack[A](u: Either[H[A], T[A]]): L[A] =
        f(u.asInstanceOf[Either[H[Any], T[Any]]]).asInstanceOf[L[A]]
      def unpack[A](p: L[A]): Either[H[A], T[A]] =
        g(p.asInstanceOf[L[Any]]).asInstanceOf[Either[H[A], T[A]]]
    }
}

trait IsCCons10 {
  implicit def apply[L[_], FH[_[_]], FT[_[_]]]: IsCCons1[L, FH, FT] = macro IsCCons1Macros.mkIsCCons1Impl[L, FH, FT]
}

trait Split1[L[_], FO[_[_]], FI[_[_]]] extends Serializable {
  type O[_]
  type I[_]

  lazy val fo: FO[O] = mkFoo
  lazy val fi: FI[I] = mkFii

  def pack[T](u: O[I[T]]): L[T]
  def unpack[T](p: L[T]): O[I[T]]

  def mkFoo: FO[O]
  def mkFii: FI[I]
}

object Split1 extends Split10 {
  type Aux[L[_], FO[_[_]], FI[_[_]], O0[_], I0[_]] = Split1[L, FO, FI] { type O[T] = O0[T] ; type I[T] = I0[T] }

  implicit def mkSplit10[L[_], FO[_[_], _[_]], U[_], FI[_[_]]]: Split1[L, ({ type λ[t[_]] = FO[t, U] })#λ, FI] =
    macro Split1Macros.mkSplit1Impl[L, ({ type λ[t[_]] = FO[t, U] })#λ, FI]

  implicit def mkSplit11[L[_], FO[_[_], _[_]], U[_], FI[_[_]]]: Split1[L, ({ type λ[t[_]] = FO[U, t] })#λ, FI] =
    macro Split1Macros.mkSplit1Impl[L, ({ type λ[t[_]] = FO[U, t] })#λ, FI]

  implicit def mkSplit12[L[_], FO[_[_]], FI[_[_], _[_]], U[_]]: Split1[L, FO, ({ type λ[t[_]] = FI[t, U] })#λ] =
    macro Split1Macros.mkSplit1Impl[L, FO, ({ type λ[t[_]] = FI[t, U] })#λ]

  implicit def mkSplit13[L[_], FO[_[_]], FI[_[_], _[_]], U[_]]: Split1[L, FO, ({ type λ[t[_]] = FI[U, t] })#λ] =
    macro Split1Macros.mkSplit1Impl[L, FO, ({ type λ[t[_]] = FI[U, t] })#λ]

  def instance[FO[_[_]], FI[_[_]], O0[_], I0[_]](implicit lazyFoo: Lazy[FO[O0]], lazyFii: Lazy[FI[I0]]): Aux[({ type λ[x] = O0[I0[x]] })#λ, FO, FI, O0, I0] =
    new Split1[({ type λ[x] = O0[I0[x]] })#λ, FO, FI] {
      type O[x] = O0[x]
      type I[x] = I0[x]
      def mkFoo: FO[O] = lazyFoo.value
      def mkFii: FI[I] = lazyFii.value
      def pack[T](u: O[I[T]]): O[I[T]] = u
      def unpack[T](p: O[I[T]]): O[I[T]] = p
    }
}

trait Split10 {
  implicit def apply[L[_], FO[_[_]], FI[_[_]]]: Split1[L, FO, FI] = macro Split1Macros.mkSplit1Impl[L, FO, FI]
}

class Generic1Macros(val c: whitebox.Context) extends CaseClassMacros {
  import c.ImplicitCandidate
  import c.universe._
  import definitions._

  private val generic1 = objectRef[Generic1.type]
  private val unchecked = typeOf[unchecked]

  def mkGeneric1Impl[T[_], FR[_[_]]](implicit tTag: WeakTypeTag[T[_]], frTag: WeakTypeTag[FR[Any]]): Tree = {
    val tpe = tTag.tpe.etaExpand
    val frTpe = c.openImplicits.headOption match {
      case Some(ImplicitCandidate(_, _, TypeRef(_, _, List(_, tpe)), _)) => tpe
      case _ => frTag.tpe.typeConstructor
    }

    if (isReprType1(tpe))
      abort("No Generic1 instance available for HList or Coproduct")

    if (isProduct1(tpe)) mkProductGeneric1(tpe, frTpe)
    else mkCoproductGeneric1(tpe, frTpe)
  }

  def mkProductGeneric1(tpe: Type, frTpe: Type): Tree = {
    val tparam = c.freshName(TypeName("P"))
    val reprName = c.freshName(TypeName("R"))
    val reprTpt = reprTypTree1(tpe, tparam)
    val (from, to) = CtorDtor.fromTo(tpe, tq"$reprName[$AnyTpe]")
    q"""
      type $reprName[$tparam] = $reprTpt
      $generic1.unsafeInstance[$tpe, $frTpe, $reprName]({ case $to }, { case $from })
    """
  }

  def mkCoproductGeneric1(tpe: Type, frTpe: Type): Tree = {
    def mkCoproductCases(tpe: Type, index: Int) = {
      val pat = c.freshName(TermName("pat"))
      val tc = tpe.typeConstructor
      val params = tc.typeParams.map(_ => Bind(typeNames.WILDCARD, EmptyTree))
      val tpt = AppliedTypeTree(mkAttributedRef(tc), params)
      cq"$pat: $tpt => $index"
    }

    val name = c.freshName(TypeName("C"))
    val reprTpt = reprTypTree1(tpe, name)
    val reprName = c.freshName(TypeName("R"))
    val coproduct = objectRef[Coproduct.type]
    val toCases = ctorsOf1(tpe).zipWithIndex.map((mkCoproductCases _).tupled)
    val to = q"$coproduct.unsafeMkCoproduct((ft: @$unchecked) match { case ..$toCases }, ft).asInstanceOf[$reprName[$AnyTpe]]"
    val from = q"$coproduct.unsafeGet(rt).asInstanceOf[${appliedType(tpe, AnyTpe)}]"

    q"""
      type $reprName[$name] = $reprTpt
      $generic1.unsafeInstance[$tpe, $frTpe, $reprName](ft => $to, rt => $from)
    """
  }
}

class IsHCons1Macros(val c: whitebox.Context) extends IsCons1Macros {
  import c.universe._

  def mkIsHCons1Impl[L[_], FH[_[_]], FT[_[_]]]
    (implicit lTag: WeakTypeTag[L[_]], fhTag: WeakTypeTag[FH[Any]], ftTag: WeakTypeTag[FT[Any]]): Tree =
      mkIsCons1(lTag.tpe, fhTag.tpe.typeConstructor, ftTag.tpe.typeConstructor)

  val isCons1TC: Tree = objectRef[IsHCons1.type]
  val consTpe: Type = hconsTpe

  private[this] val cons = objectRef[::.type]

  override def mkPackUnpack(hdName: TypeName, tlName: TypeName): (Tree, Tree) =
    (q"$cons(_, _)", q"{ case $cons(hd, tl) => (hd, tl) }")

  @deprecated("Kept for binary compatibility", "2.3.6")
  def mkPackUnpack(nme: TypeName, lTpt: Tree, hdTpt: Tree, tlTpt: Tree): (Tree, Tree) = (
    q"def pack[$nme](u: ($hdTpt, $tlTpt)): $lTpt = $cons(u._1, u._2)",
    q"def unpack[$nme](p: $lTpt): ($hdTpt, $tlTpt) = (p.head, p.tail)"
  )
}

class IsCCons1Macros(val c: whitebox.Context) extends IsCons1Macros {
  import c.universe._
  import definitions._

  def mkIsCCons1Impl[L[_], FH[_[_]], FT[_[_]]]
    (implicit lTag: WeakTypeTag[L[_]], fhTag: WeakTypeTag[FH[Any]], ftTag: WeakTypeTag[FT[Any]]): Tree =
      mkIsCons1(lTag.tpe, fhTag.tpe.typeConstructor, ftTag.tpe.typeConstructor)

  val isCons1TC: Tree = objectRef[IsCCons1.type]
  val consTpe: Type = cconsTpe

  private[this] val left = objectRef[Left.type]
  private[this] val right = objectRef[Right.type]
  private[this] val inl = objectRef[Inl.type]
  private[this] val inr = objectRef[Inr.type]

  override def mkPackUnpack(hdName: TypeName, tlName: TypeName): (Tree, Tree) = (
    q"""{
      case $left(hd) => $inl(hd: $hdName[$AnyTpe])
      case $right(tl) => $inr(tl: $tlName[$AnyTpe])
    }""",
    q"""{
      case $inl(hd) => $left(hd: $hdName[$AnyTpe])
      case $inr(tl) => $right(tl: $tlName[$AnyTpe])
    }"""
  )

  @deprecated("Kept for binary compatibility", "2.3.6")
  def mkPackUnpack(nme: TypeName, lTpt: Tree, hdTpt: Tree, tlTpt: Tree): (Tree, Tree) = {
    val Either = typeOf[Either[Any, Any]].typeConstructor

    (
      q"""def pack[$nme](u: $Either[$hdTpt, $tlTpt]): $lTpt = u match {
        case $left(hd) => $inl[$hdTpt, $tlTpt](hd)
        case $right(tl) => $inr[$hdTpt, $tlTpt](tl)
      }""",
      q"""def unpack[$nme](p: $lTpt): $Either[$hdTpt, $tlTpt] = p match {
        case $inl(hd) => $left[$hdTpt, $tlTpt](hd)
        case $inr(tl) => $right[$hdTpt, $tlTpt](tl)
      }"""
    )
  }
}

trait IsCons1Macros extends CaseClassMacros {
  val c: whitebox.Context
  import c.ImplicitCandidate
  import c.internal._
  import c.universe._

  def isCons1TC: Tree
  def consTpe: Type

  @deprecated("Kept for binary compatibility", "2.3.6")
  def mkPackUnpack(nme: TypeName, lTpt: Tree, hdTpt: Tree, tlTpt: Tree): (Tree, Tree)

  def mkPackUnpack(hdName: TypeName, tlName: TypeName): (Tree, Tree) =
    (EmptyTree, EmptyTree)

  def mkIsCons1(lTpe: Type, fhTpe0: Type, ftTpe0: Type): Tree = {
    val lParam = lTpe.typeParams.head
    val lParamTpe = lParam.asType.toType
    val lDealiasedTpe = appliedType(lTpe, lParamTpe).dealias

    val (fhTpe, ftTpe) = c.openImplicits.headOption match {
      case Some(ImplicitCandidate(_, _, TypeRef(_, _, List(_, fh, ft)), _)) => (fh, ft)
      case _ => (fhTpe0, ftTpe0)
    }

    if (!(lDealiasedTpe.typeConstructor =:= consTpe))
      abort("Not H/CCons")

    val TypeRef(_, _, List(hd, tl)) = lDealiasedTpe: @unchecked
    val hdPoly = polyType(List(lParam), hd)
    val tlPoly = polyType(List(lParam), tl)
    val name = TypeName(c.freshName())
    val hdTpt = appliedTypTree1(hdPoly, lParamTpe, name)
    val tlTpt = appliedTypTree1(tlPoly, lParamTpe, name)
    val hdName = c.freshName(TypeName("H"))
    val tlName = c.freshName(TypeName("T"))
    val (pack, unpack) = mkPackUnpack(hdName, tlName)

    q"""
      type $hdName[$name] = $hdTpt
      type $tlName[$name] = $tlTpt
      $isCons1TC.unsafeInstance[$lTpe, $fhTpe, $ftTpe, $hdName, $tlName]($pack, $unpack)
    """
  }
}

class Split1Macros(val c: whitebox.Context) extends CaseClassMacros {
  import c.ImplicitCandidate
  import c.internal._
  import c.universe._

  def mkSplit1Impl[L[_], FO[_[_]], FI[_[_]]]
    (implicit lTag: WeakTypeTag[L[_]], foTag: WeakTypeTag[FO[Any]], fiTag: WeakTypeTag[FI[Any]]): Tree = {
    val lTpe = lTag.tpe

    val (foTpe, fiTpe) = c.openImplicits.headOption match {
      case Some(ImplicitCandidate(_, _, TypeRef(_, _, List(_, fo, fi)), _)) => (fo, fi)
      case _ => (foTag.tpe.typeConstructor, fiTag.tpe.typeConstructor)
    }

    if (isReprType1(lTpe))
      abort("No Split1 instance available for HList or Coproduct")

    val lParam = lTpe.typeParams.head
    val lParamTpe = lParam.asType.toType
    val lDealiasedTpe = appliedType(lTpe, lParamTpe).dealias

    def balanced(args: List[Type]): Boolean =
      args.find(_.contains(lParam)).exists { pivot =>
        !(pivot =:= lParamTpe) && args.forall { arg =>
          arg =:= pivot || !arg.contains(lParam)
        }
      }

    val name = TypeName(c.freshName())
    val (oTpt, iTpt) = lDealiasedTpe match {
      case tpe @ TypeRef(_, _, args) if balanced(args) =>
        val pivot = args.find(_.contains(lParam)).get
        val oPoly = polyType(List(lParam), appliedType(tpe.typeConstructor, args.map(arg => if (arg =:= pivot) lParamTpe else arg)))
        val oTpt = appliedTypTree1(oPoly, lParamTpe, name)
        val iPoly = polyType(List(lParam), pivot)
        val iTpt = appliedTypTree1(iPoly, lParamTpe, name)
        (oTpt, iTpt)
      case other =>
        c.abort(c.enclosingPosition, s"Can't split $other into a non-trivial outer and inner type constructor")
    }

    val oName = c.freshName(TypeName("O"))
    val iName = c.freshName(TypeName("I"))
    val split1 = objectRef[Split1.type]

    q"""
      type $oName[$name] = $oTpt
      type $iName[$name] = $iTpt
      $split1.instance[$foTpe, $fiTpe, $oName, $iName]
    """
  }
}
