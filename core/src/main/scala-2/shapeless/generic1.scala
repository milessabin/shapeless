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

trait Generic1ScalaCompat {

  implicit def mkGeneric10[T[_], U[_], FR[_[_], _[_]]]: Generic1[T, ({ type λ[t[_]] = FR[t, U] })#λ] =
    macro Generic1Macros.mkGeneric1Impl[T, ({ type λ[t[_]] = FR[t, U] })#λ]

  implicit def mkGeneric11[T[_], U[_], FR[_[_], _[_]]]: Generic1[T, ({ type λ[t[_]] = FR[U, t] })#λ] =
    macro Generic1Macros.mkGeneric1Impl[T, ({ type λ[t[_]] = FR[U, t] })#λ]
}

trait Generic10ScalaCompat {
  implicit def apply[T[_], FR[_[_]]]: Generic1[T, FR] = macro Generic1Macros.mkGeneric1Impl[T, FR]
}

trait IsHCons1ScalaCompat {

  implicit def mkIsHCons10[L[_], FH[_[_], _[_]], U[_], FT[_[_]]]: IsHCons1[L, ({ type λ[t[_]] = FH[t, U] })#λ, FT] =
    macro IsHCons1Macros.mkIsHCons1Impl[L, ({ type λ[t[_]] = FH[t, U] })#λ, FT]

  implicit def mkIsHCons11[L[_], FH[_[_], _[_]], U[_], FT[_[_]]]: IsHCons1[L, ({ type λ[t[_]] = FH[U, t] })#λ, FT] =
    macro IsHCons1Macros.mkIsHCons1Impl[L, ({ type λ[t[_]] = FH[U, t] })#λ, FT]

  implicit def mkIsHCons12[L[_], FH[_[_]], FT[_[_], _[_]], U[_]]: IsHCons1[L, FH, ({ type λ[t[_]] = FT[t, U] })#λ] =
    macro IsHCons1Macros.mkIsHCons1Impl[L, FH, ({ type λ[t[_]] = FT[t, U] })#λ]

  implicit def mkIsHCons13[L[_], FH[_[_]], FT[_[_], _[_]], U[_]]: IsHCons1[L, FH, ({ type λ[t[_]] = FT[U, t] })#λ] =
    macro IsHCons1Macros.mkIsHCons1Impl[L, FH, ({ type λ[t[_]] = FT[U, t] })#λ]
}

trait IsHCons10ScalaCompat {
  implicit def apply[L[_], FH[_[_]], FT[_[_]]]: IsHCons1[L, FH, FT] = macro IsHCons1Macros.mkIsHCons1Impl[L, FH, FT]
}

trait IsCCons1ScalaCompat {

  implicit def mkIsCCons10[L[_], FH[_[_], _[_]], U[_], FT[_[_]]]: IsCCons1[L, ({ type λ[t[_]] = FH[t, U] })#λ, FT] =
    macro IsCCons1Macros.mkIsCCons1Impl[L, ({ type λ[t[_]] = FH[t, U] })#λ, FT]

  implicit def mkIsCCons11[L[_], FH[_[_], _[_]], U[_], FT[_[_]]]: IsCCons1[L, ({ type λ[t[_]] = FH[U, t] })#λ, FT] =
    macro IsCCons1Macros.mkIsCCons1Impl[L, ({ type λ[t[_]] = FH[U, t] })#λ, FT]

  implicit def mkIsCCons12[L[_], FH[_[_]], FT[_[_], _[_]], U[_]]: IsCCons1[L, FH, ({ type λ[t[_]] = FT[t, U] })#λ] =
    macro IsCCons1Macros.mkIsCCons1Impl[L, FH, ({ type λ[t[_]] = FT[t, U] })#λ]

  implicit def mkIsCCons13[L[_], FH[_[_]], FT[_[_], _[_]], U[_]]: IsCCons1[L, FH, ({ type λ[t[_]] = FT[U, t] })#λ] =
    macro IsCCons1Macros.mkIsCCons1Impl[L, FH, ({ type λ[t[_]] = FT[U, t] })#λ]
}

trait IsCCons10ScalaCompat {
  implicit def apply[L[_], FH[_[_]], FT[_[_]]]: IsCCons1[L, FH, FT] = macro IsCCons1Macros.mkIsCCons1Impl[L, FH, FT]
}

trait Split1ScalaCompat {

  implicit def mkSplit10[L[_], FO[_[_], _[_]], U[_], FI[_[_]]]: Split1[L, ({ type λ[t[_]] = FO[t, U] })#λ, FI] =
    macro Split1Macros.mkSplit1Impl[L, ({ type λ[t[_]] = FO[t, U] })#λ, FI]

  implicit def mkSplit11[L[_], FO[_[_], _[_]], U[_], FI[_[_]]]: Split1[L, ({ type λ[t[_]] = FO[U, t] })#λ, FI] =
    macro Split1Macros.mkSplit1Impl[L, ({ type λ[t[_]] = FO[U, t] })#λ, FI]

  implicit def mkSplit12[L[_], FO[_[_]], FI[_[_], _[_]], U[_]]: Split1[L, FO, ({ type λ[t[_]] = FI[t, U] })#λ] =
    macro Split1Macros.mkSplit1Impl[L, FO, ({ type λ[t[_]] = FI[t, U] })#λ]

  implicit def mkSplit13[L[_], FO[_[_]], FI[_[_], _[_]], U[_]]: Split1[L, FO, ({ type λ[t[_]] = FI[U, t] })#λ] =
    macro Split1Macros.mkSplit1Impl[L, FO, ({ type λ[t[_]] = FI[U, t] })#λ]
}

trait Split10ScalaCompat {
  implicit def apply[L[_], FO[_[_]], FI[_[_]]]: Split1[L, FO, FI] = macro Split1Macros.mkSplit1Impl[L, FO, FI]
}

class Generic1Macros(val c: whitebox.Context) extends CaseClassMacros {
  import c.ImplicitCandidate
  import c.universe._
  import definitions._

  private val generic1 = objectRef[Generic1.type]

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
    val ctorDtor = CtorDtor(tpe)
    val (p, ts) = ctorDtor.binding
    val to = cq"$p => ${mkHListValue(ts)}"
    val (rp, rts) = ctorDtor.reprBinding
    val from = cq"$rp => ${ctorDtor.construct(rts)}"
    val name = TypeName(c.freshName("P"))
    val reprTpt = reprTypTree1(tpe, name)
    val reprName = TypeName(c.freshName("R"))

    q"""
      type $reprName[$name] = $reprTpt
      $generic1.unsafeInstance[$tpe, $frTpe, $reprName]({ case $to }, { case $from })
    """
  }

  def mkCoproductGeneric1(tpe: Type, frTpe: Type): Tree = {
    def mkCoproductCases(tpe: Type, index: Int) = {
      val pat = TermName(c.freshName("pat"))
      val tc = tpe.typeConstructor
      val params = tc.typeParams.map(_ => Bind(typeNames.WILDCARD, EmptyTree))
      val tpt = AppliedTypeTree(mkAttributedRef(tc), params)
      cq"$pat: $tpt => $index"
    }

    val name = TypeName(c.freshName("C"))
    val reprTpt = reprTypTree1(tpe, name)
    val reprName = TypeName(c.freshName("R"))
    val coproduct = objectRef[Coproduct.type]
    val toCases = ctorsOf1(tpe).zipWithIndex.map((mkCoproductCases _).tupled)
    val to = q"$coproduct.unsafeMkCoproduct((ft: @_root_.scala.unchecked) match { case ..$toCases }, ft).asInstanceOf[$reprName[$AnyTpe]]"
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

  def mkPackUnpack(hdName: TypeName, tlName: TypeName): (Tree, Tree) = {
    val cons = objectRef[::.type]
    (q"$cons(_, _)", q"{ case $cons(hd, tl) => (hd, tl) }")
  }
}

class IsCCons1Macros(val c: whitebox.Context) extends IsCons1Macros {
  import c.universe._
  import definitions._

  def mkIsCCons1Impl[L[_], FH[_[_]], FT[_[_]]]
  (implicit lTag: WeakTypeTag[L[_]], fhTag: WeakTypeTag[FH[Any]], ftTag: WeakTypeTag[FT[Any]]): Tree =
    mkIsCons1(lTag.tpe, fhTag.tpe.typeConstructor, ftTag.tpe.typeConstructor)

  val isCons1TC: Tree = objectRef[IsCCons1.type]
  val consTpe: Type = cconsTpe

  def mkPackUnpack(hdName: TypeName, tlName: TypeName): (Tree, Tree) = {
    val left = objectRef[Left.type]
    val right = objectRef[Right.type]
    val inl = objectRef[Inl.type]
    val inr = objectRef[Inr.type]

    (
      q"""{
        case $left(hd) => $inl(hd: $hdName[$AnyTpe])
        case $right(tl) => $inr(tl: $tlName[$AnyTpe])
      }""",
      q"""{
        case $inl(hd) => $left(hd: $hdName[$AnyTpe])
        case $inr(tl) => $right(tl: $tlName[$AnyTpe])
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
  def mkPackUnpack(hdName: TypeName, tlName: TypeName): (Tree, Tree)

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

    val TypeRef(_, _, List(hd, tl)) = (lDealiasedTpe: @unchecked)
    val hdPoly = polyType(List(lParam), hd)
    val tlPoly = polyType(List(lParam), tl)
    val name = TypeName(c.freshName())
    val hdTpt = appliedTypTree1(hdPoly, lParamTpe, name)
    val tlTpt = appliedTypTree1(tlPoly, lParamTpe, name)
    val hdName = TypeName(c.freshName("H"))
    val tlName = TypeName(c.freshName("T"))
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

    val oName = TypeName(c.freshName("O"))
    val iName = TypeName(c.freshName("I"))
    val split1 = objectRef[Split1.type]

    q"""
      type $oName[$name] = $oTpt
      type $iName[$name] = $iTpt
      $split1.instance[$foTpe, $fiTpe, $oName, $iName]
    """
  }
}
