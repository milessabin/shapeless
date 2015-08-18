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

import scala.language.existentials
import scala.language.experimental.macros

import scala.annotation.{ StaticAnnotation, tailrec }
import scala.reflect.api.Universe
import scala.reflect.macros.{ blackbox, whitebox }

import ops.{ hlist, coproduct }

trait Generic1[F[_], FR[_[_]]] extends Serializable {
  type R[t]

  lazy val fr: FR[R] = mkFrr

  def to[T](ft: F[T]): R[T]
  def from[T](rt: R[T]): F[T]

  def mkFrr: FR[R]
}

object Generic1 {
  type Aux[F[_], FR[_[_]], R0[_]] = Generic1[F, FR] { type R[t] = R0[t] }

  def apply[F[_], FR[_[_]]](implicit gen: Generic1[F, FR]): Aux[F, FR, gen.R] = gen

  implicit def materialize[T[_], FR[_[_]]]: Generic1[T, FR] = macro Generic1Macros.materialize[T, FR]
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

object IsHCons1 {
  type Aux[L[_], FH[_[_]], FT[_[_]], H0[_], T0[_] <: HList] = IsHCons1[L, FH, FT] { type H[t] = H0[t] ; type T[t] = T0[t] }

  def apply[L[_], FH[_[_]], FT[_[_]]](implicit tc: IsHCons1[L, FH, FT]): Aux[L, FH, FT, tc.H, tc.T] = tc

  implicit def mkIsHCons1[L[_], FH[_[_]], FT[_[_]]]: IsHCons1[L, FH, FT] = macro IsHCons1Macros.mkIsHCons1Impl[L, FH, FT]
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

object IsCCons1 {
  type Aux[L[_], FH[_[_]], FT[_[_]], H0[_], T0[_] <: Coproduct] = IsCCons1[L, FH, FT] { type H[t] = H0[t] ; type T[t] = T0[t] }

  def apply[L[_], FH[_[_]], FT[_[_]]](implicit tc: IsCCons1[L, FH, FT]): Aux[L, FH, FT, tc.H, tc.T] = tc

  implicit def mkIsCCons1[L[_], FH[_[_]], FT[_[_]]]: IsCCons1[L, FH, FT] = macro IsCCons1Macros.mkIsCCons1Impl[L, FH, FT]
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

object Split1 {
  type Aux[L[_], FO[_[_]], FI[_[_]], O0[_], I0[_]] = Split1[L, FO, FI] { type O[T] = O0[T] ; type I[T] = I0[T] }

  implicit def apply[L[_], FO[_[_]], FI[_[_]]]: Split1[L, FO, FI] = macro Split1Macros.materialize[L, FO, FI]
}

class Generic1Macros(val c: whitebox.Context) extends CaseClassMacros {
  import c.universe._
  import internal.constantType
  import Flag._

  def materialize[T[_], FR[_[_]]](implicit tTag: WeakTypeTag[T[_]], frTag: WeakTypeTag[FR[Id]]): Tree = {
    val tpe = weakTypeOf[T[_]]
    val frTpe = frTag.tpe.typeConstructor

    if(isReprType1(tpe))
      abort("No Generic instance available for HList or Coproduct")

    if(isProduct(tpe))
      mkProductGeneric1(tpe, frTpe)
    else
      mkCoproductGeneric1(tpe, frTpe)
  }

  def mkProductGeneric1(tpe: Type, frTpe: Type): Tree = {
    def mkProductCases: (CaseDef, CaseDef) = {
      if(tpe =:= typeOf[Unit])
        (
          cq"() => _root_.shapeless.HNil",
          cq"_root_.shapeless.HNil => ()"
        )
      else if(isCaseObjectLike(tpe.typeSymbol.asClass)) {
        val singleton =
          tpe match {
            case SingleType(pre, sym) =>
              c.internal.gen.mkAttributedRef(pre, sym)
            case TypeRef(pre, sym, List()) if sym.isModule =>
              c.internal.gen.mkAttributedRef(pre, sym.asModule)
            case TypeRef(pre, sym, List()) if sym.isModuleClass =>
              c.internal.gen.mkAttributedRef(pre, sym.asClass.module)
            case other =>
              abort(s"Bad case object-like type $tpe")
          }

        (
          cq"_: $tpe => _root_.shapeless.HNil",
          cq"_root_.shapeless.HNil => $singleton: $tpe"
        )
      } else {
        val sym = tpe.typeSymbol
        val isCaseClass = sym.asClass.isCaseClass
        def hasNonGenericCompanionMember(name: String): Boolean = {
          val mSym = sym.companion.typeSignature.member(TermName(name))
          mSym != NoSymbol && !isNonGeneric(mSym)
        }

        val binders = fieldsOf(tpe).map { case (name, tpe) => (TermName(c.freshName("pat")), name, tpe) }

        val to =
          if(isCaseClass || hasNonGenericCompanionMember("unapply")) {
            val lhs = pq"${companionRef(tpe)}(..${binders.map(x => pq"${x._1}")})"
            val rhs =
              binders.foldRight(q"_root_.shapeless.HNil": Tree) {
                case ((bound, name, tpe), acc) => q"_root_.shapeless.::($bound, $acc)"
              }
            cq"$lhs => $rhs"
          } else {
            val lhs = TermName(c.freshName("pat"))
            val rhs =
              fieldsOf(tpe).foldRight(q"_root_.shapeless.HNil": Tree) {
                case ((name, tpe), acc) => q"_root_.shapeless.::($lhs.$name, $acc)"
              }
            cq"$lhs => $rhs"
          }

        val from = {
          val lhs =
            binders.foldRight(q"_root_.shapeless.HNil": Tree) {
              case ((bound, _, _), acc) => pq"_root_.shapeless.::($bound, $acc)"
            }

          val rhs = {
            val ctorArgs = binders.map { case (bound, name, tpe) => Ident(bound) }
            if(isCaseClass || hasNonGenericCompanionMember("apply"))
              q"${companionRef(tpe)}(..$ctorArgs)"
            else
              q"new $tpe(..$ctorArgs)"
          }

          cq"$lhs => $rhs"
        }

        (to, from)
      }
    }

    val (toCases, fromCases) = {
      val (to, from) = mkProductCases
      (List(to), List(from))
    }

    val nme = TypeName(c.freshName)
    val tpeTpt = appliedTypTree1(tpe, param1(tpe), nme)
    val reprTpt = reprTypTree1(tpe, nme)
    val frTpt = mkAttributedRef(frTpe)
    val rnme = TypeName(c.freshName)

    val clsName = TypeName(c.freshName())
    q"""
      final class $clsName extends _root_.shapeless.Generic1[$tpe, $frTpe] {
        type R[$nme] = $reprTpt

        def mkFrr: $frTpt[R] = _root_.shapeless.lazily[$frTpt[R]]

        def to[$nme](ft: $tpeTpt): R[$nme] = ft match { case ..$toCases }
        def from[$nme](rt: R[$nme]): $tpeTpt = rt match { case ..$fromCases }
      }
      type $rnme[$nme] = $reprTpt
      new $clsName(): _root_.shapeless.Generic1.Aux[$tpe, $frTpe, $rnme]
    """
  }

  def mkCoproductGeneric1(tpe: Type, frTpe: Type): Tree = {
    def mkCoproductCases(tpe: Type, index: Int): CaseDef = {
      val name = TermName(c.freshName("pat"))

      val tc = tpe.typeConstructor
      val params = tc.typeParams.map { _ => Bind(typeNames.WILDCARD, EmptyTree) }
      val tpeTpt = AppliedTypeTree(mkAttributedRef(tc), params)

      cq"$name: $tpeTpt => $index"
    }

    val nme = TypeName(c.freshName)
    val tpeTpt = appliedTypTree1(tpe, param1(tpe), nme)
    val reprTpt = reprTypTree1(tpe, nme)
    val frTpt = mkAttributedRef(frTpe)
    val rnme = TypeName(c.freshName)

    val to = {
      val toCases = ctorsOf1(tpe) zip (Stream from 0) map (mkCoproductCases _).tupled
      q"""_root_.shapeless.Coproduct.unsafeMkCoproduct((ft: Any) match { case ..$toCases }, ft).asInstanceOf[R[$nme]]"""
    }

    val clsName = TypeName(c.freshName())
    q"""
      final class $clsName extends _root_.shapeless.Generic1[$tpe, $frTpe] {
        type R[$nme] = $reprTpt

        def mkFrr: $frTpt[R] = _root_.shapeless.lazily[$frTpt[R]]

        def to[$nme](ft: $tpeTpt): R[$nme] = $to
        def from[$nme](rt: R[$nme]): $tpeTpt = _root_.shapeless.Coproduct.unsafeGet(rt).asInstanceOf[$tpeTpt]
      }
      type $rnme[$nme] = $reprTpt
      new $clsName(): _root_.shapeless.Generic1.Aux[$tpe, $frTpe, $rnme]
    """
  }
}

class IsHCons1Macros(val c: whitebox.Context) extends IsCons1Macros {
  import c.universe._

  def mkIsHCons1Impl[L[_], FH[_[_]], FT[_[_]]]
    (implicit lTag: WeakTypeTag[L[_]], fhTag: WeakTypeTag[FH[Id]], ftTag: WeakTypeTag[FT[Const[HNil]#λ]]): Tree =
      mkIsCons1(lTag.tpe, fhTag.tpe.typeConstructor, ftTag.tpe.typeConstructor)

  val isCons1TC: Tree = tq"_root_.shapeless.IsHCons1"
  val consTpe: Type = hconsTpe

  def mkPackUnpack(nme: TypeName, lTpt: Tree, hdTpt: Tree, tlTpt: Tree): (Tree, Tree) =
    (
      q"""
        def pack[$nme](u: ($hdTpt, $tlTpt)): $lTpt = _root_.shapeless.::(u._1, u._2)
      """,
      q"""
        def unpack[$nme](p: $lTpt): ($hdTpt, $tlTpt) = (p.head, p.tail)
      """
    )
}

class IsCCons1Macros(val c: whitebox.Context) extends IsCons1Macros {
  import c.universe._

  def mkIsCCons1Impl[L[_], FH[_[_]], FT[_[_]]]
    (implicit lTag: WeakTypeTag[L[_]], fhTag: WeakTypeTag[FH[Id]], ftTag: WeakTypeTag[FT[Const[CNil]#λ]]): Tree =
      mkIsCons1(lTag.tpe, fhTag.tpe.typeConstructor, ftTag.tpe.typeConstructor)

  val isCons1TC: Tree = tq"_root_.shapeless.IsCCons1"
  val consTpe: Type = cconsTpe

  def mkPackUnpack(nme: TypeName, lTpt: Tree, hdTpt: Tree, tlTpt: Tree): (Tree, Tree) =
    (
      q"""
        def pack[$nme](u: _root_.scala.Either[$hdTpt, $tlTpt]): $lTpt = u match {
          case _root_.scala.Left(hd) => _root_.shapeless.Inl[$hdTpt, $tlTpt](hd)
          case _root_.scala.Right(tl) => _root_.shapeless.Inr[$hdTpt, $tlTpt](tl)
        }
      """,
      q"""
        def unpack[$nme](p: $lTpt): _root_.scala.Either[$hdTpt, $tlTpt] = p match {
          case _root_.shapeless.Inl(hd) => _root_.scala.Left[$hdTpt, $tlTpt](hd)
          case _root_.shapeless.Inr(tl) => _root_.scala.Right[$hdTpt, $tlTpt](tl)
        }
      """
    )
}

trait IsCons1Macros extends CaseClassMacros {
  import c.universe._

  val isCons1TC: Tree
  val consTpe: Type

  def mkPackUnpack(nme: TypeName, lTpt: Tree, hdTpt: Tree, tlTpt: Tree): (Tree, Tree)

  def mkIsCons1(lTpe: Type, fhTpe: Type, ftTpe: Type): Tree = {
    val fhTpt = mkAttributedRef(fhTpe)
    val ftTpt = mkAttributedRef(ftTpe)

    val lParam = lTpe.typeParams.head
    val lParamTpe = lParam.asType.toType
    val lDealiasedTpe = appliedType(lTpe, lParamTpe).dealias

    if(!(lDealiasedTpe.typeConstructor =:= consTpe))
      abort("Not H/CCons")

    val TypeRef(_, _, List(hd, tl)) = lDealiasedTpe

    val lPoly = c.internal.polyType(List(lParam), lDealiasedTpe)
    val hdPoly = c.internal.polyType(List(lParam), hd)
    val tlPoly = c.internal.polyType(List(lParam), tl)

    val nme = TypeName(c.freshName)
    val lTpt = appliedTypTree1(lPoly, lParamTpe, nme)
    val hdTpt = appliedTypTree1(hdPoly, lParamTpe, nme)
    val tlTpt = appliedTypTree1(tlPoly, lParamTpe, nme)

    val (pack, unpack) = mkPackUnpack(nme, lTpt, hdTpt, tlTpt)
    q"""
      new $isCons1TC[$lTpe, $fhTpt, $ftTpt] {
        type H[$nme] = $hdTpt
        type T[$nme] = $tlTpt

        def mkFhh: $fhTpt[H] = _root_.shapeless.lazily[$fhTpt[H]]
        def mkFtt: $ftTpt[T] = _root_.shapeless.lazily[$ftTpt[T]]

        $pack
        $unpack
      }
    """
  }
}

class Split1Macros(val c: whitebox.Context) {
  import c.universe._

  def appliedTypTree1(tpe: Type, param: Type, arg: TypeName): Tree = {
    tpe match {
      case t if t =:= param =>
        Ident(arg)
      case PolyType(params, body) if params.head.asType.toType =:= param =>
        appliedTypTree1(body, param, arg)
      case t @ TypeRef(pre, sym, List()) if t.takesTypeArgs =>
        val argTrees = t.typeParams.map(sym => appliedTypTree1(sym.asType.toType, param, arg))
        AppliedTypeTree(c.internal.gen.mkAttributedRef(pre, sym), argTrees)
      case TypeRef(pre, sym, List()) =>
        c.internal.gen.mkAttributedRef(pre, sym)
      case TypeRef(pre, sym, args) =>
        val argTrees = args.map(appliedTypTree1(_, param, arg))
        AppliedTypeTree(c.internal.gen.mkAttributedRef(pre, sym), argTrees)
      case t if t.takesTypeArgs =>
        val argTrees = t.typeParams.map(sym => appliedTypTree1(sym.asType.toType, param, arg))
        AppliedTypeTree(c.internal.gen.mkAttributedRef(tpe.typeConstructor.typeSymbol), argTrees)
      case t =>
        tq"$tpe"
    }
  }

  def materialize[L[_], FO[_[_]], FI[_[_]]]
    (implicit lTag: WeakTypeTag[L[_]], foTag: WeakTypeTag[FO[Id]], fiTag: WeakTypeTag[FI[Id]]): Tree = {
    val lTpe = lTag.tpe
    val foTpe = foTag.tpe.typeConstructor
    val fiTpe = fiTag.tpe.typeConstructor

    val foTpt = c.internal.gen.mkAttributedRef(foTpe.typeSymbol)
    val fiTpt = c.internal.gen.mkAttributedRef(fiTpe.typeSymbol)

    val lParam = lTpe.typeParams.head
    val lParamTpe = lParam.asType.toType
    val lDealiasedTpe = appliedType(lTpe, lParamTpe).dealias

    val nme = TypeName(c.freshName)

    def balanced(args: List[Type]): Boolean =
      args.find(_.contains(lParam)).map { pivot =>
        !(pivot =:= lParamTpe) &&
        args.forall { arg =>
          arg =:= pivot || !arg.contains(lParam)
        }
      }.getOrElse(false)

    val (oTpt, iTpt) =
      lDealiasedTpe match {
        case tpe @ TypeRef(pre, sym, args) if balanced(args) =>
          val Some(pivot) = args.find(_.contains(lParam))
          val oPoly = c.internal.polyType(List(lParam), appliedType(tpe.typeConstructor, args.map { arg => if(arg =:= pivot) lParamTpe else arg }))
          val oTpt = appliedTypTree1(oPoly, lParamTpe, nme)
          val iPoly = c.internal.polyType(List(lParam), pivot)
          val iTpt = appliedTypTree1(iPoly, lParamTpe, nme)
          (oTpt, iTpt)
        case other =>
          c.abort(c.enclosingPosition, s"Can't split $other into a non-trivial outer and inner type constructor")
      }

    val lPoly = c.internal.polyType(List(lParam), lDealiasedTpe)
    val lTpt = appliedTypTree1(lPoly, lParamTpe, nme)

    q"""
      new _root_.shapeless.Split1[$lTpe, $foTpt, $fiTpt] {
        type O[$nme] = $oTpt
        type I[$nme] = $iTpt

        def mkFoo: $foTpt[O] = _root_.shapeless.lazily[$foTpt[O]]
        def mkFii: $fiTpt[I] = _root_.shapeless.lazily[$fiTpt[I]]

        def pack[$nme](u: O[I[$nme]]): $lTpt = u
        def unpack[$nme](p: $lTpt): O[I[$nme]] = p
      }
    """
  }
}
