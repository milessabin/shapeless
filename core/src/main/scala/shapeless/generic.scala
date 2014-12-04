/*
 * Copyright (c) 2012-14 Lars Hupel, Miles Sabin
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

import scala.annotation.{ StaticAnnotation, tailrec }
import scala.reflect.api.Universe
import scala.reflect.macros.whitebox

trait Generic[T] {
  type Repr
  def to(t : T) : Repr
  def from(r : Repr) : T
}

object Generic {
  type Aux[T, Repr0] = Generic[T] { type Repr = Repr0 }

  def apply[T](implicit gen: Generic[T]): Aux[T, gen.Repr] = gen

  implicit def materialize[T, R]: Aux[T, R] = macro GenericMacros.materialize[T, R]
}

trait LabelledGeneric[T] extends Generic[T]

object LabelledGeneric {
  type Aux[T, Repr0] = LabelledGeneric[T]{ type Repr = Repr0 }

  def apply[T](implicit lgen: LabelledGeneric[T]): Aux[T, lgen.Repr] = lgen

  implicit def materialize[T, R]: Aux[T, R] = macro GenericMacros.materializeLabelled[T, R]
}

class nonGeneric extends StaticAnnotation

trait CaseClassMacros {
  val c: whitebox.Context

  import c.universe._
  import internal.constantType
  import Flag._

  def hlistTpe = typeOf[HList]
  def hnilTpe = typeOf[HNil]
  def hconsTpe = typeOf[::[_, _]].typeConstructor
  def coproductTpe = typeOf[Coproduct]
  def cnilTpe = typeOf[CNil]
  def cconsTpe = typeOf[:+:[_, _]].typeConstructor

  def atatTpe = typeOf[tag.@@[_,_]].typeConstructor
  def fieldTypeTpe = typeOf[shapeless.labelled.FieldType[_, _]].typeConstructor

  def abort(msg: String) =
    c.abort(c.enclosingPosition, msg)

  def isProduct(tpe: Type): Boolean =
    tpe =:= typeOf[Unit] || (tpe.typeSymbol.isClass && isCaseClassLike(classSym(tpe)))

  def isCoproduct(tpe: Type): Boolean = {
    val sym = tpe.typeSymbol
    if(!sym.isClass) false
    else {
      val sym = classSym(tpe)
      (sym.isTrait || sym.isAbstract) && sym.isSealed
    }
  }

  def fieldsOf(tpe: Type): List[(TermName, Type)] =
    tpe.decls.toList collect {
      case sym: TermSymbol if isCaseAccessorLike(sym) => (sym.name, sym.typeSignatureIn(tpe).finalResultType)
    }

  def ctorsOf(tpe: Type): List[Type] = {
    def collectCtors(classSym: ClassSymbol): List[ClassSymbol] = {
      classSym.knownDirectSubclasses.toList flatMap { child0 =>
        val child = child0.asClass
        child.typeSignature // Workaround for <https://issues.scala-lang.org/browse/SI-7755>
        if (isCaseClassLike(child))
          List(child)
        else if (child.isSealed)
          collectCtors(child)
        else
          abort(s"$child is not case class like or a sealed trait")
      }
    }

    if(isProduct(tpe))
      List(tpe)
    else if(isCoproduct(tpe)) {
      val ctors = collectCtors(classSym(tpe)).sortBy(_.fullName)
      if (ctors.isEmpty) abort(s"Sealed trait $tpe has no case class subtypes")

      // We're using an extremely optimistic strategy here, basically ignoring
      // the existence of any existential types.
      val baseTpe: TypeRef = tpe.dealias match {
        case tr: TypeRef => tr
        case _ => abort(s"bad type $tpe")
      }

      ctors map { sym =>
        val subTpe = sym.asType.toType
        val normalized = sym.typeParams match {
          case Nil  => subTpe
          case tpes => appliedType(subTpe, baseTpe.args)
        }

        normalized
      }
    }
    else
      abort(s"$tpe is not a case class, case class-like, a sealed trait or Unit")
  }

  def nameAsValue(name: Name): Constant = Constant(name.decodedName.toString.trim)

  def nameOf(tpe: Type) = tpe.typeSymbol.name

  def mkCompoundTpe(nil: Type, cons: Type, items: List[Type]): Type =
    items.foldRight(nil) { case (tpe, acc) => appliedType(cons, List(tpe, acc)) }

  def mkFieldTpe(name: Name, valueTpe: Type): Type = {
    val keyTpe = appliedType(atatTpe, List(typeOf[scala.Symbol], constantType(nameAsValue(name))))
    appliedType(fieldTypeTpe, List(keyTpe, valueTpe))
  }

  def mkHListTpe(items: List[Type]): Type =
    mkCompoundTpe(hnilTpe, hconsTpe, items)

  def mkRecordTpe(fields: List[(TermName, Type)]): Type =
    mkCompoundTpe(hnilTpe, hconsTpe, fields.map((mkFieldTpe _).tupled))

  def mkCoproductTpe(items: List[Type]): Type =
    mkCompoundTpe(cnilTpe, cconsTpe, items)

  def mkUnionTpe(fields: List[(TermName, Type)]): Type =
    mkCompoundTpe(cnilTpe, cconsTpe, fields.map((mkFieldTpe _).tupled))

  def unfoldCompoundTpe(compoundTpe: Type, nil: Type, cons: Type): List[Type] = {
    @tailrec
    def loop(tpe: Type, acc: List[Type]): List[Type] =
      tpe.dealias match {
        case TypeRef(_, consSym, List(hd, tl))
          if consSym.asType.toType.typeConstructor =:= cons => loop(tl, hd :: acc)
        case `nil` => acc
        case other => abort(s"Bad compound type $compoundTpe")
      }
    loop(compoundTpe, Nil).reverse
  }

  def hlistElements(tpe: Type): List[Type] =
    unfoldCompoundTpe(tpe, hnilTpe, hconsTpe)

  def coproductElements(tpe: Type): List[Type] =
    unfoldCompoundTpe(tpe, cnilTpe, cconsTpe)

  def reprTpe(tpe: Type, labelled: Boolean): Type = {
    if(isProduct(tpe)) {
      val fields = fieldsOf(tpe)
      if(labelled)
        mkRecordTpe(fields)
      else
        mkHListTpe(fields.map(_._2))
    } else {
      val ctors = ctorsOf(tpe)
      if(labelled) {
        val labelledCases = ctors.map(tpe => (nameOf(tpe).toTermName, tpe))
        mkUnionTpe(labelledCases)
      } else
        mkCoproductTpe(ctors)
    }
  }

  def isCaseClassLike(sym: ClassSymbol): Boolean =
    sym.isCaseClass ||
    (!sym.isAbstract && !sym.isTrait && sym.knownDirectSubclasses.isEmpty && fieldsOf(sym.typeSignature).nonEmpty)

  def isCaseObjectLike(sym: ClassSymbol): Boolean = sym.isModuleClass && isCaseClassLike(sym)

  def isCaseAccessorLike(sym: TermSymbol): Boolean =
    !isNonGeneric(sym) && sym.isPublic && (if(sym.owner.asClass.isCaseClass) sym.isCaseAccessor else sym.isAccessor)

  def classSym(tpe: Type): ClassSymbol = {
    val sym = tpe.typeSymbol
    if (!sym.isClass)
      abort(s"$sym is not a class or trait")

    val classSym = sym.asClass
    classSym.typeSignature // Workaround for <https://issues.scala-lang.org/browse/SI-7755>

    classSym
  }

  // See https://github.com/milessabin/shapeless/issues/212
  def companionRef(tpe: Type): Tree = {
    val global = c.universe.asInstanceOf[scala.tools.nsc.Global]
    val gTpe = tpe.asInstanceOf[global.Type]
    val pre = gTpe.prefix
    val sym = gTpe.typeSymbol.companionSymbol
    global.gen.mkAttributedRef(pre, sym).asInstanceOf[Tree]
  }

  def isNonGeneric(sym: Symbol): Boolean = {
    def check(sym: Symbol): Boolean = {
      // See https://issues.scala-lang.org/browse/SI-7424
      sym.typeSignature                   // force loading method's signature
      sym.annotations.foreach(_.tree.tpe) // force loading all the annotations

      sym.annotations.exists(_.tree.tpe =:= typeOf[nonGeneric])
    }

    // See https://issues.scala-lang.org/browse/SI-7561
    check(sym) ||
    (sym.isTerm && sym.asTerm.isAccessor && check(sym.asTerm.accessed)) ||
    sym.overrides.exists(isNonGeneric)
  }
}

class GenericMacros(val c: whitebox.Context) extends CaseClassMacros {
  import c.universe._
  import internal.constantType
  import Flag._

  def materialize[T: WeakTypeTag, R: WeakTypeTag] =
    materializeAux(false, weakTypeOf[T])

  def materializeLabelled[T: WeakTypeTag, R: WeakTypeTag] =
    materializeAux(true, weakTypeOf[T])

  def materializeAux(labelled: Boolean, tpe: Type): Tree = {
    if(tpe <:< typeOf[HList] || tpe <:< typeOf[Coproduct])
      materializeIdentityGeneric(tpe, labelled)
    else
      materializeGeneric(tpe, labelled)
  }

  def materializeGeneric(tpe: Type, labelled: Boolean) = {
    def mkElem(elem: Tree, name: Name, tpe: Type): Tree =
      if(labelled) q"$elem.asInstanceOf[${mkFieldTpe(name, tpe)}]" else elem

    def mkCoproductCases(tpe: Type, index: Int): (CaseDef, CaseDef) = {
      val name = TermName(c.freshName("pat"))

      def mkCoproductValue(tree: Tree): Tree =
        (0 until index).foldLeft(q"_root_.shapeless.Inl($tree)": Tree) {
          case (acc, _) => q"_root_.shapeless.Inr($acc)"
        }

      val body = mkCoproductValue(mkElem(q"$name: $tpe", nameOf(tpe), tpe))
      val pat = mkCoproductValue(pq"$name")
      (
        cq"$name: $tpe => $body",
        cq"$pat => $name"
      )
    }

    def mkProductCases(tpe: Type): (CaseDef, CaseDef) = {
      def mkCase(lhs: Tree, rhs: Tree) = cq"$lhs => $rhs"

      if(tpe =:= typeOf[Unit])
        (
          cq"() => _root_.shapeless.HNil",
          cq"_root_.shapeless.HNil => ()"
        )
      else if(isCaseObjectLike(tpe.typeSymbol.asClass)) {
        val singleton =
          tpe match {
            case SingleType(_, singleton) => singleton
            case TypeRef(pre, sym, args) => sym.asClass.module
            case other =>
              abort(s"Bad case object-like type $tpe")
          }

        (
          cq"_: $tpe => _root_.shapeless.HNil",
          cq"_root_.shapeless.HNil => $singleton"
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
                case ((bound, name, tpe), acc) =>
                  val elem = mkElem(q"$bound", name, tpe)
                  q"_root_.shapeless.::($elem, $acc)"
              }
            cq"$lhs => $rhs"
          } else {
            val lhs = TermName(c.freshName("pat"))
            val rhs =
              fieldsOf(tpe).foldRight(q"_root_.shapeless.HNil": Tree) {
                case ((name, tpe), acc) =>
                  val elem = mkElem(q"$lhs.$name", name, tpe)
                  q"_root_.shapeless.::($elem, $acc)"
              }
            cq"$lhs => $rhs"
          }

        val from = {
          val lhs =
            binders.foldRight(q"_root_.shapeless.HNil": Tree) {
              case ((bound, _, _), acc) => pq"_root_.shapeless.::($bound, $acc)"
            }

          val rhs = {
            val ctorArgs = binders.map { case (bound, name, tpe) => mkElem(Ident(bound), name, tpe) }
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

    val (toCases, fromCases) =
      if(isProduct(tpe)) {
        val (to, from) = mkProductCases(tpe)
        (List(to), List(from))
      } else {
        val (to, from) = (ctorsOf(tpe) zip (Stream from 0) map (mkCoproductCases _).tupled).unzip
        (to, from :+ cq"_ => _root_.scala.Predef.???")
      }

    val genericTypeConstructor =
      (if(labelled) typeOf[LabelledGeneric[_]].typeConstructor
       else typeOf[Generic[_]].typeConstructor).typeSymbol

    val clsName = TypeName(c.freshName())
    q"""
      final class $clsName extends $genericTypeConstructor[$tpe] {
        type Repr = ${reprTpe(tpe, labelled)}
        def to(p: $tpe): Repr = p match { case ..$toCases }
        def from(p: Repr): $tpe = p match { case ..$fromCases }
      }
      new $clsName()
    """
  }

  def materializeIdentityGeneric(tpe: Type, labelled: Boolean) = {
    val genericTypeConstructor =
      (if(labelled) typeOf[LabelledGeneric[_]].typeConstructor
       else typeOf[Generic[_]].typeConstructor).typeSymbol

    val clsName = TypeName(c.freshName())
    q"""
      final class $clsName extends $genericTypeConstructor[$tpe] {
        type Repr = $tpe
        def to(p: $tpe): $tpe = p
        def from(p: $tpe): $tpe = p
      }
      new $clsName()
    """
  }
}
