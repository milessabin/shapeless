/*
 * Copyright (c) 2012-15 Lars Hupel, Miles Sabin
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
import scala.reflect.macros.{ blackbox, whitebox }

import ops.{ hlist, coproduct }

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

  implicit def materializeProduct[T, K <: HList, V <: HList, R <: HList]
    (implicit
      lab: DefaultSymbolicLabelling.Aux[T, K],
      gen: Generic.Aux[T, V],
      zip: hlist.ZipWithKeys.Aux[K, V, R],
      ev: R <:< V
    ): Aux[T, R] =
    new LabelledGeneric[T] {
      type Repr = R
      def to(t: T): Repr = zip(gen.to(t))
      def from(r: Repr): T = gen.from(r)
    }

  implicit def materializeCoproduct[T, K <: HList, V <: Coproduct, R <: Coproduct]
    (implicit
      lab: DefaultSymbolicLabelling.Aux[T, K],
      gen: Generic.Aux[T, V],
      zip: coproduct.ZipWithKeys.Aux[K, V, R],
      ev: R <:< V
    ): Aux[T, R] =
    new LabelledGeneric[T] {
      type Repr = R
      def to(t: T): Repr = zip(gen.to(t))
      def from(r: Repr): T = gen.from(r)
    }
}

class nonGeneric extends StaticAnnotation

trait IsTuple[T]

object IsTuple {
  implicit def apply[T]: IsTuple[T] = macro GenericMacros.mkIsTuple[T]
}

trait HasProductGeneric[T]

object HasProductGeneric {
  implicit def apply[T]: HasProductGeneric[T] = macro GenericMacros.mkHasProductGeneric[T]
}

trait HasCoproductGeneric[T]

object HasCoproductGeneric {
  implicit def apply[T]: HasCoproductGeneric[T] = macro GenericMacros.mkHasCoproductGeneric[T]
}

trait ReprTypes {
  val c: blackbox.Context
  import c.universe._

  def hlistTpe = typeOf[HList]
  def hnilTpe = typeOf[HNil]
  def hconsTpe = typeOf[::[_, _]].typeConstructor
  def coproductTpe = typeOf[Coproduct]
  def cnilTpe = typeOf[CNil]
  def cconsTpe = typeOf[:+:[_, _]].typeConstructor

  def atatTpe = typeOf[tag.@@[_,_]].typeConstructor
  def fieldTypeTpe = typeOf[shapeless.labelled.FieldType[_, _]].typeConstructor
}

trait CaseClassMacros extends ReprTypes {
  val c: whitebox.Context

  import c.universe._
  import internal.constantType
  import Flag._

  def abort(msg: String) =
    c.abort(c.enclosingPosition, msg)

  def isReprType(tpe: Type): Boolean =
    tpe <:< hlistTpe || tpe <:< coproductTpe

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

  def ownerChain(sym: Symbol): List[Symbol] = {
    @tailrec
    def loop(sym: Symbol, acc: List[Symbol]): List[Symbol] =
      if(sym.owner == NoSymbol) acc
      else loop(sym.owner, sym :: acc)

    loop(sym, Nil)
  }

  def mkDependentRef(prefix: Type, path: List[Name]): (Type, Symbol) = {
    val (_, pre, sym) =
      path.foldLeft((prefix, NoType, NoSymbol)) {
        case ((pre, _, sym), nme) =>
          val sym0 = pre.member(nme)
          val pre0 = sym0.typeSignature
          (pre0, pre, sym0)
      }
    (pre, sym)
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

      val tpePrefix = prefix(tpe)

      ctors map { sym =>
        val suffix = ownerChain(sym).dropWhile(_ != tpePrefix.typeSymbol)
        if(suffix.isEmpty) {
          if(sym.isModuleClass) {
            val moduleSym = sym.asClass.module
            val modulePre = prefix(moduleSym.typeSignature)
            c.internal.singleType(modulePre, moduleSym)
          } else {
            val subTpeSym = sym.asType
            val subTpePre = prefix(subTpeSym.typeSignature)
            c.internal.typeRef(subTpePre, subTpeSym, baseTpe.args)
          }
        } else {
          if(sym.isModuleClass) {
            val path = suffix.tail.map(_.name.toTermName)
            val (modulePre, moduleSym) = mkDependentRef(tpePrefix, path)
            c.internal.singleType(modulePre, moduleSym)
          } else {
            val path = suffix.tail.init.map(_.name.toTermName) :+ suffix.last.name.toTypeName
            val (subTpePre, subTpeSym) = mkDependentRef(tpePrefix, path)
            c.internal.typeRef(subTpePre, subTpeSym, baseTpe.args)
          }
        }
      }
    }
    else
      abort(s"$tpe is not a case class, case class-like, a sealed trait or Unit")
  }

  def nameAsString(name: Name): String = name.decodedName.toString.trim

  def nameAsValue(name: Name): Constant = Constant(nameAsString(name))

  def nameOf(tpe: Type) = tpe.typeSymbol.name

  def mkCompoundTpe(nil: Type, cons: Type, items: List[Type]): Type =
    items.foldRight(nil) { case (tpe, acc) => appliedType(cons, List(tpe, acc)) }

  def mkLabelTpe(name: Name): Type =
    appliedType(atatTpe, List(typeOf[scala.Symbol], constantType(nameAsValue(name))))

  def mkFieldTpe(name: Name, valueTpe: Type): Type = {
    appliedType(fieldTypeTpe, List(mkLabelTpe(name), valueTpe))
  }

  def mkHListTpe(items: List[Type]): Type =
    mkCompoundTpe(hnilTpe, hconsTpe, items)

  def mkCoproductTpe(items: List[Type]): Type =
    mkCompoundTpe(cnilTpe, cconsTpe, items)

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

  def reprTpe(tpe: Type): Type = {
    if(isProduct(tpe)) mkHListTpe(fieldsOf(tpe).map(_._2))
    else mkCoproductTpe(ctorsOf(tpe))
  }

  def isCaseClassLike(sym: ClassSymbol): Boolean =
    sym.isCaseClass ||
    (!sym.isAbstract && !sym.isTrait && sym.knownDirectSubclasses.isEmpty && fieldsOf(sym.typeSignature).nonEmpty)

  def isCaseObjectLike(sym: ClassSymbol): Boolean = sym.isModuleClass && isCaseClassLike(sym)

  def isCaseAccessorLike(sym: TermSymbol): Boolean =
    !isNonGeneric(sym) && sym.isPublic && (if(sym.owner.asClass.isCaseClass) sym.isCaseAccessor else sym.isAccessor)

  def isSealedHierarchyClassSymbol(symbol: ClassSymbol): Boolean = {
    def helper(classSym: ClassSymbol): Boolean = {
      classSym.knownDirectSubclasses.toList forall { child0 =>
        val child = child0.asClass
        child.typeSignature // Workaround for <https://issues.scala-lang.org/browse/SI-7755>

        isCaseClassLike(child) || (child.isSealed && helper(child))
      }
    }

    symbol.isSealed && helper(symbol)
  }

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

  def prefix(tpe: Type): Type = {
    val global = c.universe.asInstanceOf[scala.tools.nsc.Global]
    val gTpe = tpe.asInstanceOf[global.Type]
    gTpe.prefix.asInstanceOf[Type]
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

  def isTuple(tpe: Type): Boolean =
    tpe <:< typeOf[Unit] || definitions.TupleClass.seq.contains(tpe.typeSymbol)
}

class GenericMacros(val c: whitebox.Context) extends CaseClassMacros {
  import c.universe._
  import internal.constantType
  import Flag._

  def materialize[T: WeakTypeTag, R: WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[T]
    if(isReprType(tpe))
      abort("No Generic instance available for HList or Coproduct")

    def mkCoproductCases(tpe: Type, index: Int): (CaseDef, CaseDef) = {
      val name = TermName(c.freshName("pat"))

      def mkCoproductValue(tree: Tree): Tree =
        (0 until index).foldLeft(q"_root_.shapeless.Inl($tree)": Tree) {
          case (acc, _) => q"_root_.shapeless.Inr($acc)"
        }

      val body = mkCoproductValue(q"$name: $tpe")
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

    val (toCases, fromCases) =
      if(isProduct(tpe)) {
        val (to, from) = mkProductCases(tpe)
        (List(to), List(from))
      } else {
        val (to, from) = (ctorsOf(tpe) zip (Stream from 0) map (mkCoproductCases _).tupled).unzip
        (to, from :+ cq"_ => _root_.scala.Predef.???")
      }

    val clsName = TypeName(c.freshName())
    q"""
      final class $clsName extends Generic[$tpe] {
        type Repr = ${reprTpe(tpe)}
        def to(p: $tpe): Repr = p match { case ..$toCases }
        def from(p: Repr): $tpe = p match { case ..$fromCases }
      }
      new $clsName()
    """
  }

  def mkIsTuple[T: WeakTypeTag]: Tree = {
    val tTpe = weakTypeOf[T]
    if(!isTuple(tTpe))
      abort(s"Unable to materialize IsTuple for non-tuple type $tTpe")

    q"""new IsTuple[$tTpe] {}"""
  }

  def mkHasProductGeneric[T: WeakTypeTag]: Tree = {
    val tTpe = weakTypeOf[T]
    if(isReprType(tTpe) || !isProduct(tTpe))
      abort(s"Unable to materialize HasProductGeneric for $tTpe")

    q"""new HasProductGeneric[$tTpe] {}"""
  }

  def mkHasCoproductGeneric[T: WeakTypeTag]: Tree = {
    val tTpe = weakTypeOf[T]
    if(isReprType(tTpe) || !isCoproduct(tTpe))
      abort(s"Unable to materialize HasCoproductGeneric for $tTpe")

    q"""new HasCoproductGeneric[$tTpe] {}"""
  }
}
