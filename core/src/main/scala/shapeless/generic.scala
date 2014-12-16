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

import scala.annotation.StaticAnnotation
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

class GenericMacros(val c: whitebox.Context) {
  import c.universe._

  def materialize[T: WeakTypeTag, R: WeakTypeTag] =
    materializeAux(false, weakTypeOf[T], weakTypeOf[R])

  def materializeLabelled[T: WeakTypeTag, R: WeakTypeTag] =
    materializeAux(true, weakTypeOf[T], weakTypeOf[R])

  def materializeAux(labelled: Boolean, tpe: Type, rTpe: Type): Tree = {
    import c.{ abort, enclosingPosition, typeOf }

    val helper = new Helper(tpe, false, labelled, labelled)
    if (tpe <:< typeOf[HList] || tpe <:< typeOf[Coproduct])
      helper.materializeIdentityGeneric
    else
      helper.materializeGeneric
  }

  def deriveProductInstance[C[_], T](ev: Tree)(implicit tTag: WeakTypeTag[T], cTag: WeakTypeTag[C[Any]]) =
    deriveInstanceAux(ev, true, false, tTag.tpe, cTag.tpe)

  def deriveLabelledProductInstance[C[_], T](ev: Tree)(implicit tTag: WeakTypeTag[T], cTag: WeakTypeTag[C[Any]]) =
    deriveInstanceAux(ev, true, true, tTag.tpe, cTag.tpe)

  def deriveInstance[C[_], T](ev: Tree)(implicit tTag: WeakTypeTag[T], cTag: WeakTypeTag[C[Any]]) =
    deriveInstanceAux(ev, false, false, tTag.tpe, cTag.tpe)

  def deriveLabelledInstance[C[_], T](ev: Tree)(implicit tTag: WeakTypeTag[T], cTag: WeakTypeTag[C[Any]]) =
    deriveInstanceAux(ev, false, true, tTag.tpe, cTag.tpe)

  def deriveInstanceAux(deriver: Tree, product: Boolean, labelled: Boolean, tTpe: Type, cTpe: Type): Tree = {
    val helper = new Helper(tTpe, product, labelled, false)
    helper.deriveInstance(deriver, cTpe.typeConstructor)
  }

  class Helper(val fromTpe: Type, val toProduct: Boolean, val toLabelled: Boolean, val labelledRepr: Boolean) {
    import internal.constantType
    import Flag._

    def absurdValueTree = reify { ??? }.tree
    def hconsValueTree = reify {  ::  }.tree
    def hnilValueTree  = reify { HNil }.tree
    def inlValueTree = reify {  Inl  }.tree
    def inrValueTree  = reify { Inr }.tree

    def anyRefTpe = typeOf[AnyRef]
    def unitTpe = typeOf[Unit]
    def hconsTpe = typeOf[::[_, _]].typeConstructor
    def hnilTpe = typeOf[HNil]
    def cconsTpe = typeOf[:+:[_, _]].typeConstructor
    def cnilTpe = typeOf[CNil]
    def atatTpe = typeOf[tag.@@[_,_]].typeConstructor
    def symTpe = typeOf[scala.Symbol]
    def fieldTypeTpe = typeOf[shapeless.labelled.FieldType[_, _]].typeConstructor
    def genericTpe = typeOf[Generic[_]].typeConstructor
    def labelledGenericTpe = typeOf[LabelledGeneric[_]].typeConstructor
    def typeClassTpe = typeOf[TypeClass[Any]].typeConstructor
    def labelledTypeClassTpe = typeOf[LabelledTypeClass[Any]].typeConstructor
    def productTypeClassTpe = typeOf[ProductTypeClass[Any]].typeConstructor
    def labelledProductTypeClassTpe = typeOf[LabelledProductTypeClass[Any]].typeConstructor
    def deriveCtorsTpe = typeOf[DeriveConstructors]

    def toName = TermName("to")
    def fromName = TermName("from")
    def reprName = TypeName("Repr")
    def applyName = TermName("apply")
    def unapplyName = TermName("unapply")

    def nameAsValue(name: Name): Constant = Constant(name.decodedName.toString.trim)

    def nameAsLiteral(name: Name): Tree = c.universe.Literal(nameAsValue(name))

    def nameOf(tpe: Type) = tpe.typeSymbol.name

    def fieldsOf(tpe: Type): List[(TermName, Type)] =
      tpe.decls.toList collect {
        case sym: TermSymbol if isCaseAccessorLike(sym) => (sym.name, sym.typeSignatureIn(tpe).finalResultType)
      }

    def reprOf(tpe: Type): Type = {
      val fields = fieldsOf(tpe)
      if(labelledRepr)
        mkRecordTpe(fields)
      else
        mkHListTpe(fields.map(_._2))
    }

    // See https://github.com/milessabin/shapeless/issues/212
    def companionRef(tpe: Type): Tree = {
      val global = c.universe.asInstanceOf[scala.tools.nsc.Global]
      val gTpe = tpe.asInstanceOf[global.Type]
      val pre = gTpe.prefix
      val sym = gTpe.typeSymbol.companionSymbol
      global.gen.mkAttributedRef(pre, sym).asInstanceOf[Tree]
    }

    def mkCompoundTpe(nil: Type, cons: Type, items: List[Type]): Type =
      items.foldRight(nil) { case (tpe, acc) => appliedType(cons, List(tpe, acc)) }

    def mkFieldTpe(name: Name, valueTpe: Type): Type = {
      val keyTpe = appliedType(atatTpe, List(symTpe, constantType(nameAsValue(name))))
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

    lazy val fromSym = {
      val sym = fromTpe.typeSymbol
      if (!sym.isClass)
        abort(s"$sym is not a class or trait")

      val fromSym0 = sym.asClass
      fromSym0.typeSignature // Workaround for <https://issues.scala-lang.org/browse/SI-7755>

      fromSym0
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

    def isCaseClassLike(sym: ClassSymbol): Boolean =
      sym.isCaseClass ||
      (!sym.isAbstract && !sym.isTrait && sym.knownDirectSubclasses.isEmpty && fieldsOf(sym.typeSignature).nonEmpty)

    def isCaseAccessorLike(sym: TermSymbol): Boolean =
      !isNonGeneric(sym) && sym.isPublic && (if(sym.owner.asClass.isCaseClass) sym.isCaseAccessor else sym.isAccessor)

    lazy val fromProduct = fromTpe =:= unitTpe || isCaseClassLike(fromSym)

    lazy val fromCtors = {
      def collectCtors(classSym: ClassSymbol): List[ClassSymbol] = {
        classSym.knownDirectSubclasses.toList flatMap { child0 =>
          val child = child0.asClass
          child.info // Workaround for <https://issues.scala-lang.org/browse/SI-7755>
          if (isCaseClassLike(child))
            List(child)
          else if (child.isSealed)
            collectCtors(child)
          else
            abort(s"$child is not case class like or a sealed trait")
        }
      }

      if(fromProduct)
        List(fromTpe)
      else if (fromSym.isSealed) { // multiple ctors
        if (toProduct) abort(s"Cannot derive a ProductTypeClass for non-Product trait $fromTpe")
        val ctors = collectCtors(fromSym).sortBy(_.fullName)
        if (ctors.isEmpty) abort(s"Sealed trait $fromTpe has no case class subtypes")

        // We're using an extremely optimistic strategy here, basically ignoring
        // the existence of any existential types.
        val baseTpe: TypeRef = fromTpe.dealias match {
          case tr: TypeRef => tr
          case _ => abort(s"bad type $fromTpe")
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
        abort(s"$fromSym is not a case class, a sealed trait or Unit")
    }

    def abort(msg: String) =
      c.abort(c.enclosingPosition, msg)

    def mkElem(elem: Tree, name: Name, tpe: Type): Tree =
      if(labelledRepr) q"$elem.asInstanceOf[${mkFieldTpe(name, tpe)}]" else elem

    type ProductCaseFn = Type => CaseDef
    type CaseFn = (Type, Int) => CaseDef

    def mkProductCases(toRepr: ProductCaseFn, fromRepr: ProductCaseFn): (List[CaseDef], List[CaseDef]) =
      (List(toRepr(fromTpe)), List(fromRepr(fromTpe)))

    def mkCases(toRepr: CaseFn, fromRepr: CaseFn): (List[CaseDef], List[CaseDef]) = {
      val to = fromCtors zip (Stream from 0) map toRepr.tupled
      val from = fromCtors zip (Stream from 0) map fromRepr.tupled
      (to, from :+ cq"_ => $absurdValueTree")
    }

    def mkCoproductValue(tree: Tree, index: Int): Tree =
      (0 until index).foldLeft(q"$inlValueTree($tree)": Tree) { case (acc, _) => q"$inrValueTree($acc)" }

    def mkToCoproductCase(tpe: Type, index: Int): CaseDef = {
      val name = TermName(c.freshName("pat"))
      val body = mkCoproductValue(mkElem(q"$name", nameOf(tpe), tpe), index)
      cq"$name: $tpe => $body"
    }

    def mkFromCoproductCase(tpe: Type, index: Int): CaseDef = {
      val name = TermName(c.freshName("pat"))
      val pat = mkCoproductValue(pq"$name", index)
      cq"$pat => $name"
    }

    def mkToReprCase(tpe: Type): CaseDef = mkToReprCase(tpe, -1)

    def mkToReprCase(tpe: Type, index: Int): CaseDef = {
      def mkCase(lhs: Tree, rhs: Tree) =
        if(index == -1)
          cq"$lhs => $rhs"
        else
          cq"$lhs => ${mkCoproductValue(mkElem(rhs, nameOf(tpe), tpe), index)}"

      if(tpe =:= unitTpe)
        mkCase(pq"()", hnilValueTree)
      else {
        val sym = tpe.typeSymbol
        val isCaseClass = sym.asClass.isCaseClass
        val hasUnapply = {
          val unapplySym = sym.companion.typeSignature.member(unapplyName)
          unapplySym != NoSymbol && !isNonGeneric(unapplySym)
        }

        if(isCaseClass || hasUnapply) {
          val binders = fieldsOf(tpe).map { case (name, tpe) => (TermName(c.freshName("pat")), name, tpe) }
          val lhs = pq"${companionRef(tpe)}(..${binders.map(x => pq"${x._1}")})"
          val rhs = 
            binders.foldRight(hnilValueTree) {
              case ((bound, name, tpe), acc) => q"$hconsValueTree(${mkElem(q"$bound", name, tpe)}, $acc)"
            }
          mkCase(lhs, rhs)
        } else {
          val lhs = TermName(c.freshName("pat"))
          val rhs =
            fieldsOf(tpe).foldRight(hnilValueTree) {
              case ((name, tpe), acc) =>
                val elem = mkElem(q"$lhs.$name", name, tpe)
                q"$hconsValueTree($elem, $acc)"
            }
          mkCase(pq"$lhs", rhs)
        }
      }
    }

    def mkFromReprCase(tpe: Type): CaseDef = mkFromReprCase(tpe, -1)

    def mkFromReprCase(tpe: Type, index: Int = -1): CaseDef = {
      def mkCase(lhs: Tree, rhs: Tree) =
        if(index == -1)
          cq"$lhs => $rhs"
        else
          cq"${mkCoproductValue(lhs, index)} => $rhs"

      if(tpe =:= unitTpe)
        mkCase(hnilValueTree, q"()")
      else {
        val sym = tpe.typeSymbol
        val isCaseClass = sym.asClass.isCaseClass
        val hasApply = {
          val applySym = sym.companion.typeSignature.member(applyName)
          applySym != NoSymbol && !isNonGeneric(applySym)
        }

        val binders = fieldsOf(tpe).map { case (name, tpe) => (TermName(c.freshName("pat")), name, tpe) } 
        val ctorArgs = binders.map { case (bound, name, tpe) => mkElem(Ident(bound), name, tpe) }

        val lhs =
          binders.foldRight(hnilValueTree) {
            case ((bound, _, _), acc) => pq"$hconsValueTree($bound, $acc)"
          }

        val rhs =
          if(isCaseClass || hasApply)
            q"${companionRef(tpe)}(..$ctorArgs)"
          else
            q"new $tpe(..$ctorArgs)"

        mkCase(lhs, rhs)
      }
    }

    def materializeGeneric = {
      val genericTypeConstructor: Type = if(toLabelled) labelledGenericTpe else genericTpe

      val reprTpe =
        if(fromProduct) reprOf(fromTpe)
        else if(toLabelled) {
          val labelledCases = fromCtors.map(tpe => (nameOf(tpe).toTermName, tpe))
          mkUnionTpe(labelledCases)
        } else
          mkCoproductTpe(fromCtors)

      val (toCases, fromCases) =
        if(fromProduct) mkProductCases(mkToReprCase, mkFromReprCase)
        else mkCases(mkToCoproductCase, mkFromCoproductCase)

      val clsName = TypeName(c.freshName())
      q"""
        final class $clsName extends ${genericTypeConstructor.typeSymbol}[$fromTpe] {
          type $reprName = $reprTpe
          def $toName(p: $fromTpe): $reprTpe = p match { case ..$toCases }
          def $fromName(p: $reprTpe): $fromTpe = p match { case ..$fromCases }
        }
        new $clsName()
      """
    }

    def materializeIdentityGeneric = {
      val clsName = TypeName(c.freshName())
      q"""
        final class $clsName extends ${genericTpe.typeSymbol}[$fromTpe] {
          type $reprName = $fromTpe
          def $toName(p: $fromTpe): $fromTpe = p
          def $fromName(p: $fromTpe): $fromTpe = p
        }
        new $clsName()
      """
    }

    def deriveInstance(deriver: Tree, tc: Type): Tree = {
      fromSym.baseClasses.find(sym => sym != fromSym && sym.isClass && sym.asClass.isSealed) match {
        case Some(sym) if c.inferImplicitValue(deriveCtorsTpe) == EmptyTree =>
          val msg =
            s"Attempting to derive a type class instance for class `${fromSym.name.decodedName.toString}` with "+
            s"sealed superclass `${sym.name.decodedName.toString}`; this is most likely unintended. To silence "+
            s"this warning, import `TypeClass.deriveConstructors`"

          if (c.compilerSettings contains "-Xfatal-warnings")
            c.error(c.enclosingPosition, msg)
          else
            c.warning(c.enclosingPosition, msg)
        case _ =>
      }

      val elemTpes: List[Type] = fromCtors.flatMap(fieldsOf(_).map(_._2)).filterNot(fromTpe =:= _).distinct
      val elemInstanceNames = List.fill(elemTpes.length)(TermName(c.freshName("inst")))
      val elemInstanceMap = (elemTpes zip elemInstanceNames).toMap
      val elemInstanceDecls = (elemInstanceMap map { case (tpe, name) =>
        val appTpe = tq"${tc.typeSymbol}[$tpe]"
        q"lazy val $name: $appTpe = ${definitions.PredefModule}.implicitly[$appTpe]"
      }).toList

      val tpeInstanceName = TermName(c.freshName())
      val instanceMap = elemInstanceMap.mapValues(Ident(_)) + (fromTpe -> q"$tpeInstanceName")

      val reprInstance = {
        def mkCompoundValue(nil: Tree, cons: Tree, items: List[(Name, Tree)]): Tree =
          items.foldRight(nil) { case ((name, instance), acc) =>
            Apply(
              cons,
              (if(toLabelled) List(nameAsLiteral(name)) else Nil) ++ List(instance, acc)
            )
          }

        def mkInstance(tpe: Type): Tree =
          mkCompoundValue(
            q"$deriver.emptyProduct", q"$deriver.product",
            fieldsOf(tpe).map { case (name, tpe) => (name, instanceMap(tpe)) }
          )

        if(toProduct)
          mkInstance(fromTpe)
        else
          mkCompoundValue(
            q"$deriver.emptyCoproduct", q"$deriver.coproduct",
            fromCtors.map { tpe => (tpe.typeSymbol.name, mkInstance(tpe)) }
          )
      }

      val reprTpe =
        if(toProduct)
          reprOf(fromTpe)
        else
          mkCoproductTpe(fromCtors.map(reprOf))

      val (toCases, fromCases) =
        if(toProduct) mkProductCases(mkToReprCase, mkFromReprCase)
        else mkCases(mkToReprCase, mkFromReprCase)

      val objName, reprName, toName, fromName = TermName(c.freshName())
      q"""
        object $objName {
          def $toName(p: $fromTpe): $reprTpe = p match { case ..$toCases }
          def $fromName(p: $reprTpe): $fromTpe = p match { case ..$fromCases }
          ..$elemInstanceDecls
          lazy val $reprName: ${tc.typeSymbol}[$reprTpe] = $reprInstance
          lazy val $tpeInstanceName: ${tc.typeSymbol}[$fromTpe] = $deriver.project($reprName, $toName, $fromName)
        }
        $objName.$tpeInstanceName
      """
    }
  }
}
