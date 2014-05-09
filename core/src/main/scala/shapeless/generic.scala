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

import scala.collection.breakOut
import scala.collection.immutable.ListMap
import scala.reflect.macros.Context

trait Generic[T] {
  type Repr
  def to(t : T) : Repr
  def from(r : Repr) : T
}

trait LowPriorityGeneric {
  implicit def apply[T]: Generic[T] = macro GenericMacros.materialize[T]
  // Refinement for products, here we can provide the calling context with
  // a proof that the resulting Repr <: HList
  implicit def product[T <: Product]: Generic[T] = macro GenericMacros.materializeForProduct[T]

}

object Generic extends LowPriorityGeneric {
  type Aux[T, Repr0] = Generic[T] { type Repr = Repr0 }

  // Refinement for coproducts, here we can provide the calling context with
  // a proof that the resulting Repr <: Coproduct
  implicit def coproduct[T <: Coproduct]: Generic[T] = macro GenericMacros.materializeForCoproduct[T]
}

trait LabelledGeneric[T] {
  type Repr
  def to(t : T) : Repr
  def from(r : Repr) : T
}

trait LowPriorityLabelledGeneric {
  implicit def apply[T]: LabelledGeneric[T] = macro GenericMacros.materializeLabelled[T]
}

object LabelledGeneric extends LowPriorityLabelledGeneric {
  // Refinement for products, here we can provide the calling context with
  // a proof that the resulting Repr is a record
  type Aux[T, Out0] = LabelledGeneric[T]{ type Repr = Out0 }

  implicit def product[T <: Product]: LabelledGeneric[T] = macro GenericMacros.materializeLabelledForProduct[T]
}

object GenericMacros {
  import shapeless.record.FieldType

  def materialize[T]
    (c: Context)(implicit tT: c.WeakTypeTag[T]): c.Expr[Generic[T]] =
      materializeAux[Generic[T]](c)(false, false, tT.tpe)

  def materializeForProduct[T <: Product]
    (c: Context)(implicit tT: c.WeakTypeTag[T]): c.Expr[Generic[T] { type Repr <: HList }] =
      materializeAux[Generic[T] { type Repr <: HList }](c)(true, false, tT.tpe)

  def materializeForCoproduct[T <: Coproduct]
    (c: Context)(implicit tT: c.WeakTypeTag[T]): c.Expr[Generic[T] { type Repr <: Coproduct}] =
    materializeAux[Generic[T] { type Repr <: Coproduct}](c)(true, false, tT.tpe)

  def materializeLabelled[T]
    (c: Context)(implicit tT: c.WeakTypeTag[T]): c.Expr[LabelledGeneric[T]] =
      materializeAux[LabelledGeneric[T]](c)(false, true, tT.tpe)

  def materializeLabelledForProduct[T <: Product]
    (c: Context)(implicit tT: c.WeakTypeTag[T]): c.Expr[LabelledGeneric[T] { type Repr <: HList }] =
      materializeAux[LabelledGeneric[T] { type Repr <: HList }](c)(true, true, tT.tpe)

  def materializeAux[G]
    (c0 : Context)(product0: Boolean, labelled0: Boolean, tpe0: c0.Type): c0.Expr[G] = {
    import c0.{ abort, enclosingPosition, typeOf, Expr }

    if (product0 && tpe0 <:< typeOf[Coproduct])
      abort(enclosingPosition, s"Cannot materialize Coproduct $tpe0 as a Product")

    val helper = new Helper[c0.type] {
      val c: c0.type = c0
      val fromTpe = tpe0
      val toProduct = product0
      val toLabelled = labelled0
      val labelledRepr = labelled0
    }

    Expr[G] {
      if (tpe0 <:< typeOf[HList] || tpe0 <:< typeOf[Coproduct])
        helper.materializeIdentityGeneric
      else
        helper.materializeGeneric
    }
  }

  def deriveProductInstance[C[_], T]
    (c: Context)(ev: c.Expr[_])(implicit tTag: c.WeakTypeTag[T], cTag: c.WeakTypeTag[C[Any]]): c.Expr[C[T]] =
    deriveInstanceAux(c)(ev.tree, true, false, tTag, cTag)

  def deriveLabelledProductInstance[C[_], T]
    (c: Context)(ev: c.Expr[_])(implicit tTag: c.WeakTypeTag[T], cTag: c.WeakTypeTag[C[Any]]): c.Expr[C[T]] =
    deriveInstanceAux(c)(ev.tree, true, true, tTag, cTag)

  def deriveInstance[C[_], T]
    (c: Context)(ev: c.Expr[_])(implicit tTag: c.WeakTypeTag[T], cTag: c.WeakTypeTag[C[Any]]): c.Expr[C[T]] =
    deriveInstanceAux(c)(ev.tree, false, false, tTag, cTag)

  def deriveLabelledInstance[C[_], T]
    (c: Context)(ev: c.Expr[_])(implicit tTag: c.WeakTypeTag[T], cTag: c.WeakTypeTag[C[Any]]): c.Expr[C[T]] =
    deriveInstanceAux(c)(ev.tree, false, true, tTag, cTag)

  def deriveInstanceAux[C[_], T](c0: Context)
    (deriver: c0.Tree, product0: Boolean, labelled0: Boolean, tTag: c0.WeakTypeTag[T], cTag: c0.WeakTypeTag[C[Any]]): c0.Expr[C[T]] = {
    import c0.Expr
    val helper = new Helper[c0.type] {
      val c: c0.type = c0
      val fromTpe = tTag.tpe
      val toProduct = product0
      val toLabelled = labelled0
      val labelledRepr = false
    }

    Expr[C[T]] {
      helper.deriveInstance(deriver, cTag.tpe.typeConstructor)
    }
  }

  trait Helper[+C <: Context] {
    val c: C
    val fromTpe: c.Type
    val toProduct: Boolean
    val toLabelled: Boolean
    val labelledRepr: Boolean

    import c.universe._
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
    def fieldTypeTpe = typeOf[shapeless.record.FieldType[_, _]].typeConstructor
    def genericTpe = typeOf[Generic[_]].typeConstructor
    def labelledGenericTpe = typeOf[LabelledGeneric[_]].typeConstructor
    def typeClassTpe = typeOf[TypeClass[Any]].typeConstructor
    def labelledTypeClassTpe = typeOf[LabelledTypeClass[Any]].typeConstructor
    def productTypeClassTpe = typeOf[ProductTypeClass[Any]].typeConstructor
    def labelledProductTypeClassTpe = typeOf[LabelledProductTypeClass[Any]].typeConstructor
    def deriveCtorsTpe = typeOf[DeriveConstructors]

    def toName = newTermName("to")
    def fromName = newTermName("from")
    def reprName = newTypeName("Repr")

    def nameAsValue(name: Name): Constant = Constant(name.decoded.trim)

    def nameAsLiteral(name: Name): Tree = Literal(nameAsValue(name))

    def nameOf(tpe: Type) = tpe.typeSymbol.name

    def fieldsOf(tpe: Type): List[(TermName, Type)] =
      tpe.declarations.toList collect {
        case sym: TermSymbol if sym.isVal && sym.isCaseAccessor => (sym.name.toTermName, sym.typeSignatureIn(tpe))
      }

    def reprOf(tpe: Type): Type = {
      val fields = fieldsOf(tpe)
      if(labelledRepr)
        mkRecordTpe(fields)
      else
        mkHListTpe(fields.map(_._2))
    }

    def mkCompoundTpe(nil: Type, cons: Type, items: List[Type]): Type =
      items.foldRight(nil) { case (tpe, acc) => appliedType(cons, List(tpe, acc)) }

    def mkFieldTpe(name: Name, valueTpe: Type): Type = {
      val keyTpe = appliedType(atatTpe, List(symTpe, ConstantType(nameAsValue(name))))
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

    lazy val fromProduct = fromTpe =:= unitTpe || fromSym.isCaseClass

    lazy val fromCtors = {
      def collectCtors(classSym: ClassSymbol): List[ClassSymbol] = {
        classSym.knownDirectSubclasses.toList flatMap { child0 =>
          val child = child0.asClass
          child.typeSignature // Workaround for <https://issues.scala-lang.org/browse/SI-7755>
          if (child.isCaseClass)
            List(child)
          else if (child.isSealed)
            collectCtors(child)
          else
            abort(s"$child is not a case class or a sealed trait")
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
        val baseTpe: TypeRef = fromTpe.normalize match {
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
      val name = newTermName(c.fresh("pat"))
      val body = mkCoproductValue(mkElem(q"$name", nameOf(tpe), tpe), index)
      cq"$name: $tpe => $body"
    }

    def mkFromCoproductCase(tpe: Type, index: Int): CaseDef = {
      val name = newTermName(c.fresh("pat"))
      val pat = mkCoproductValue(pq"$name", index)
      cq"$pat => $name"
    }

    def mkBinder(boundName: TermName, name: TermName, tpe: Type) = pq"$boundName"
    def mkValue(boundName: TermName, name: TermName, tpe: Type) = mkElem(q"$boundName", name, tpe)

    def mkTransCase(
      tpe: Type,
      bindFrom: (TermName, TermName, Type) => Tree,
      bindRepr: (TermName, TermName, Type) => Tree
    )(mkCaseDef: (Tree, Tree) => CaseDef): CaseDef = {
      val boundFields = fieldsOf(tpe).map { case (name, tpe) => (newTermName(c.fresh("pat")), name, tpe) }

      val fromTree =
        if(tpe =:= unitTpe) q"()"
        else q"${tpe.typeSymbol.companionSymbol.asTerm}(..${boundFields.map(bindFrom.tupled)})"

      val reprTree =
        boundFields.foldRight(hnilValueTree) {
          case (bf, acc) => q"$hconsValueTree(${bindRepr.tupled(bf)}, $acc)"
        }

      mkCaseDef(fromTree, reprTree)
    }

    def mkToProductReprCase(tpe: Type): CaseDef =
      mkTransCase(tpe, mkBinder, mkValue) { case (lhs, rhs) => cq"$lhs => $rhs" }

    def mkFromProductReprCase(tpe: Type): CaseDef =
      mkTransCase(tpe, mkValue, mkBinder) { case (rhs, lhs) => cq"$lhs => $rhs" }

    def mkToReprCase(tpe: Type, index: Int): CaseDef =
      mkTransCase(tpe, mkBinder, mkValue) { case (lhs, rhs) =>
        cq"$lhs => ${mkCoproductValue(mkElem(rhs, nameOf(tpe), tpe), index)}"
      }

    def mkFromReprCase(tpe: Type, index: Int): CaseDef =
      mkTransCase(tpe, mkValue, mkBinder) { case (rhs, lhs) =>
        cq"${mkCoproductValue(lhs, index)} => $rhs"
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
        if(fromProduct) mkProductCases(mkToProductReprCase, mkFromProductReprCase)
        else mkCases(mkToCoproductCase, mkFromCoproductCase)

      val clsName = newTypeName(c.fresh())
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
      val clsName = newTypeName(c.fresh())
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
            s"Attempting to derive a type class instance for class `${fromSym.name.decoded}` with "+
            s"sealed superclass `${sym.name.decoded}`; this is most likely unintended. To silence "+
            s"this warning, import `TypeClass.deriveConstructors`"

          if (c.compilerSettings contains "-Xfatal-warnings")
            c.error(c.enclosingPosition, msg)
          else
            c.warning(c.enclosingPosition, msg)
        case _ =>
      }

      val elemTpes: List[Type] = fromCtors.flatMap(fieldsOf(_).map(_._2)).filterNot(fromTpe =:= _).distinct
      val elemInstanceNames = List.fill(elemTpes.length)(newTermName(c.fresh("inst")))
      val elemInstanceMap = (elemTpes zip elemInstanceNames).toMap
      val elemInstanceDecls = (elemInstanceMap map { case (tpe, name) =>
        val appTpe = tq"${tc.typeSymbol}[$tpe]"
        q"lazy val $name: $appTpe = ${definitions.PredefModule}.implicitly[$appTpe]"
      }).toList

      val tpeInstanceName = newTermName(c.fresh())
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
        if(toProduct) mkProductCases(mkToProductReprCase, mkFromProductReprCase)
        else mkCases(mkToReprCase, mkFromReprCase)

      val objName, reprName, toName, fromName = newTermName(c.fresh())
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
