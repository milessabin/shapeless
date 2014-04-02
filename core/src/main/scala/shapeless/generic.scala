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
import scala.reflect.macros.{ blackbox, whitebox }

trait Generic[T] {
  type Repr
  def to(t : T) : Repr
  def from(r : Repr) : T
}

trait LowPriorityGeneric {
  implicit def apply[T]: Generic[T] = macro GenericMacros.materialize[T]
}

object Generic extends LowPriorityGeneric {
  type Aux[T, Repr0] = Generic[T] { type Repr = Repr0 }

  // Refinement for products, here we can provide the calling context with
  // a proof that the resulting Repr <: HList
  implicit def product[T <: Product]: Generic[T] = macro GenericMacros.materializeForProduct[T]
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
  implicit def product[T <: Product]: LabelledGeneric[T] = macro GenericMacros.materializeLabelledForProduct[T]
}

object GenericMacros {
  import shapeless.record.FieldType

  def materialize[T]
    (c: whitebox.Context)(implicit tT: c.WeakTypeTag[T]): c.Expr[Generic[T]] =
      materializeAux[Generic[T]](c)(false, false, tT.tpe)

  def materializeForProduct[T <: Product]
    (c: whitebox.Context)(implicit tT: c.WeakTypeTag[T]): c.Expr[Generic[T] { type Repr <: HList }] =
      materializeAux[Generic[T] { type Repr <: HList }](c)(true, false, tT.tpe)

  def materializeLabelled[T]
    (c: whitebox.Context)(implicit tT: c.WeakTypeTag[T]): c.Expr[LabelledGeneric[T]] =
      materializeAux[LabelledGeneric[T]](c)(false, true, tT.tpe)

  def materializeLabelledForProduct[T <: Product]
    (c: whitebox.Context)(implicit tT: c.WeakTypeTag[T]): c.Expr[LabelledGeneric[T] { type Repr <: HList }] =
      materializeAux[LabelledGeneric[T] { type Repr <: HList }](c)(true, true, tT.tpe)

  def materializeAux[G]
    (c0 : whitebox.Context)(product0: Boolean, labelled0: Boolean, tpe0: c0.Type): c0.Expr[G] = {
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
    (c: whitebox.Context)(implicit tTag: c.WeakTypeTag[T], cTag: c.WeakTypeTag[C[Any]]): c.Expr[C[T]] =
    deriveInstanceAux(c)(true, false, tTag, cTag)

  def deriveLabelledProductInstance[C[_], T]
    (c: whitebox.Context)(implicit tTag: c.WeakTypeTag[T], cTag: c.WeakTypeTag[C[Any]]): c.Expr[C[T]] =
    deriveInstanceAux(c)(true, true, tTag, cTag)

  def deriveInstance[C[_], T]
    (c: whitebox.Context)(implicit tTag: c.WeakTypeTag[T], cTag: c.WeakTypeTag[C[Any]]): c.Expr[C[T]] =
    deriveInstanceAux(c)(false, false, tTag, cTag)

  def deriveLabelledInstance[C[_], T]
    (c: whitebox.Context)(implicit tTag: c.WeakTypeTag[T], cTag: c.WeakTypeTag[C[Any]]): c.Expr[C[T]] =
    deriveInstanceAux(c)(false, true, tTag, cTag)

  def deriveInstanceAux[C[_], T](c0: whitebox.Context)
    (product0: Boolean, labelled0: Boolean, tTag: c0.WeakTypeTag[T], cTag: c0.WeakTypeTag[C[Any]]): c0.Expr[C[T]] = {
    import c0.Expr
    val helper = new Helper[c0.type] {
      val c: c0.type = c0
      val fromTpe = tTag.tpe
      val toProduct = product0
      val toLabelled = labelled0
      val labelledRepr = false
    }

    Expr[C[T]] {
      helper.deriveInstance(cTag.tpe.typeConstructor)
    }
  }

  trait Helper[+C <: blackbox.Context] {
    val c: C
    val fromTpe: c.Type
    val toProduct: Boolean
    val toLabelled: Boolean
    val labelledRepr: Boolean

    import c.universe._
    import internal._
    import Flag._

    def unitValueTree = reify { () }.tree
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
    def fieldTypeTpe = typeOf[FieldType[_, _]].typeConstructor
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

    def nameAsValue(name: Name): Constant = Constant(name.decodedName.toString.trim)

    def nameAsLiteral(name: Name): Tree = Literal(nameAsValue(name))

    def nameOf(tpe: Type) = tpe.typeSymbol.name

    def fieldsOf(tpe: Type): List[(Name, Type)] =
      tpe.decls.toList collect {
        case sym: TermSymbol if sym.isVal && sym.isCaseAccessor => (sym.name, sym.typeSignatureIn(tpe))
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
      val keyTpe = appliedType(atatTpe, List(symTpe, constantType(nameAsValue(name))))
      appliedType(fieldTypeTpe, List(keyTpe, valueTpe))
    }

    def mkHListTpe(items: List[Type]): Type =
      mkCompoundTpe(hnilTpe, hconsTpe, items)

    def mkRecordTpe(fields: List[(Name, Type)]): Type =
      mkCompoundTpe(hnilTpe, hconsTpe, fields.map((mkFieldTpe _).tupled))

    def mkCoproductTpe(items: List[Type]): Type =
      mkCompoundTpe(cnilTpe, cconsTpe, items)

    def mkUnionTpe(fields: List[(Name, Type)]): Type =
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
          child.info // Workaround for <https://issues.scala-lang.org/browse/SI-7755>
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
        val baseTpe: TypeRef = fromTpe match {
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

    def mkObjectSelection(defns: List[Tree], member: TermName): Tree = {
      val name = TermName(c.freshName())

      val module =
        ModuleDef(
          Modifiers(),
          name,
          Template(
            List(TypeTree(anyRefTpe)),
            noSelfType,
            mkConstructor :: defns
          )
        )

      Block(
        List(module),
        Select(Ident(name), member)
      )
    }

    def mkClass(parent: Type, defns: List[Tree]): Tree = {
      val name = TypeName(c.freshName())

      val clazz =
        ClassDef(
          Modifiers(FINAL),
          name,
          List(),
          Template(
            List(TypeTree(parent)),
            noSelfType,
            mkConstructor :: defns
          )
        )

      Block(
        List(clazz),
        Apply(Select(New(Ident(name)), termNames.CONSTRUCTOR), List())
      )
    }

    def mkConstructor =
      DefDef(
        Modifiers(),
        termNames.CONSTRUCTOR,
        List(),
        List(List()),
        TypeTree(),
        Block(
          List(Apply(Select(Super(This(typeNames.EMPTY), typeNames.EMPTY), termNames.CONSTRUCTOR), List())),
          Literal(Constant(()))
        )
      )

    def mkElem(elem: Tree, name: Name, tpe: Type): Tree =
      if(labelledRepr)
        TypeApply(Select(elem, TermName("asInstanceOf")), List(TypeTree(mkFieldTpe(name, tpe))))
      else
        elem

    type ProductCaseFn = Type => CaseDef
    type CaseFn = (Type, Int) => CaseDef
    
    def mkProductCases(toRepr: ProductCaseFn, fromRepr: ProductCaseFn): (List[CaseDef], List[CaseDef]) =
      (List(toRepr(fromTpe)), List(fromRepr(fromTpe)))

    def mkCases(toRepr: CaseFn, fromRepr: CaseFn): (List[CaseDef], List[CaseDef]) = {
      val to = fromCtors zip (Stream from 0) map toRepr.tupled
      val from = fromCtors zip (Stream from 0) map fromRepr.tupled
      val redundantCatchAllCase = CaseDef(Ident(termNames.WILDCARD), EmptyTree, absurdValueTree)
      (to, from :+ redundantCatchAllCase)
    }

    def mkTrans(name: TermName, inputTpe: Type, outputTpe: Type, cases: List[CaseDef]): Tree = {
      val param = TermName(c.freshName("param"))

      DefDef(
        Modifiers(),
        name,
        List(),
        List(List(ValDef(Modifiers(PARAM), param, TypeTree(inputTpe), EmptyTree))),
        TypeTree(outputTpe),
        Match(Ident(param), cases)
      )
    }

    def mkCoproductValue(tree: Tree, index: Int): Tree = {
      val inl = Apply(inlValueTree, List(tree))
      (0 until index).foldLeft(inl: Tree) { case (acc, _) =>
        Apply(inrValueTree, List(acc))
      }
    }

    def mkToCoproductCase(tpe: Type, index: Int): CaseDef = {
      val name = TermName(c.freshName("pat"))
      CaseDef(
        Bind(name, Typed(Ident(termNames.WILDCARD), TypeTree(tpe))),
        EmptyTree,
        mkCoproductValue(mkElem(Ident(name), nameOf(tpe), tpe), index)
      )
    }

    def mkFromCoproductCase(tpe: Type, index: Int): CaseDef = {
      val name = TermName(c.freshName("pat"))
      CaseDef(
        mkCoproductValue(Bind(name, Ident(termNames.WILDCARD)), index),
        EmptyTree,
        Ident(name)
      )
    }

    def mkBinder(boundName: Name, name: Name, tpe: Type) = Bind(boundName, Ident(termNames.WILDCARD))
    def mkValue(boundName: Name, name: Name, tpe: Type) = mkElem(Ident(boundName), name, tpe)

    def mkTransCase(
      tpe: Type,
      bindFrom: (Name, Name, Type) => Tree,
      bindRepr: (Name, Name, Type) => Tree
    )(mkCaseDef: (Tree, Tree) => CaseDef): CaseDef = {
      val boundFields = fieldsOf(tpe).map { case (name, tpe) => (TermName(c.freshName("pat")), name, tpe) }

      val fromTree =
        if(tpe =:= unitTpe) unitValueTree
        else Apply(Ident(tpe.typeSymbol.companion.asTerm), boundFields.map(bindFrom.tupled))

      val reprTree =
        boundFields.foldRight(hnilValueTree) {
          case (bf, acc) => Apply(hconsValueTree, List(bindRepr.tupled(bf), acc))
        }

      mkCaseDef(fromTree, reprTree)
    }

    def mkToProductReprCase(tpe: Type): CaseDef =
      mkTransCase(tpe, mkBinder, mkValue) { case (lhs, rhs) => CaseDef(lhs, EmptyTree, rhs) }

    def mkFromProductReprCase(tpe: Type): CaseDef =
      mkTransCase(tpe, mkValue, mkBinder) { case (rhs, lhs) => CaseDef(lhs, EmptyTree, rhs) }

    def mkToReprCase(tpe: Type, index: Int): CaseDef =
      mkTransCase(tpe, mkBinder, mkValue) { case (lhs, rhs) => 
        CaseDef(lhs, EmptyTree, mkCoproductValue(mkElem(rhs, nameOf(tpe), tpe), index))
      }

    def mkFromReprCase(tpe: Type, index: Int): CaseDef =
      mkTransCase(tpe, mkValue, mkBinder) { case (rhs, lhs) =>
        CaseDef(mkCoproductValue(lhs, index), EmptyTree, rhs)
      }

    def materializeGeneric = {
      val genericTypeConstructor: Type = if(toLabelled) labelledGenericTpe else genericTpe

      val reprTpe =
        if(fromProduct) reprOf(fromTpe)
        else if(toLabelled) {
          val labelledCases = fromCtors.map(tpe => (nameOf(tpe), tpe))
          mkUnionTpe(labelledCases)
        } else
          mkCoproductTpe(fromCtors)

      val (toCases, fromCases) = 
        if(fromProduct) mkProductCases(mkToProductReprCase, mkFromProductReprCase)
        else mkCases(mkToCoproductCase, mkFromCoproductCase)

      mkClass(
        appliedType(
          genericTypeConstructor,
          List(fromTpe)
        ),
        List(
          TypeDef(Modifiers(), reprName, List(), TypeTree(reprTpe)),
          mkTrans(toName, fromTpe, reprTpe, toCases),
          mkTrans(fromName, reprTpe, fromTpe, fromCases)
        )
      )
    }

    def materializeIdentityGeneric = {
      def mkIdentityDef(name: TermName) = {
        val param = TermName("t")
        DefDef(
          Modifiers(),
          name,
          List(),
          List(List(ValDef(Modifiers(PARAM), param, TypeTree(fromTpe), EmptyTree))),
          TypeTree(fromTpe),
          Ident(param)
        )
      }

      mkClass(
        appliedType(genericTpe, List(fromTpe)),
        List(
          TypeDef(Modifiers(), reprName, List(), TypeTree(fromTpe)),
          mkIdentityDef(toName),
          mkIdentityDef(fromName)
        )
      )
    }

    def deriveInstance(tc: Type): Tree = {
      fromSym.baseClasses.find(sym => sym != fromSym && sym.isClass && sym.asClass.isSealed) match {
        case Some(sym) if c.inferImplicitValue(deriveCtorsTpe) == EmptyTree =>
          val msg =
            s"Attempting to derive a type class instance for class `${fromSym.name.decodedName.toString}` with "+
            s"sealed superclass `${sym.name.decodedName.toString}`; this is most likely unintended. To silence "+
            s"this warning, import `TypeClass.ignoreParent`"

          if (c.compilerSettings contains "-Xfatal-warnings")
            c.error(c.enclosingPosition, msg)
          else
            c.warning(c.enclosingPosition, msg)
        case _ =>
      }

      val deriverName = TermName(c.freshName("inst"))

      def mkImplicitlyAndAssign(name: TermName, typ: Type): ValDef = {
        def mkImplicitly(typ: Type): Tree =
          TypeApply(
            Select(Ident(definitions.PredefModule), TermName("implicitly")),
            List(TypeTree(typ))
          )

        ValDef(
          Modifiers(LAZY),
          name,
          TypeTree(typ),
          mkImplicitly(typ)
        )
      }

      val elemTpes: List[Type] = fromCtors.flatMap(fieldsOf(_).map(_._2)).filterNot(fromTpe =:= _).distinct
      val elemInstanceNames = List.fill(elemTpes.length)(TermName(c.freshName("inst")))
      val elemInstanceMap = (elemTpes zip elemInstanceNames).toMap
      val elemInstanceDecls = (elemInstanceMap map { case (tpe, name) =>
        mkImplicitlyAndAssign(name, appliedType(tc, List(tpe)))
      }).toList

      val tpeInstanceName = TermName(c.freshName())
      val instanceMap = elemInstanceMap.mapValues(Ident(_)) + (fromTpe -> Ident(tpeInstanceName))

      val deriverTpe: Type = (toProduct, toLabelled) match {
        case (false, false) => typeClassTpe
        case (false, true)  => labelledTypeClassTpe
        case (true, false)  => productTypeClassTpe
        case (true, true)   => labelledProductTypeClassTpe
      }

      val deriverInstanceDecl =
        mkImplicitlyAndAssign(deriverName, appliedType(deriverTpe, List(tc)))

      val reprInstance = {
        val tc = Ident(deriverName)

        val emptyProduct: Tree = Select(tc, TermName("emptyProduct"))
        val product: Tree = Select(tc, TermName("product"))

        val emptyCoproduct: Tree = Select(tc, TermName("emptyCoproduct"))
        val coproduct: Tree = Select(tc, TermName("coproduct"))

        def mkCompoundValue(nil: Tree, cons: Tree, items: List[(Name, Tree)]): Tree =
          items.foldRight(nil) { case ((name, instance), acc) =>
            Apply(
              cons,
              (if(toLabelled) List(nameAsLiteral(name)) else Nil) ++ List(instance, acc)
            )
          }

        def mkInstance(tpe: Type): Tree =
          mkCompoundValue(
            emptyProduct, product,
            fieldsOf(tpe).map { case (name, tpe) => (name, instanceMap(tpe)) }
          )

        if(toProduct)
          mkInstance(fromTpe)
        else
          mkCompoundValue(
            emptyCoproduct, coproduct,
            fromCtors.map { tpe => (tpe.typeSymbol.name, mkInstance(tpe)) }
          )
      }

      val reprTpe =
        if(toProduct)
          reprOf(fromTpe)
        else
          mkCoproductTpe(fromCtors.map(reprOf))

      val reprName = TermName(c.freshName("inst"))
      val reprInstanceDecl =
        ValDef(
          Modifiers(LAZY),
          reprName,
          TypeTree(appliedType(tc, List(reprTpe))),
          reprInstance
        )

      val toName, fromName = TermName(c.freshName())

      val tpeInstanceDecl =
        ValDef(
          Modifiers(LAZY),
          tpeInstanceName,
          TypeTree(appliedType(tc, List(fromTpe))),
          Apply(
            Select(Ident(deriverName), TermName("project")),
            List(Ident(reprName), Ident(toName), Ident(fromName))
          )
        )

      val instanceDecls = deriverInstanceDecl :: elemInstanceDecls ::: List(reprInstanceDecl, tpeInstanceDecl)

      val (toCases, fromCases) =
        if(toProduct) mkProductCases(mkToProductReprCase, mkFromProductReprCase)
        else mkCases(mkToReprCase, mkFromReprCase)

      mkObjectSelection(
        mkTrans(toName, fromTpe, reprTpe, toCases) :: mkTrans(fromName, reprTpe, fromTpe, fromCases) :: instanceDecls,
        tpeInstanceName
      )
    }
  }
}
