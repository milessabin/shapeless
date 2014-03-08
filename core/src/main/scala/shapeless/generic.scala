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
    (ctx: Context)(implicit tT: ctx.WeakTypeTag[T]): ctx.Expr[Generic[T]] =
      materializeAux[Generic[T]](ctx)(false, false, tT.tpe)

  def materializeForProduct[T <: Product]
    (ctx: Context)(implicit tT: ctx.WeakTypeTag[T]): ctx.Expr[Generic[T] { type Repr <: HList }] =
      materializeAux[Generic[T] { type Repr <: HList }](ctx)(true, false, tT.tpe)

  def materializeLabelled[T]
    (ctx: Context)(implicit tT: ctx.WeakTypeTag[T]): ctx.Expr[LabelledGeneric[T]] =
      materializeAux[LabelledGeneric[T]](ctx)(false, true, tT.tpe)

  def materializeLabelledForProduct[T <: Product]
    (ctx: Context)(implicit tT: ctx.WeakTypeTag[T]): ctx.Expr[LabelledGeneric[T] { type Repr <: HList }] =
      materializeAux[LabelledGeneric[T] { type Repr <: HList }](ctx)(true, true, tT.tpe)

  def materializeAux[G]
    (ctx : Context)(product: Boolean, labelled0: Boolean, tpe0: ctx.Type): ctx.Expr[G] = {
    if (product && tpe0 <:< ctx.typeOf[Coproduct])
      ctx.abort(ctx.enclosingPosition, s"Cannot materialize Coproduct $tpe0 as a Product")

    val helper = new Helper[ctx.type] {
      val c: ctx.type = ctx
      val expandInner = false
      val optimizeSingleItem = false
      val checkParent = false
      val tpe = tpe0
      val labelled = labelled0
    }

    ctx.Expr[G] {
      if (tpe0 <:< ctx.typeOf[HList] || tpe0 <:< ctx.typeOf[Coproduct])
        helper.ADT.materializeIdentityGeneric
      else
        helper.ADT.materializeGeneric
    }
  }

  trait Helper[+C <: Context] {

    val c: C
    val expandInner: Boolean
    val optimizeSingleItem: Boolean
    val checkParent: Boolean
    val tpe: c.Type
    val labelled: Boolean

    import c.universe._
    import Flag._


    def ADT: ADT = {
      val sym = tpe.typeSymbol
      if (!sym.isClass)
        exit(s"$sym is not a class or trait")

      val classSym = sym.asClass
      classSym.typeSignature // Workaround for <https://issues.scala-lang.org/browse/SI-7755>

      if (tpe =:= typeOf[Unit])
        ADTSingle(tpe, classSym, UnitADTCase)
      else if (tpe <:< typeOf[HList])
        ADTSingle(tpe, classSym, HListADTCase(tpe))
      else if (tpe <:< typeOf[Coproduct])
        ADTCoproduct(tpe)
      else {
        if (checkParent)
          classSym.baseClasses.find(sym => sym != classSym && sym.isClass && sym.asClass.isSealed) match {
            case Some(sym) if c.inferImplicitValue(typeOf[IgnoreParent]) == EmptyTree =>
              val msg =
                s"Attempting to derive a type class instance for class `${classSym.name.decoded}` with "+
                s"sealed superclass `${sym.name.decoded}`; this is most likely unintended. To silence "+
                s"this warning, import `TypeClass.ignoreParent`"

              if (c.compilerSettings contains "-Xfatal-warnings")
                c.error(c.enclosingPosition, msg)
              else
                c.warning(c.enclosingPosition, msg)
            case _ =>
          }

        if (classSym.isCaseClass) // one-case ADT
          ADTSingle(tpe, classSym, ExpandingADTCase(tpe, classSym.companionSymbol.asTerm))
        else if (classSym.isSealed) // multiple cases
          ADTMulti(tpe, classSym)
        else
          exit(s"$classSym is not a case class, a sealed trait, an HList, a Coproduct or Unit")
      }
    }

    def undefined =
      reify { ??? }.tree

    def exit(msg: String) =
      c.abort(c.enclosingPosition, msg)

    def constructor =
      DefDef(
        Modifiers(),
        nme.CONSTRUCTOR,
        List(),
        List(List()),
        TypeTree(),
        Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(())))
      )

    def absurdCase =
      CaseDef(
        Ident(nme.WILDCARD),
        EmptyTree,
        undefined
      )

    def mkDummyObject(contents: List[Tree], select: TermName): Tree = {
      val name = newTermName(c.fresh())

      val module =
        ModuleDef(
          Modifiers(),
          name,
          Template(
            List(TypeTree(typeOf[AnyRef])),
            emptyValDef,
            constructor :: contents
          )
        )

      Block(
        List(module),
        Select(Ident(name), select)
      )
    }

    def mkDummyClass(contents: List[Tree], supertpe: Type): Tree = {
      val name = newTypeName(c.fresh())

      val clazz =
        ClassDef(
          Modifiers(FINAL),
          name,
          List(),
          Template(
            List(TypeTree(supertpe)),
            emptyValDef,
            constructor :: contents
          )
        )

      Block(
        List(clazz),
        Apply(Select(New(Ident(name)), nme.CONSTRUCTOR), List())
      )
    }

    def mkImplicitly(typ: Type): Tree =
      TypeApply(
        Select(Ident(definitions.PredefModule), newTermName("implicitly")),
        List(TypeTree(typ))
      )

    def mkImplicitlyAndAssign(name: TermName, typ: Type): ValDef =
      ValDef(
        Modifiers(LAZY),
        name,
        TypeTree(typ),
        mkImplicitly(typ)
      )

    def mkCompoundTpe[Parent, Nil <: Parent, Cons[_, _ <: Parent] <: Parent](
      items: List[Type])(implicit
      nil: c.WeakTypeTag[Nil],
      cons: c.WeakTypeTag[Cons[Any, Nothing]]
    ): Type =
      items.foldRight(nil.tpe) { case (tpe, acc) =>
        appliedType(cons.tpe, List(tpe, acc))
      }

    def mkHListTpe(items: List[Type]): Type =
      mkCompoundTpe[HList, HNil, ::](items)

    def mkCoproductTpe(items: List[Type]): Type =
      mkCompoundTpe[Coproduct, CNil, :+:](items)

    def destCompoundTpe[Parent, Nil <: Parent, Cons[_, _ <: Parent] <: Parent](tpe: Type)(implicit nil: c.WeakTypeTag[Nil]): List[Type] =
      if (tpe <:< nil.tpe)
        Nil
      else
        tpe match {
          case TypeRef(_, _, List(h, t)) =>
            h :: destCompoundTpe[Parent, Nil, Cons](t)
          case _ =>
            exit(s"bad type $tpe")
        }

    def destHListTpe(tpe: Type): List[Type] =
      destCompoundTpe[HList, HNil, ::](tpe)

    def destCoproductTpe(tpe: Type): List[Type] =
      destCompoundTpe[Coproduct, CNil, :+:](tpe)

    def mkFieldTpe(name: Name, tpe: Type): Type = {
      val atatTpe = typeOf[tag.@@[_,_]].typeConstructor
      val symTpe = typeOf[scala.Symbol]
      val fieldTypeTpe = typeOf[FieldType[_, _]].typeConstructor

      val kTpe = appliedType(atatTpe, List(symTpe, ConstantType(constantOfName(name))))
      appliedType(fieldTypeTpe, List(kTpe, tpe))
    }

    def mkRecordTpe(fields: List[(Name, Type)]): Type = {
      val items = fields.map { case (k, v) => mkFieldTpe(k, v) }
      mkCompoundTpe[HList, HNil, ::](items)
    }

    def anyNothing =
      TypeBoundsTree(Ident(typeOf[Nothing].typeSymbol), Ident(typeOf[Any].typeSymbol))

    def constantOfName(name: Name): Constant =
      Constant(name.decoded.trim)

    def literalOfName(name: Name): Tree =
      Literal(constantOfName(name))

    sealed trait ADT {
      def tpe: Type

      def usesCoproduct: Boolean

      def reprTpe: Type

      def combineAllInstances(tc: Tree, mapping: Map[Type, Tree]): Tree

      def allFieldTypes: List[Type]

      def mkToRepr(name: TermName): Tree
      def mkFromRepr(name: TermName): Tree

      def mkInstances(resName: TermName, tc: Type, to: Tree, from: Tree): List[Tree] = {
        val reprName, capabilityName = newTermName(c.fresh("inst"))
        val count = allFieldTypes.length
        val freshs = List.fill(count)(newTermName(c.fresh("inst")))

        val baseMapping = (allFieldTypes zip freshs).toMap
        val mapping = baseMapping.mapValues(Ident(_)) + (tpe → Ident(resName))

        val capability =
          if (usesCoproduct)
            typeOf[TypeClass[Any]]
          else
            typeOf[ProductTypeClass[Any]]

        val capabilityInstance =
          mkImplicitlyAndAssign(capabilityName, appliedType(capability, List(tc)))

        val baseInstances = baseMapping map { case (tpe, name) =>
          mkImplicitlyAndAssign(name, appliedType(tc, List(tpe)))
        }

        val reprInstance =
          ValDef(
            Modifiers(LAZY),
            reprName,
            TypeTree(appliedType(tc, List(reprTpe))),
            combineAllInstances(Ident(capabilityName), mapping)
          )

        val resInstance =
          ValDef(
            Modifiers(LAZY),
            resName,
            TypeTree(appliedType(tc, List(tpe))),
            Apply(Select(Ident(capabilityName), newTermName("project")), List(Ident(reprName), to, from))
          )

        capabilityInstance :: baseInstances.toList ::: List(reprInstance, resInstance)
      }

      def deriveInstance(tc: Type): Tree = {
        val toName, fromName, resName = newTermName(c.fresh())

        mkDummyObject(
          mkToRepr(toName) :: mkFromRepr(fromName) :: mkInstances(resName, tc, Ident(toName), Ident(fromName)),
          resName
        )
      }
      
      def outerTypeConstructor: Type =
        (if(labelled) typeOf[LabelledGeneric[_]] else typeOf[Generic[_]]).typeConstructor

      def materializeGeneric = {
        val toName = newTermName("to")
        val fromName = newTermName("from")

        mkDummyClass(
          List(
            TypeDef(Modifiers(), newTypeName("Repr"), List(), TypeTree(reprTpe)),
            mkToRepr(toName),
            mkFromRepr(fromName)
          ),
          appliedType(
            outerTypeConstructor,
            List(tpe)
          )
        )
      }

      def materializeIdentityGeneric = {
        val toName = newTermName("to")
        val fromName = newTermName("from")

        def mkIdentityDef(name: TermName) = {
          val param = newTermName("t")
          DefDef(
            Modifiers(),
            name,
            List(),
            List(List(ValDef(Modifiers(PARAM), param, TypeTree(tpe), EmptyTree))),
            TypeTree(tpe),
            Ident(param)
          )
        }

        mkDummyClass(
          List(
            TypeDef(Modifiers(), newTypeName("Repr"), List(), TypeTree(tpe)),
            mkIdentityDef(toName),
            mkIdentityDef(fromName)
          ),
          appliedType(
            outerTypeConstructor,
            List(tpe)
          )
        )
      }
    }

    case class ADTCoproduct(tpe: Type) extends ADT {

      val cases = destCoproductTpe(tpe)

      val (init, last) = cases match {
        case Nil    => exit("$tpe appears to be CNil")
        case i :+ l => (i, l)
      }

      def reprTpe = tpe

      def usesCoproduct = true

      def combineAllInstances(tc: Tree, mapping: Map[Type, Tree]) = {
        val lastInstance =
          Apply(Select(tc, newTermName("coproduct1")), List(mapping(last)))

        init.foldRight(lastInstance) { case (tpe, acc) =>
          Apply(Select(tc, newTermName("coproduct")), List(mapping(tpe), acc))
        }
      }

      def allFieldTypes = cases

      private def mkToOrFrom(name: TermName, inputTpe: Type, outputTpe: Type): Tree = {
        val param = newTermName(c.fresh("param"))

        DefDef(
          Modifiers(),
          name,
          List(),
          List(List(ValDef(Modifiers(PARAM), param, TypeTree(inputTpe), EmptyTree))),
          TypeTree(outputTpe),
          Ident(param)
        )
      }

      def mkToRepr(name: TermName) =
        mkToOrFrom(name, tpe, reprTpe)

      def mkFromRepr(name: TermName) =
        mkToOrFrom(name, reprTpe, tpe)

    }

    def inlValueTree = reify { Inl }.tree
    def inrValueTree = reify { Inr }.tree


    trait ADTComposed { self: ADT =>
      protected def cases: List[ADTCase]
      protected def wrap(index: Int)(tree: Tree): Tree

      def allFieldTypes =
        cases.flatMap(_.fieldTypes).filterNot(tpe =:= _).distinct

      private def mkToOrFrom(name: TermName, inputTpe: Type, outputTpe: Type, exhaust: Boolean, mkCase: (ADTCase, Tree => Tree) => CaseDef): Tree = {
        val param = newTermName(c.fresh("param"))

        val clauses =
          cases zip (Stream from 0) map { case (c, index) => mkCase(c, wrap(index)) }

        DefDef(
          Modifiers(),
          name,
          List(),
          List(List(ValDef(Modifiers(PARAM), param, TypeTree(inputTpe), EmptyTree))),
          TypeTree(outputTpe),
          Match(Ident(param), if (exhaust) clauses :+ absurdCase else clauses)
        )
      }

      def mkToRepr(name: TermName) =
        mkToOrFrom(name, tpe, reprTpe, false, _.mkToReprCase(_))

      def mkFromRepr(name: TermName) =
        mkToOrFrom(name, reprTpe, tpe, usesCoproduct, _.mkFromReprCase(_))
    }

    case class ADTSingle(tpe: Type, classSym: ClassSymbol, cse: ADTCase) extends ADT with ADTComposed {

      if (cse.fieldTypes contains tpe)
        exit("Single-case recursive ADTs are not supported")

      def cases = List(cse)

      def reprTpe = cse.reprTpe

      def usesCoproduct = false

      def combineAllInstances(tc: Tree, mapping: Map[Type, Tree]) =
        Apply(Select(tc, newTermName("namedCase")), List(cse.mkInstance(tc, mapping), literalOfName(classSym.name)))

      protected def wrap(index: Int)(tree: Tree) = tree

    }

    case class ADTMulti(tpe: Type, classSym: ClassSymbol) extends ADT with ADTComposed {

      private def collectCases(classSym: ClassSymbol): List[ClassSymbol] = {
        classSym.knownDirectSubclasses.toList flatMap { child0 =>
          val child = child0.asClass
          child.typeSignature // Workaround for <https://issues.scala-lang.org/browse/SI-7755>
          if (child.isCaseClass)
            List(child)
          else if (child.isSealed)
            collectCases(child)
          else
            exit(s"$child is not a case class or a sealed trait")
        }
      }

      // We're using an extremely optimistic strategy here, basically ignoring
      // the existence of any existential types.
      private def normalize(classSym: ClassSymbol): Type = tpe match {
        case base: TypeRef =>
          val subTpe = classSym.asType.toType
          classSym.typeParams match {
            case Nil =>
              subTpe
            case tpes =>
              appliedType(subTpe, base.args)
          }
        case _ =>
          exit(s"bad type $tpe")
      }

      val cases = collectCases(classSym).sortBy(_.fullName) map { sym =>
        val normalized = normalize(sym)
        if (expandInner)
          ExpandingADTCase(normalized, sym.companionSymbol.asTerm)
        else
          SimpleADTCase(normalized)
      }

      val (init, last) = cases match {
        case Nil    => exit("$tpe appears to have no cases")
        case i :+ l => (i, l)
      }

      def reprTpe =
        mkCoproductTpe(cases.map(_.reprTpe))

      def usesCoproduct = true

      def combineAllInstances(tc: Tree, mapping: Map[Type, Tree]) = {
        val lastInstance =
          Apply(Select(tc, newTermName("namedCoproduct1")), List(last.mkInstance(tc, mapping), literalOfName(last.name)))

        init.foldRight(lastInstance) { case (cse, acc) =>
          val instance = cse.mkInstance(tc, mapping)
          Apply(Select(tc, newTermName("namedCoproduct")), List(instance, literalOfName(cse.name), acc))
        }
      }

      protected def wrap(index: Int)(tree: Tree): Tree = {
        val inl = Apply(inlValueTree, List(tree))
        (0 until index).foldLeft(inl: Tree) { case (acc, _) =>
          Apply(inrValueTree, List(acc))
        }
      }

    }

    def hNilValueTree  = reify { HNil }.tree
    def hConsValueTree = reify {  ::  }.tree

    sealed trait ADTCase {
      def tpe: Type
      def fieldTypes: List[Type]
      def reprTpe: Type
      def mkInstance(tc: Tree, mapping: Map[Type, Tree]): Tree
      def mkToReprCase(wrap: Tree => Tree): CaseDef
      def mkFromReprCase(wrap: Tree => Tree): CaseDef

      def name = tpe.typeSymbol.name
    }

    trait IdentityCase { self: ADTCase =>
      def reprTpe = tpe

      def mkToReprCase(wrap: Tree => Tree): CaseDef = {
        val name = newTermName(c.fresh("x"))
        CaseDef(
          Bind(name, Typed(Ident(nme.WILDCARD), TypeTree(tpe))),
          EmptyTree,
          wrap(Ident(name))
        )
      }

      def mkFromReprCase(wrap: Tree => Tree): CaseDef = {
        val name = newTermName(c.fresh("x"))
        CaseDef(
          wrap(Bind(name, Ident(nme.WILDCARD))),
          EmptyTree,
          Ident(name)
        )
      }
    }

    case class SimpleADTCase(tpe: Type) extends ADTCase with IdentityCase {
      def fieldTypes: List[Type] = List(tpe)

      def mkInstance(tc: Tree, mapping: Map[Type, Tree]): Tree =
        mapping(tpe)
    }

    case class HListADTCase(tpe: Type) extends ADTCase with IdentityCase {
      def fieldTypes: List[Type] = destHListTpe(tpe)

      def mkInstance(tc: Tree, mapping: Map[Type, Tree]): Tree = {
        val empty: Tree = Select(tc, newTermName("emptyProduct"))
        val cons:  Tree = Select(tc, newTermName("product"))
        fieldTypes.foldRight(empty) { case (tpe, acc) =>
          Apply(cons, List(mapping(tpe), acc))
        }
      }
    }

    case object UnitADTCase extends ADTCase {
      def unitValueTree = reify { () }.tree

      val tpe = typeOf[Unit]
      val fieldTypes = Nil
      val reprTpe = typeOf[HNil]

      def mkInstance(tc: Tree, mapping: Map[Type, Tree]): Tree =
        Select(tc, newTermName("emptyProduct"))

      def mkToReprCase(wrap: Tree => Tree): CaseDef =
        CaseDef(
          unitValueTree,
          EmptyTree,
          wrap(hNilValueTree)
        )

      def mkFromReprCase(wrap: Tree => Tree): CaseDef =
        CaseDef(
          wrap(hNilValueTree),
          EmptyTree,
          unitValueTree
        )
    }

    case class ExpandingADTCase(tpe: Type, companion: TermSymbol) extends ADTCase {

      lazy val fields = tpe.declarations.toList collect {
        case x: TermSymbol if x.isVal && x.isCaseAccessor => x
      }

      def fieldFreshs(): List[TermName] =
        List.fill(fields.length)(newTermName(c.fresh("pat")))

      def fieldTypes =
        fields.map(fieldType)

      def fieldType(sym: TermSymbol): Type =
        sym.typeSignatureIn(tpe)

      def labelledFields: List[(Name, Type)] =
        fields.map { sym => (sym.name, fieldType(sym)) }

      def fieldNames: List[Name] =
        fields.map { sym => sym.name }

      def reprTpe = fieldTypes match {
        case List(tpe) if optimizeSingleItem =>
          tpe
        case tpes =>
          if(labelled)
            mkRecordTpe(labelledFields)
          else
            mkHListTpe(tpes)
      }

      def mkInstance(tc: Tree, mapping: Map[Type, Tree]): Tree = fields match {
        case List(sym) if optimizeSingleItem =>
          Apply(Select(tc, newTermName("namedField")), List(mapping(fieldType(sym)), literalOfName(sym.name)))
        case _ =>
          val empty: Tree = Select(tc, newTermName("emptyProduct"))
          val cons:  Tree = Select(tc, newTermName("namedProduct"))
          fields.foldRight(empty) { case (sym, acc) =>
            Apply(cons, List(mapping(fieldType(sym)), literalOfName(sym.name), acc))
          }
      }

      def mkElem(sym: TermName, name: Name, tpe: Type): Tree =
        if(labelled)
          TypeApply(Select(Ident(sym), newTermName("asInstanceOf")), List(TypeTree(mkFieldTpe(name, tpe))))
        else
          Ident(sym)

      def mkToReprCase(wrap: Tree => Tree): CaseDef = {
        val freshs = fieldFreshs() zip labelledFields
        val res = freshs match {
          case List((name, _)) if optimizeSingleItem =>
            Ident(name)
          case names =>
            names.foldRight(hNilValueTree) { case ((sym, (name, tpe)), acc) =>
              Apply(hConsValueTree, List(mkElem(sym, name, tpe), acc))
            }
        }
        CaseDef(
          Apply(Ident(companion), freshs.map(f => Bind(f._1, Ident(nme.WILDCARD)))),
          EmptyTree,
          wrap(res)
        )
      }

      def mkFromReprCase(wrap: Tree => Tree): CaseDef = {
        val freshs = fieldFreshs()
        val pat = freshs match {
          case List(name) if optimizeSingleItem =>
            Bind(name, Ident(nme.WILDCARD))
          case names =>
            names.foldRight(Ident(nme.WILDCARD): Tree) { case (sym, acc) =>
              Apply(hConsValueTree, List(Bind(sym, Ident(nme.WILDCARD)), acc))
            }
        }
        CaseDef(
          wrap(pat),
          EmptyTree,
          Apply(Ident(companion), freshs.map(Ident(_)))
        )
      }
    }
  }
}
