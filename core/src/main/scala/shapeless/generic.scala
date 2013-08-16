/*
 * Copyright (c) 2012-13 Lars Hupel, Miles Sabin 
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
  implicit def apply[T] = macro GenericMacros.materialize[T]
}

object Generic extends LowPriorityGeneric {
  type Aux[T, Repr0] = Generic[T] { type Repr = Repr0 }

  // Refinement for products, here we can provide the calling context with
  // a proof that the resulting Repr <: HList
  implicit def product[T <: Product] = macro GenericMacros.materializeForProduct[T]
}

object GenericMacros {
  def materialize[T: context.WeakTypeTag](context : Context): context.Expr[Generic[T]] = {
    val tpe0 = context.weakTypeOf[T]
    if (tpe0 <:< context.typeOf[HList] || tpe0 <:< context.typeOf[Coproduct])
      context.universe.reify {
        new Generic[T] {
          type Repr = T
          def to(t : T) : T = t
          def from(t : T) : T = t
        }
      }
    else if (tpe0 =:= context.typeOf[Unit])
      context.universe.reify {
        new Generic[T] {
          type Repr = HNil
          def to(t: Unit): HNil = HNil
          def from(r : HNil): Unit = ()
        }
      }
    else {
      val helper = new Helper[context.type] {
        val c: context.type = context
        val expandInner = false
        val optimizeSingleItem = false
        val tpe = tpe0
      }
      context.Expr[Generic[T]](helper.ADT.materializeGeneric)
    }
  }

  def materializeForProduct[T <: Product: context.WeakTypeTag](context : Context): context.Expr[Generic[T] { type Repr <: HList }] = {
    val tpe0 = context.weakTypeOf[T]
    if (tpe0 <:< context.typeOf[Coproduct])
      context.abort(context.enclosingPosition, s"Cannot materialize Coproduct $tpe0 as a Product")

    val helper = new Helper[context.type] {
      val c: context.type = context
      val expandInner = false
      val optimizeSingleItem = false
      val tpe = tpe0
    }

    context.Expr[Generic[T] { type Repr <: HList }] {
      if (tpe0 <:< context.typeOf[HList])
        // Explicit tree construction used here because we can't prove that tpe0
        // refines HList to reify's satisfaction
        helper.ADT.materializeIdentityGeneric
      else
        helper.ADT.materializeGeneric
    }
  }

  trait Helper[+C <: Context] {

    val c: C
    val expandInner: Boolean
    val optimizeSingleItem: Boolean
    val tpe: c.Type

    import c.universe._
    import Flag._


    def ADT: ADT = {
      def collectCases(classSym: ClassSymbol): List[ClassSymbol] = {
        classSym.knownDirectSubclasses.toList flatMap { child0 =>
          val child = child0.asClass
          child.typeSignature // Workaround for https://issues.scala-lang.org/browse/SI-7755
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
      def normalize(classSym: ClassSymbol): Type = tpe match {
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

      val sym = tpe.typeSymbol
      if (!sym.isClass)
        exit(s"$sym is not a class or trait")

      val classSym = sym.asClass
      classSym.typeSignature // Workaround for https://issues.scala-lang.org/browse/SI-7755
      if (classSym.isCaseClass) // one-case ADT
        ADTSingle(tpe, classSym, ExpandingADTCase(tpe, classSym.companionSymbol.asTerm))
      else if (classSym.isSealed) { // multiple cases
        val cases = collectCases(classSym).sortBy(_.fullName)
        ADTMulti(tpe, classSym, cases map { sym =>
          val normalized = normalize(sym)
          if (expandInner)
            ExpandingADTCase(normalized, sym.companionSymbol.asTerm)
          else
            SimpleADTCase(normalized)
        })
      }
      else
        exit(s"$classSym is not a case class or a sealed trait")
    }


    def undefined = reify { ??? }.tree

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

    def mkDummyClass(contents: List[Tree], supertpt: Tree): Tree = {
      val name = newTypeName(c.fresh())

      val clazz =
        ClassDef(
          Modifiers(FINAL),
          name,
          List(),
          Template(
            List(supertpt),
            emptyValDef,
            constructor :: contents
          )
        )

      Block(
        List(clazz),
        Apply(Select(New(Ident(name)), nme.CONSTRUCTOR), List())
      )
    }

    def mkImplicitly(tpt: Tree): Tree =
      TypeApply(
        Select(Ident(definitions.PredefModule), newTermName("implicitly")),
        List(tpt)
      )

    def mkImplicitlyAndAssign(name: TermName, tpt: Tree): ValDef =
      ValDef(
        Modifiers(LAZY),
        name,
        tpt,
        mkImplicitly(tpt)
      )

    def mkCompoundTpt[Parent, Nil <: Parent, Cons[_, _ <: Parent] <: Parent](
      items: List[Tree])(implicit
      nil: c.WeakTypeTag[Nil],
      cons: c.WeakTypeTag[Cons[Any, Nothing]]
    ): Tree =
      items.foldRight(TypeTree(nil.tpe): Tree) { case (tpt, acc) =>
        AppliedTypeTree(Ident(cons.tpe.typeSymbol), List(tpt, acc))
      }

    def mkHListTpt(items: List[Tree]): Tree =
      mkCompoundTpt[HList, HNil, ::](items)

    def mkCoproductTpt(items: List[Tree]): Tree =
      mkCompoundTpt[Coproduct, CNil, :+:](items)

    def anyNothing =
      TypeBoundsTree(Ident(typeOf[Nothing].typeSymbol), Ident(typeOf[Any].typeSymbol))

    sealed trait ADT {
      def tpe: Type
      def classSym: ClassSymbol
      def cases: List[ADTCase]

      def usesCoproduct: Boolean

      def wrap(index: Int)(tree: Tree): Tree

      def reprTpt: Tree

      def combineCaseInstances(tc: Tree, mapping: Map[Type, Tree]): Tree

      lazy val allFieldTypes: List[Type] =
        cases.flatMap(_.fieldTypes).filterNot(tpe =:= _).distinct

      def selfTpt: Tree =
        TypeTree(tpe)

      def mkToOrFrom(name: TermName, inputTpt: Tree, outputTpt: Tree, exhaust: Boolean, mkCase: (ADTCase, Tree => Tree) => CaseDef): Tree = {
        val param = newTermName(c.fresh("param"))

        val clauses =
          cases zip (Stream from 0) map { case (c, index) => mkCase(c, wrap(index)) }

        DefDef(
          Modifiers(),
          name,
          List(),
          List(List(ValDef(Modifiers(PARAM), param, inputTpt, EmptyTree))),
          outputTpt,
          Match(Ident(param), if (exhaust) clauses :+ absurdCase else clauses)
        )
      }

      def mkToRepr(name: TermName): Tree =
        mkToOrFrom(
          name,
          selfTpt,
          reprTpt,
          false,
          _.mkToReprCase(_)
        )

      def mkFromRepr(name: TermName): Tree =
        mkToOrFrom(
          name,
          reprTpt,
          selfTpt,
          usesCoproduct,
          _.mkFromReprCase(_)
        )

      def mkInstances(resName: TermName, tc: Type, to: Tree, from: Tree): List[Tree] = {
        val reprName, capabilityName = newTermName(c.fresh("inst"))
        val count = allFieldTypes.length
        val freshs = List.fill(count)(newTermName(c.fresh("inst")))

        val baseMapping = (allFieldTypes zip freshs).toMap
        val mapping = baseMapping.mapValues(Ident(_)) + (tpe → Ident(resName))

        val capability =
          if (usesCoproduct)
            typeOf[TypeClass[Any]].typeSymbol
          else
            typeOf[ProductTypeClass[Any]].typeSymbol

        val capabilityInstance =
          mkImplicitlyAndAssign(capabilityName, AppliedTypeTree(Ident(capability), List(TypeTree(tc))))

        val baseInstances = baseMapping map { case (tpe, name) =>
          mkImplicitlyAndAssign(name, AppliedTypeTree(Ident(tc.typeSymbol), List(TypeTree(tpe))))
        }

        val reprInstance =
          ValDef(
            Modifiers(LAZY),
            reprName,
            AppliedTypeTree(Ident(tc.typeSymbol), List(reprTpt)),
            combineCaseInstances(Ident(capabilityName), mapping)
          )

        val resInstance =
          ValDef(
            Modifiers(LAZY),
            resName,
            AppliedTypeTree(Ident(tc.typeSymbol), List(selfTpt)),
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

      def materializeGeneric = {
        val toName = newTermName("to")
        val fromName = newTermName("from")

        mkDummyClass(
          List(
            TypeDef(Modifiers(), newTypeName("Repr"), List(), reprTpt),
            mkToRepr(toName),
            mkFromRepr(fromName)
          ),
          AppliedTypeTree(
            Ident(typeOf[Generic[_]].typeSymbol),
            List(selfTpt)
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
            List(List(ValDef(Modifiers(PARAM), param, selfTpt, EmptyTree))),
            selfTpt,
            Ident(param)
          )
        }

        mkDummyClass(
          List(
            TypeDef(Modifiers(), newTypeName("Repr"), List(), selfTpt),
            mkIdentityDef(toName),
            mkIdentityDef(fromName)
          ),
          AppliedTypeTree(
            Ident(typeOf[Generic[_]].typeSymbol),
            List(selfTpt)
          )
        )
      }
    }

    case class ADTSingle(tpe: Type, classSym: ClassSymbol, cse: ADTCase) extends ADT {

      if (cse.fieldTypes contains tpe)
        exit("Single-case recursive ADTs are not supported")

      def cases = List(cse)

      def reprTpt = cse.reprTpt

      def wrap(index: Int)(tree: Tree) = tree

      def usesCoproduct = false

      def combineCaseInstances(tc: Tree, mapping: Map[Type, Tree]) =
        cse.mkInstance(tc, mapping)

    }

    case class ADTMulti(tpe: Type, classSym: ClassSymbol, cases: List[ADTCase]) extends ADT {

      if (cases.isEmpty)
        exit(s"$tpe appears to have no cases")

      def reprTpt =
        mkCoproductTpt(cases.map(_.reprTpt))

      def wrap(index: Int)(tree: Tree): Tree = {
        val inl = Apply(reify { Inl }.tree, List(tree))
        (0 until index).foldLeft(inl: Tree) { case (acc, _) =>
          Apply(reify { Inr }.tree, List(acc))
        }
      }

      def usesCoproduct = true

      def combineCaseInstances(tc: Tree, mapping: Map[Type, Tree]) =
        cases.map(_.mkInstance(tc, mapping)).foldRight(undefined) { case (instance, acc) =>
          Apply(Select(tc, newTermName("coproduct")), List(instance, acc))
        }

    }

    sealed trait ADTCase {
      def fieldTypes: List[Type]
      def reprTpt: Tree
      def mkInstance(tc: Tree, mapping: Map[Type, Tree]): Tree
      def mkToReprCase(wrap: Tree => Tree): CaseDef
      def mkFromReprCase(wrap: Tree => Tree): CaseDef
    }

    case class SimpleADTCase(tpe: Type) extends ADTCase {

      def fieldTypes: List[Type] = List(tpe)
      def reprTpt: Tree = TypeTree(tpe)

      def mkInstance(tc: Tree, mapping: Map[Type, Tree]): Tree =
        mapping(tpe)

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

    case class ExpandingADTCase(tpe: Type, companion: TermSymbol) extends ADTCase {

      def hNilValueTree  = reify { HNil }.tree
      def hConsValueTree = reify {  ::  }.tree

      lazy val fields = tpe.declarations.toList collect {
        case x: TermSymbol if x.isVal && x.isCaseAccessor => x
      }

      def fieldFreshs(): List[TermName] =
        List.fill(fields.length)(newTermName(c.fresh("pat")))

      def fieldTypes: List[Type] =
        fields.map(_.typeSignatureIn(tpe))

      def reprTpt = fieldTypes match {
        case List(tpe) if optimizeSingleItem =>
          TypeTree(tpe)
        case tpes =>
          mkHListTpt(tpes.map(TypeTree(_)))
      }

      def mkInstance(tc: Tree, mapping: Map[Type, Tree]): Tree = fieldTypes match {
        case List(tpe) if optimizeSingleItem =>
          mapping(tpe)
        case tpes =>
          val empty: Tree = Select(tc, newTermName("emptyProduct"))
          val cons:  Tree = Select(tc, newTermName("product"))
          fieldTypes.foldRight(empty) { case (tpe, acc) =>
            Apply(cons, List(mapping(tpe), acc))
          }
      }

      def mkToReprCase(wrap: Tree => Tree): CaseDef = {
        val freshs = fieldFreshs()
        val res = freshs match {
          case List(name) if optimizeSingleItem =>
            Ident(name)
          case names =>
            names.foldRight(hNilValueTree) { case (sym, acc) =>
              Apply(hConsValueTree, List(Ident(sym), acc))
            }
        }
        CaseDef(
          Apply(Ident(companion), freshs.map(f => Bind(f, Ident(nme.WILDCARD)))),
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

// vim: expandtab:ts=2:sw=2
