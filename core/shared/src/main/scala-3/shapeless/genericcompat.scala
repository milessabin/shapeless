/*
 * Copyright (c) 2012-18 Lars Hupel, Miles Sabin
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

import scala.quoted.*

// See `Materialized` for why each type class with an output type has a macro for each of these

trait GenericScalaCompat extends GenericScalaCompat0 {
  transparent inline implicit def materialize[T, R]: Generic.Aux[T, R] = ${ GenericMacros.materialize[T, R] }

  transparent inline implicit def materializeGeneric[T]: Materialized[Generic[T]] =
    ${ GenericMacros.materializedGeneric[T] }
}

trait GenericScalaCompat0 {
  implicit def materializeAux[T, R](implicit gen: Materialized[Generic[T]], ev: gen.value.Repr =:= R): Generic.Aux[T, R] =
    ev.substituteCo[[r] =>> Generic.Aux[T, r]](gen.value)
}

trait LabelledGenericScalaCompat extends LabelledGenericScalaCompat0 {
  transparent inline implicit def materialize[T, R]: LabelledGeneric.Aux[T, R] =
    ${ GenericMacros.materializeLabelled[T, R] }

  transparent inline implicit def materializeLabelledGeneric[T]: Materialized[LabelledGeneric[T]] =
    ${ GenericMacros.materializedLabelledGeneric[T] }
}

trait LabelledGenericScalaCompat0 {
  implicit def materializeAux[T, R](
    implicit gen: Materialized[LabelledGeneric[T]], ev: gen.value.Repr =:= R
  ): LabelledGeneric.Aux[T, R] =
    ev.substituteCo[[r] =>> LabelledGeneric.Aux[T, r]](gen.value)
}

trait IsTupleScalaCompat {
  transparent inline implicit def apply[T]: IsTuple[T] = ${ GenericMacros.mkIsTuple[T] }
}

trait HasProductGenericScalaCompat {
  transparent inline implicit def apply[T]: HasProductGeneric[T] = ${ GenericMacros.mkHasProductGeneric[T] }
}

trait HasCoproductGenericScalaCompat {
  transparent inline implicit def apply[T]: HasCoproductGeneric[T] = ${ GenericMacros.mkHasCoproductGeneric[T] }
}

/**
 * Scala 3 implementation of the `Generic` family of type classes.
 *
 * This follows the Scala 2 macro implementation closely so that the representation types of a given data type are the
 * same on both Scala versions. In particular the constructors of a sealed family are its (transitively) case class
 * like and object leaves, ordered by fully qualified name, rather than the direct children in declaration order which
 * a Scala 3 `Mirror` would provide.
 */
object GenericMacros {

  def materialize[T: Type, R: Type](using Quotes): Expr[Generic.Aux[T, R]] =
    mkGeneric[T].asInstanceOf[Expr[Generic.Aux[T, R]]]

  def materializeLabelled[T: Type, R: Type](using Quotes): Expr[LabelledGeneric.Aux[T, R]] =
    mkLabelledGeneric[T].asInstanceOf[Expr[LabelledGeneric.Aux[T, R]]]

  def materializedGeneric[T: Type](using Quotes): Expr[Materialized[Generic[T]]] =
    MacroUtils.materialized(mkGeneric[T])

  def materializedLabelledGeneric[T: Type](using Quotes): Expr[Materialized[LabelledGeneric[T]]] =
    MacroUtils.materialized(mkLabelledGeneric[T])

  def mkGeneric[T: Type](using Quotes): Expr[Generic[T]] = {
    import quotes.reflect.*
    val tpe = MacroUtils.normalize(TypeRepr.of[T])
    if (isReprType(tpe)) report.errorAndAbort("No Generic instance available for HList or Coproduct")
    val res = productInfo(tpe) match {
      case Some(info) => mkProductGeneric[T](info)
      case None => mkCoproductGeneric[T](tpe)
    }
    if (sys.props.contains("shapeless.debug")) report.info(res.asTerm.show(using Printer.TreeStructure))
    res
  }

  def mkLabelledGeneric[T: Type](using Quotes): Expr[LabelledGeneric[T]] = {
    import quotes.reflect.*
    val tpe = MacroUtils.normalize(TypeRepr.of[T])
    val gen = mkGeneric[T]
    val keys = labelsOf(tpe).map(l => ConstantType(StringConstant(l)))
    val repr = reprOf(tpe)
    val labelledRepr =
      if (repr <:< TypeRepr.of[HList])
        MacroUtils.mkHListTpe(keys.zip(MacroUtils.unpackHList(repr)).map(MacroUtils.mkFieldTpe.tupled))
      else
        MacroUtils.mkCoproductTpe(keys.zip(MacroUtils.unpackCoproduct(repr)).map(MacroUtils.mkFieldTpe.tupled))
    labelledRepr.asType match {
      case '[r] => '{ LabelledGeneric.unsafeInstance[T, r]($gen) }
    }
  }

  /** The generic representation type of `tpe`. */
  def reprOf(using Quotes)(tpe: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr =
    productInfo(tpe) match {
      case Some((fields, _, _)) => MacroUtils.mkHListTpe(fields.map(_._2))
      case None => MacroUtils.mkCoproductTpe(ctorsOf(tpe))
    }

  def mkIsTuple[T: Type](using Quotes): Expr[IsTuple[T]] = {
    import quotes.reflect.*
    val tpe = TypeRepr.of[T]
    if (!isTuple(tpe)) report.errorAndAbort(s"Unable to materialize IsTuple for non-tuple type ${tpe.show}")
    '{ new IsTuple[T] }
  }

  def mkHasProductGeneric[T: Type](using Quotes): Expr[HasProductGeneric[T]] = {
    import quotes.reflect.*
    val tpe = MacroUtils.normalize(TypeRepr.of[T])
    if (isReprType(tpe) || productInfo(tpe).isEmpty)
      report.errorAndAbort(s"Unable to materialize HasProductGeneric for ${tpe.show}")
    '{ new HasProductGeneric[T] }
  }

  def mkHasCoproductGeneric[T: Type](using Quotes): Expr[HasCoproductGeneric[T]] = {
    import quotes.reflect.*
    val tpe = MacroUtils.normalize(TypeRepr.of[T])
    if (isReprType(tpe) || !isCoproduct(tpe))
      report.errorAndAbort(s"Unable to materialize HasCoproductGeneric for ${tpe.show}")
    '{ new HasCoproductGeneric[T] }
  }

  def mkProductGeneric[T: Type](using Quotes)(
    info: (List[(String, quotes.reflect.TypeRepr)], quotes.reflect.Term => List[quotes.reflect.Term], List[quotes.reflect.Term] => quotes.reflect.Term)
  ): Expr[Generic[T]] = {
    import quotes.reflect.*
    val (fields, deconstruct, construct) = info
    val repr = MacroUtils.mkHListTpe(fields.map(_._2))
    val tpe = TypeRepr.of[T]
    val to = MacroUtils.lambda(tpe, repr)(t => MacroUtils.mkHList(deconstruct(t)))
    val from = MacroUtils.lambda(repr, tpe)(r => construct(hlistElems(r, fields.size)))
    Select.unique(Ref(TypeRepr.of[Generic.type].termSymbol), "instance")
      .appliedToTypes(List(tpe, repr))
      .appliedTo(to, from)
      .asExprOf[Generic[T]]
  }

  def mkCoproductGeneric[T: Type](using Quotes)(tpe: quotes.reflect.TypeRepr): Expr[Generic[T]] = {
    import quotes.reflect.*
    val ctors = ctorsOf(tpe)
    val repr = MacroUtils.mkCoproductTpe(ctors)

    def index(p: Expr[T]): Expr[Int] =
      ctors.zipWithIndex.foldRight('{ -1 }: Expr[Int]) { case ((ctor, i), acc) =>
        val test = instanceTest(p.asTerm, ctor)
        '{ if ($test) ${ Expr(i) } else $acc }
      }

    repr.asType match {
      case '[r] =>
        '{
          Generic.instance[T, r](
            (p: T) => Coproduct.unsafeMkCoproduct(${ index('p) }, p).asInstanceOf[r],
            (r: r) => Coproduct.unsafeGet(r.asInstanceOf[Coproduct]).asInstanceOf[T]
          )
        }
    }
  }

  /** A test whether the value `p` is an instance of the constructor type `ctor`. */
  def instanceTest(using Quotes)(p: quotes.reflect.Term, ctor: quotes.reflect.TypeRepr): Expr[Boolean] = {
    import quotes.reflect.*
    singletonRef(ctor) match {
      case Some(ref) => '{ ${ p.asExprOf[Any] }.asInstanceOf[AnyRef] eq ${ ref.asExprOf[Any] }.asInstanceOf[AnyRef] }
      case None =>
        // A class test, which (unlike a type test) can't be unchecked
        val erased = ctor match {
          case AppliedType(tycon, args) => tycon.appliedTo(args.map(_ => TypeBounds.empty))
          case other => other
        }
        val cls = Ref(defn.Predef_classOf).appliedToType(erased)
        Select.unique(cls, "isInstance").appliedTo(p).asExprOf[Boolean]
    }
  }

  /** The elements of an `HList` valued term with statically known length `n`. */
  def hlistElems(using Quotes)(hl: quotes.reflect.Term, n: Int): List[quotes.reflect.Term] = {
    import quotes.reflect.*
    List.iterate(hl, n + 1)(t => Select.unique(t, "tail")).init.map(t => Select.unique(t, "head"))
  }

  def isReprType(using Quotes)(tpe: quotes.reflect.TypeRepr): Boolean = {
    import quotes.reflect.*
    tpe <:< TypeRepr.of[HList] || tpe <:< TypeRepr.of[Coproduct]
  }

  def isTuple(using Quotes)(tpe: quotes.reflect.TypeRepr): Boolean = {
    import quotes.reflect.*
    tpe <:< TypeRepr.of[Unit] || (1 to 22).exists(n => tpe.typeSymbol == defn.TupleClass(n))
  }

  def isProduct(using Quotes)(tpe: quotes.reflect.TypeRepr): Boolean =
    productInfo(tpe).isDefined

  def isCoproduct(using Quotes)(tpe: quotes.reflect.TypeRepr): Boolean = {
    import quotes.reflect.*
    val sym = tpe.typeSymbol
    sym.isClassDef && sym.flags.is(Flags.Sealed) && (sym.flags.is(Flags.Trait) || sym.flags.is(Flags.Abstract))
  }

  def isNonGeneric(using Quotes)(sym: quotes.reflect.Symbol): Boolean = {
    import quotes.reflect.*
    val ng = TypeRepr.of[nonGeneric].typeSymbol
    sym.hasAnnotation(ng) || sym.allOverriddenSymbols.exists(_.hasAnnotation(ng))
  }

  /** Scala 3 synthesizes inline `apply` methods ("constructor proxies") for classes; these can't be referred to directly. */
  def isConstructorProxy(using Quotes)(sym: quotes.reflect.Symbol): Boolean = {
    import quotes.reflect.*
    sym.flags.is(Flags.Synthetic) && sym.flags.is(Flags.Inline)
  }

  def isPublic(using Quotes)(sym: quotes.reflect.Symbol): Boolean = {
    import quotes.reflect.*
    !sym.flags.is(Flags.Private) && !sym.flags.is(Flags.Protected) && !sym.flags.is(Flags.PrivateLocal) &&
      sym.privateWithin.isEmpty && sym.protectedWithin.isEmpty
  }

  /**
   * Whether `sym` (a constructor or `apply` method) is accessible from the expansion site: public, or private or
   * qualified private with the expansion within its scope of access. Protected members are treated as inaccessible.
   */
  def isAccessible(using Quotes)(sym: quotes.reflect.Symbol): Boolean = {
    import quotes.reflect.*
    val enclosing = List.unfold(Symbol.spliceOwner)(s => Option.when(!s.isNoSymbol)((s, s.owner)))
    def within(owner: Symbol): Boolean =
      enclosing.contains(owner) || !owner.companionModule.isNoSymbol && enclosing.contains(owner.companionModule.moduleClass)
    if (sym.flags.is(Flags.Protected) || sym.flags.is(Flags.PrivateLocal) || sym.protectedWithin.nonEmpty) false
    else if (sym.flags.is(Flags.Private)) within(sym.owner)
    else sym.privateWithin.forall(scope => within(scope.typeSymbol))
  }

  /** The type of a value of type `tpe` when passed as an argument: varargs become `Seq` and by-name is dropped. */
  def normalizeFieldType(using Quotes)(tpe: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr = {
    import quotes.reflect.*
    tpe match {
      case ByNameType(t) => normalizeFieldType(t)
      case AppliedType(tycon, List(elem)) if tycon.typeSymbol == defn.RepeatedParamClass =>
        TypeRepr.of[scala.collection.immutable.Seq].appliedTo(elem)
      case AnnotatedType(t, annot) if annot.tpe.typeSymbol == defn.RepeatedAnnot =>
        normalizeFieldType(t)
      case AnnotatedType(t, _) => normalizeFieldType(t)
      case AppliedType(tycon, args) => tycon.appliedTo(args.map(stripAnnotations))
      case other => other
    }
  }

  def stripAnnotations(using Quotes)(tpe: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr = {
    import quotes.reflect.*
    tpe match {
      case AnnotatedType(t, _) => stripAnnotations(t)
      case AppliedType(tycon, args) => tycon.appliedTo(args.map(stripAnnotations))
      case other => other
    }
  }

  def isVararg(using Quotes)(tpe: quotes.reflect.TypeRepr): Boolean = {
    import quotes.reflect.*
    tpe match {
      case AppliedType(tycon, _) => tycon.typeSymbol == defn.RepeatedParamClass
      case AnnotatedType(_, annot) => annot.tpe.typeSymbol == defn.RepeatedAnnot
      case _ => false
    }
  }

  /** A term referring to the singleton denoted by `tpe`, if it is a singleton type. */
  def singletonRef(using Quotes)(tpe: quotes.reflect.TypeRepr): Option[quotes.reflect.Term] = {
    import quotes.reflect.*
    tpe.dealias match {
      case tr: TermRef => Some(Ref.term(tr))
      case tr: TypeRef if tr.typeSymbol.flags.is(Flags.Module) && tr.typeSymbol.isClassDef =>
        tr.qualifier match {
          case pre: TermRef => Some(Select(Ref.term(pre), tr.typeSymbol.companionModule))
          case pre: ThisType if !pre.typeSymbol.flags.is(Flags.Package) && !pre.typeSymbol.flags.is(Flags.Module) =>
            Some(Select(This(pre.typeSymbol), tr.typeSymbol.companionModule))
          case _ => Some(Ref(tr.typeSymbol.companionModule))
        }
      case _ => None
    }
  }

  /** A term referring to the companion object of the class type `tpe`, as seen from its prefix. */
  def companionRef(using Quotes)(tpe: quotes.reflect.TypeRepr): Option[quotes.reflect.Term] = {
    import quotes.reflect.*
    val companion = tpe.typeSymbol.companionModule
    if (companion.isNoSymbol) None
    else tpe.dealias match {
      case AppliedType(tr: TypeRef, _) => companionFor(tr, companion)
      case tr: TypeRef => companionFor(tr, companion)
      case _ => None
    }
  }

  def companionFor(using Quotes)(tr: quotes.reflect.TypeRef, companion: quotes.reflect.Symbol): Option[quotes.reflect.Term] = {
    import quotes.reflect.*
    tr.qualifier match {
      case pre: TermRef => Some(Select(Ref.term(pre), companion))
      case pre: ThisType if !pre.typeSymbol.flags.is(Flags.Package) && !pre.typeSymbol.flags.is(Flags.Module) =>
        Some(Select(This(pre.typeSymbol), companion))
      case _ => Some(Ref(companion))
    }
  }

  /** Instantiates a polymorphic method type with the given type arguments, if any. */
  def instantiate(using Quotes)(tpe: quotes.reflect.TypeRepr, targs: List[quotes.reflect.TypeRepr]): quotes.reflect.TypeRepr = {
    import quotes.reflect.*
    tpe match {
      case pt: PolyType if pt.paramNames.size == targs.size => pt.appliedTo(targs)
      case other => other
    }
  }

  /** The non-implicit (explicit) parameter lists of a method type. */
  def explicitParamLists(using Quotes)(tpe: quotes.reflect.TypeRepr): List[List[(String, quotes.reflect.TypeRepr)]] = {
    import quotes.reflect.*
    tpe match {
      case pt: PolyType => explicitParamLists(pt.resType)
      case mt: MethodType if !mt.isImplicit => mt.paramNames.zip(mt.paramTypes) :: explicitParamLists(mt.resType)
      case _ => Nil
    }
  }

  /**
   * Applies `fn`, whose type parameters (if any) have already been supplied, to `args` as its first parameter list,
   * empty argument lists for any further explicit parameter lists and implicitly resolved arguments for implicit
   * parameter lists.
   */
  def applyFields(using Quotes)(fn: quotes.reflect.Term, args: List[quotes.reflect.Term]): quotes.reflect.Term = {
    import quotes.reflect.*
    def loop(fn: Term, first: Boolean): Term = fn.tpe.widen match {
      case mt: MethodType if mt.isImplicit =>
        val implicitArgs = mt.paramTypes.map { pt =>
          Implicits.search(pt) match {
            case s: ImplicitSearchSuccess => s.tree
            case f: ImplicitSearchFailure => report.errorAndAbort(f.explanation)
          }
        }
        loop(fn.appliedToArgs(implicitArgs), first)
      case mt: MethodType =>
        if (first) {
          val adapted = args.zip(mt.paramTypes).map { case (arg, pt) =>
            if (isVararg(pt)) Typed(arg, Inferred(pt)) else arg
          }
          loop(fn.appliedToArgs(adapted), false)
        } else loop(fn.appliedToNone, false)
      case _ => fn
    }
    loop(fn, true)
  }

  /** `tpe` with any wildcard type arguments replaced by their upper bounds. */
  def withoutWildcards(using Quotes)(tpe: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr = {
    import quotes.reflect.*
    tpe match {
      case AppliedType(tycon, args) => tycon.appliedTo(args.map {
        case TypeBounds(_, hi) => hi
        case arg => arg
      })
      case other => other
    }
  }

  /** Fields of a case class like type: public param accessors and lazy vals, in declaration order. */
  def fieldsOf(using Quotes)(tpe: quotes.reflect.TypeRepr): List[(String, quotes.reflect.Symbol)] = {
    import quotes.reflect.*
    val cls = tpe.typeSymbol
    if (cls.flags.is(Flags.Module)) Nil
    else if (cls.flags.is(Flags.Case))
      // Scala 2 compiled case classes also report their underlying private fields (with names ending in a space),
      // which are the only representation of fields without a public accessor
      cls.caseFields.foldLeft(List.empty[(String, Symbol)]) { (acc, f) =>
        val name = f.name.trim
        if (acc.exists(_._1 == name)) acc else (name, f) :: acc
      }.reverse.map { case (name, f) =>
        (name, cls.caseFields.find(a => a.name == name && !a.flags.is(Flags.Local)).getOrElse(f))
      }.filterNot { case (_, f) => isNonGeneric(f) }
    else
      // For Scala 2 compiled classes the public accessors of fields are methods
      cls.declarations.filter { f =>
        (f.isValDef || f.isDefDef && f.paramSymss.isEmpty && !f.isClassConstructor) && !f.name.endsWith(" ") &&
          isPublic(f) && (f.flags.is(Flags.ParamAccessor) || f.flags.is(Flags.Lazy)) && !isNonGeneric(f)
      }.map(f => (f.name, f))
  }

  /**
   * If `tpe0` is a product type, its labelled field types, and functions to deconstruct and reconstruct its values.
   */
  def productInfo(using Quotes)(tpe0: quotes.reflect.TypeRepr): Option[
    (List[(String, quotes.reflect.TypeRepr)], quotes.reflect.Term => List[quotes.reflect.Term], List[quotes.reflect.Term] => quotes.reflect.Term)
  ] = {
    import quotes.reflect.*
    val tpe = tpe0.dealias
    val sym = tpe.typeSymbol

    def unitInfo = (List.empty[(String, TypeRepr)], (_: Term) => List.empty[Term], (_: List[Term]) => '{ () }.asTerm)

    if (tpe =:= TypeRepr.of[Unit]) Some(unitInfo)
    else if (tpe =:= TypeRepr.of[AnyRef] || tpe =:= TypeRepr.of[Any] || !sym.isClassDef && singletonRef(tpe).isEmpty) None
    else singletonRef(tpe) match {
      case Some(ref) if tpe.isSingleton || sym.flags.is(Flags.Module) =>
        Some((List.empty[(String, TypeRepr)], (_: Term) => List.empty[Term], (_: List[Term]) => ref))
      case _ =>
        val isConcrete = !sym.flags.is(Flags.Abstract) && !sym.flags.is(Flags.Trait) && sym != defn.ObjectClass
        val ambiguousJavaCtor = sym.flags.is(Flags.JavaDefined) && sym.declarations.count(_.isClassConstructor) != 1
        if (!isConcrete || ambiguousJavaCtor) None
        else {
          val fields = fieldsOf(tpe)
          val fieldTpes = fields.map { case (name, f) => (name, normalizeFieldType(tpe.memberType(f).widenByName)) }

          def aligned(params: List[(String, TypeRepr)]): Boolean =
            params.size == fields.size && params.zip(fieldTpes).forall { case ((pn, pt), (fn, ft)) =>
              (pn == fn || (pt match { case ByNameType(_) => true; case _ => false })) && normalizeFieldType(pt) =:= ft
            }

          def viaApply: Option[List[Term] => Term] = for {
            companion <- companionRef(tpe)
            companionTpe = companion.tpe.widen
            apply <- companionTpe.typeSymbol.methodMember("apply").find { m =>
              !isConstructorProxy(m) && !isNonGeneric(m) && isAccessible(m) && explicitParamLists(instantiate(companionTpe.memberType(m), tpe.typeArgs)).headOption.fold(fields.isEmpty)(aligned)
            }
            if sym.flags.is(Flags.Case) || explicitParamLists(instantiate(companionTpe.memberType(apply), tpe.typeArgs)).size == 1
          } yield { (args: List[Term]) =>
            val sel = Select(companion, apply)
            val fn = if (tpe.typeArgs.nonEmpty) sel.appliedToTypes(tpe.typeArgs) else sel
            applyFields(fn, args)
          }

          def viaCtor: Option[List[Term] => Term] = {
            val ctor = sym.primaryConstructor
            val ctorTpe = instantiate(tpe.memberType(ctor), tpe.typeArgs)
            val lists = explicitParamLists(ctorTpe)
            val ok = !ctor.isNoSymbol && isAccessible(ctor) && !isNonGeneric(ctor) && lists.headOption.fold(fields.isEmpty)(params => aligned(params) && (sym.flags.is(Flags.Case) || lists.size == 1))
            Option.when(ok) { (args: List[Term]) =>
              val tycon = tpe match {
                case AppliedType(tycon, _) => tycon
                case other => other
              }
              val sel = Select(New(Inferred(tycon)), ctor)
              val fn = if (tpe.typeArgs.nonEmpty) sel.appliedToTypes(tpe.typeArgs) else sel
              applyFields(fn, args)
            }
          }

          val isCaseClass = sym.flags.is(Flags.Case)
          // For case classes prefer the constructor: looking at the companion can force definitions which are being
          // typed (eg. `object Foo { val gen = Generic[Foo] }`)
          val construct = if (isCaseClass) viaCtor.orElse(viaApply) else viaApply.orElse(viaCtor)

          def deconstruct(t: Term): List[Term] =
            fields.zipWithIndex.map { case ((_, f), i) =>
              if (isPublic(f)) Select(t, f)
              else {
                val ft = fieldTpes(i)._2
                ft.asType match {
                  case '[ft] => '{ ${ t.asExprOf[Product] }.productElement(${ Expr(i) }).asInstanceOf[ft] }.asTerm
                }
              }
            }

          construct.filter(_ => isCaseClass || fields.forall { case (_, f) => isPublic(f) }).map { c =>
            (fieldTpes, deconstruct, c)
          }
        }
    }
  }

  /** The labels of the fields of a product type or the constructors of a coproduct type. */
  def labelsOf(using Quotes)(tpe: quotes.reflect.TypeRepr): List[String] = {
    import quotes.reflect.*
    productInfo(tpe) match {
      case Some((fields, _, _)) => fields.map(_._1)
      case None if isCoproduct(tpe) => ctorsOf(tpe).map(nameOf)
      case None => report.errorAndAbort(s"${tpe.show} is not case class like or the root of a sealed family of types")
    }
  }

  def nameOf(using Quotes)(tpe: quotes.reflect.TypeRepr): String = {
    import quotes.reflect.*
    tpe.dealias match {
      case tr: TermRef => tr.termSymbol.name
      case other => other.typeSymbol.name.stripSuffix("$")
    }
  }

  /** A fully qualified name for `sym` in the style of Scala 2, used to order constructors consistently. */
  def scala2FullName(using Quotes)(sym: quotes.reflect.Symbol): String =
    sym.fullName.split('.').map(_.stripSuffix("$")).mkString(".")

  /** The constructors (case class like or singleton leaves) of the sealed type `tpe`, ordered by name. */
  def ctorsOf(using Quotes)(tpe0: quotes.reflect.TypeRepr): List[quotes.reflect.TypeRepr] = {
    import quotes.reflect.*
    val tpe = tpe0.dealias
    if (productInfo(tpe).isDefined) List(tpe)
    else if (!isCoproduct(tpe)) report.errorAndAbort(s"${tpe.show} is not a case class, case class-like, a sealed trait or Unit")
    else {
      val ctors = leaves(tpe).sortBy { case (sym, _) => scala2FullName(sym) }.map(_._2)
      val distinct = ctors.foldLeft(List.empty[TypeRepr])((acc, c) => if (acc.exists(_ =:= c)) acc else c :: acc).reverse
      if (distinct.isEmpty) report.errorAndAbort(s"Sealed trait ${tpe.show} has no case class subtypes")
      distinct
    }
  }

  /** The leaves of the sealed family `tpe`, with their symbols and their types as instances of `tpe`. */
  def leaves(using Quotes)(tpe: quotes.reflect.TypeRepr): List[(quotes.reflect.Symbol, quotes.reflect.TypeRepr)] = {
    import quotes.reflect.*
    val sym = tpe.typeSymbol
    sym.children.flatMap { child =>
      if (child != sym) leaf(tpe, child)
      else anonymousLeaves(tpe) match {
        case Nil => report.errorAndAbort(s"${tpe.show} has an anonymous subtype with unstable prefix")
        case anons => anons
      }
    }.filter { case (_, c) => c <:< tpe }
  }

  /**
   * Scala 3 records a sealed type with anonymous subclasses as its own child, and the types of the anonymous subclasses
   * aren't visible in the types of the values which instantiate them. Here we take the immutable vals of the sealed type
   * (or a refinement of it) defined in the enclosing object or the companion of the sealed type to be those values.
   */
  def anonymousLeaves(using Quotes)(tpe: quotes.reflect.TypeRepr): List[(quotes.reflect.Symbol, quotes.reflect.TypeRepr)] = {
    import quotes.reflect.*
    val sym = tpe.typeSymbol
    val scopes = List(sym.owner, sym.companionModule.moduleClass).filter(s => !s.isNoSymbol && s.isClassDef).distinct
    scopes.flatMap { scope =>
      val pre = scope.companionModule match {
        case m if !m.isNoSymbol => m.termRef
        case _ => This(scope).tpe
      }
      scope.declaredFields.filter { f =>
        f.isValDef && !f.flags.is(Flags.Mutable) && {
          val ft = pre.memberType(f).widen
          ft <:< tpe && ft.typeSymbol == sym
        }
      }.map(f => (f, pre.select(f)))
    }
  }

  def leaf(using Quotes)(tpe: quotes.reflect.TypeRepr, child: quotes.reflect.Symbol): List[(quotes.reflect.Symbol, quotes.reflect.TypeRepr)] = {
    import quotes.reflect.*
    locally {
      val childTpe = childType(tpe, child)
      if (child.isTerm || productInfo(withoutWildcards(childTpe)).isDefined) List((child, childTpe))
      else if (isCoproduct(childTpe)) leaves(childTpe)
      else report.errorAndAbort(s"${child.name} is not case class like or a sealed trait")
    }
  }

  /** The type of the child `child` of the sealed type `parent`, instantiated to conform to `parent`. */
  def childType(using Quotes)(parent: quotes.reflect.TypeRepr, child: quotes.reflect.Symbol): quotes.reflect.TypeRepr = {
    import quotes.reflect.*
    val prefix = parent.dealias match {
      case AppliedType(tr: TypeRef, _) => Some(tr.qualifier)
      case tr: TypeRef => Some(tr.qualifier)
      case _ => None
    }
    // The parent's prefix and its prefixes in turn, eg. `outer.inner` then `outer` for `outer.inner.T`
    val prefixes = List.unfold(prefix) {
      case Some(pre @ TermRef(q, _)) => Some((pre, Some(q)))
      case Some(pre) => Some((pre, None))
      case None => None
    }
    def isStatic(owner: Symbol): Boolean =
      owner.flags.is(Flags.Package) || owner.flags.is(Flags.Module) && owner.isClassDef && isStatic(owner.owner)

    // A path to `sym`: via the parent's prefixes where one of them provides it, otherwise via the path to its owner
    // when that is an object
    def select(sym: Symbol): TypeRepr = {
      val owner = sym.owner
      def own = if (sym.isTerm) sym.termRef else sym.typeRef
      if (isStatic(owner)) own
      else prefixes.find(pre => pre.typeSymbol == owner || pre.widen.derivesFrom(owner)) match {
        case Some(pre) => pre.select(sym)
        case None if owner.flags.is(Flags.Module) && owner.isClassDef => select(owner.companionModule).select(sym)
        case None => own
      }
    }

    if (child.isTerm) select(child)
    else if (child.flags.is(Flags.Module)) select(child.companionModule)
    else {
      val tparams = child.typeMembers.filter(_.isTypeParam)
      val base = select(child)
      if (tparams.isEmpty) base
      else {
        val generic = base.appliedTo(tparams.map(_.typeRef))
        val bindings = generic.baseType(parent.typeSymbol) match {
          case AppliedType(_, childArgs) => childArgs.zip(parent.dealias.typeArgs).foldLeft(Map.empty[Symbol, TypeRepr]) {
            case (acc, (c, p)) => unifyParams(tparams, c, p, acc)
          }
          case _ => Map.empty[Symbol, TypeRepr]
        }
        base.appliedTo(tparams.map(p => bindings.getOrElse(p, TypeBounds.empty)))
      }
    }
  }

  def unifyParams(using Quotes)(
    tparams: List[quotes.reflect.Symbol],
    pattern: quotes.reflect.TypeRepr,
    actual: quotes.reflect.TypeRepr,
    acc: Map[quotes.reflect.Symbol, quotes.reflect.TypeRepr]
  ): Map[quotes.reflect.Symbol, quotes.reflect.TypeRepr] = {
    import quotes.reflect.*
    pattern match {
      case tr: TypeRef if tparams.contains(tr.typeSymbol) =>
        if (acc.contains(tr.typeSymbol)) acc else acc.updated(tr.typeSymbol, actual)
      case AppliedType(ptycon, pargs) => actual match {
        case AppliedType(atycon, aargs) if ptycon =:= atycon && pargs.size == aargs.size =>
          pargs.zip(aargs).foldLeft(acc) { case (acc, (p, a)) => unifyParams(tparams, p, a, acc) }
        case _ => acc
      }
      case _ => acc
    }
  }
}
