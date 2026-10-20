/*
 * Copyright (c) 2011-16 Miles Sabin
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

/** Utilities shared by the Scala 3 macro implementations. */
/**
 * An instance materialized by a macro, typed precisely (eg. `Materialized[Generic.Aux[Foo, Int :: HNil]]`).
 *
 * Type classes with an output type (eg. `Generic.Aux[T, R]`) are materialized by a transparent macro declared with the
 * output (eg. `Generic.materialize[T, R]`). Before expanding a macro the compiler instantiates its type arguments,
 * approximating any undetermined type variables in them (eg. `R` as `Any`, or as its bound), and from Scala 3.10 it
 * requires the expansion to conform to the type of the macro's splice computed from them, which an instance with a
 * precise output (eg. `Generic.Aux[T, Int :: HNil]`) doesn't. So from Scala 3.10 those macros only succeed where the
 * output is determined, by the expected type or explicitly.
 *
 * Where it isn't, a lower priority instance (eg. `Generic.materializeAux`), which isn't a macro, takes a
 * `Materialized` instance from a macro declared without the output (eg. `Materialized[Generic[T]]`), which its
 * expansion conforms to, and determines the output from it. As it is only a candidate for `Materialized` instances,
 * that macro doesn't compete with the one declared with the output.
 */
final case class Materialized[+A](value: A)

object MacroUtils {
  /** `a` as a `Materialized` instance of its precise type. */
  def materialized[A: Type](a: Expr[A])(using Quotes): Expr[Materialized[A]] = {
    import quotes.reflect.*
    a.asTerm.tpe.widen.asType match {
      case '[p] => '{ Materialized[p](${ a.asExprOf[p] }) }.asExprOf[Materialized[A]]
    }
  }


  /**
   * Builds an application of the method `name` of `qual` to the explicit argument lists `argss`.
   *
   * Scala 3 macros cannot hand untyped trees back to the typer, so this does the work the typer would do for the common
   * cases: type arguments are inferred from the explicit arguments (or given in `targs`) and trailing implicit parameter
   * lists are resolved by implicit search.
   */
  def applyInferred(using Quotes)(
    qual: quotes.reflect.Term,
    name: String,
    targs: List[quotes.reflect.TypeRepr],
    argss: List[List[quotes.reflect.Term]]
  ): quotes.reflect.Term = {
    import quotes.reflect.*

    val qualTpe = qual.tpe.widen
    val candidates = qualTpe.typeSymbol.methodMember(name)
    if (candidates.isEmpty) report.errorAndAbort(s"missing method '$name'")

    def explicitArities(tpe: TypeRepr): List[Int] = tpe match {
      case pt: PolyType => explicitArities(pt.resType)
      case mt: MethodType if !mt.isImplicit => mt.paramNames.size :: explicitArities(mt.resType)
      case _ => Nil
    }

    val sym = candidates.find(s => explicitArities(qualTpe.memberType(s)) == argss.map(_.size)).getOrElse(candidates.head)
    val sel = Select(qual, sym)

    val (fn, tpe) = qualTpe.memberType(sym) match {
      case pt: PolyType =>
        val inferred =
          if (targs.nonEmpty) targs
          else inferTypeArgs(pt, argss.flatten.map(_.tpe.widenTermRefByName))
        (sel.appliedToTypes(inferred), pt.appliedTo(inferred))
      case other => (sel, other)
    }

    def applyAll(fn: Term, tpe: TypeRepr, argss: List[List[Term]]): Term = (tpe, argss) match {
      case (mt: MethodType, _) if mt.isImplicit =>
        val args = mt.paramTypes.map { pt =>
          Implicits.search(pt) match {
            case s: ImplicitSearchSuccess => s.tree
            case f: ImplicitSearchFailure => report.errorAndAbort(f.explanation)
          }
        }
        val applied = fn.appliedToArgs(args)
        applyAll(applied, applied.tpe.widen, argss)
      case (mt: MethodType, _) if argss.nonEmpty =>
        val applied = fn.appliedToArgs(adaptArgs(mt, argss.head))
        applyAll(applied, applied.tpe.widen, argss.tail)
      case (_, Nil) => fn
      case _ => report.errorAndAbort(s"too many argument lists for method '$name'")
    }

    // The trees built here aren't typechecked, so check the arguments, applying implicit conversions (including
    // numeric widening), as the typer would
    def adaptArgs(mt: MethodType, args: List[Term]): List[Term] = {
      val (params, repeated) = mt.paramTypes.lastOption match {
        case Some(AnnotatedType(tpe, _)) if tpe.typeSymbol == defn.RepeatedParamClass => (mt.paramTypes.init, tpe.typeArgs.headOption)
        case Some(tpe) if tpe.typeSymbol == defn.RepeatedParamClass => (mt.paramTypes.init, tpe.typeArgs.headOption)
        case _ => (mt.paramTypes, None)
      }
      if (args.size < params.size || repeated.isEmpty && args.size > params.size)
        report.errorAndAbort(s"wrong number of arguments for method '$name': expected ${params.size}, found ${args.size}")
      val expected = params ++ List.fill(args.size - params.size)(repeated.toList).flatten
      args.zip(expected).map { case (arg, pt) =>
        val formal = pt match {
          case ByNameType(underlying) => underlying
          case other => other
        }
        def mismatch = report.errorAndAbort(
          s"type mismatch in argument of method '$name': found ${arg.tpe.widen.show}, required ${formal.show}"
        )
        if (arg.tpe <:< formal) arg
        else Implicits.search(TypeRepr.of[Function1].appliedTo(List(arg.tpe.widen, formal))) match {
          case s: ImplicitSearchSuccess =>
            val converted = Select.unique(s.tree, "apply").appliedTo(arg)
            if (converted.tpe <:< formal) converted else mismatch
          case _: ImplicitSearchFailure => mismatch
        }
      }
    }

    applyAll(fn, tpe, argss)
  }

  /** Infers type arguments for `pt` by matching the types of its explicit parameters against `argTpes`. */
  def inferTypeArgs(using Quotes)(
    pt: quotes.reflect.PolyType,
    argTpes: List[quotes.reflect.TypeRepr]
  ): List[quotes.reflect.TypeRepr] = {
    import quotes.reflect.*

    def explicitParams(tpe: TypeRepr): List[TypeRepr] = tpe match {
      case mt: MethodType if !mt.isImplicit => mt.paramTypes ++ explicitParams(mt.resType)
      case _ => Nil
    }

    def unify(param: TypeRepr, arg: TypeRepr, acc: Map[Int, TypeRepr]): Map[Int, TypeRepr] = param match {
      case ParamRef(binder, idx) if binder == pt =>
        if (acc.contains(idx)) acc else acc.updated(idx, arg)
      case AppliedType(tycon, pargs) =>
        arg.baseType(tycon.typeSymbol) match {
          case AppliedType(_, aargs) if aargs.size == pargs.size =>
            pargs.zip(aargs).foldLeft(acc) { case (acc, (p, a)) => unify(p, a, acc) }
          case _ => acc
        }
      case _ => acc
    }

    val bindings = explicitParams(pt.resType).zip(argTpes).foldLeft(Map.empty[Int, TypeRepr]) {
      case (acc, (p, a)) => unify(p, a, acc)
    }

    pt.paramNames.indices.toList.map { i =>
      bindings.getOrElse(i, report.errorAndAbort(s"Unable to infer type argument ${pt.paramNames(i)}"))
    }
  }

  /** Builds an `HList` value, and its precise type, from the given elements. */
  def mkHList(using Quotes)(elems: List[quotes.reflect.Term]): quotes.reflect.Term = {
    import quotes.reflect.*
    elems.foldRight('{ HNil: HNil }.asTerm) { (elem, acc) =>
      val hd = elem.tpe.widenTermRefByName
      val tl = acc.tpe.widenTermRefByName
      Select.unique(New(Inferred(TypeRepr.of[::].appliedTo(List(hd, tl)))), "<init>")
        .appliedToTypes(List(hd, tl))
        .appliedToArgs(List(elem, acc))
    }
  }

  /** The `HList` type with the given element types. */
  def mkHListTpe(using Quotes)(elems: List[quotes.reflect.TypeRepr]): quotes.reflect.TypeRepr = {
    import quotes.reflect.*
    elems.foldRight(TypeRepr.of[HNil])((elem, acc) => TypeRepr.of[::].appliedTo(List(elem, acc)))
  }

  /** The `Coproduct` type with the given element types. */
  def mkCoproductTpe(using Quotes)(elems: List[quotes.reflect.TypeRepr]): quotes.reflect.TypeRepr = {
    import quotes.reflect.*
    elems.foldRight(TypeRepr.of[CNil])((elem, acc) => TypeRepr.of[:+:].appliedTo(List(elem, acc)))
  }

  /** The labelled field type `FieldType[K, V]`. */
  def mkFieldTpe(using Quotes)(key: quotes.reflect.TypeRepr, value: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr = {
    import quotes.reflect.*
    TypeRepr.of[labelled.FieldType].appliedTo(List(key, value))
  }

  def unpackHList(using Quotes)(tpe: quotes.reflect.TypeRepr): List[quotes.reflect.TypeRepr] = {
    import quotes.reflect.*
    unpackReprType(tpe, TypeRepr.of[HNil], TypeRepr.of[::].typeSymbol)
  }

  def unpackCoproduct(using Quotes)(tpe: quotes.reflect.TypeRepr): List[quotes.reflect.TypeRepr] = {
    import quotes.reflect.*
    unpackReprType(tpe, TypeRepr.of[CNil], TypeRepr.of[:+:].typeSymbol)
  }

  def unpackReprType(using Quotes)(
    tpe: quotes.reflect.TypeRepr, nil: quotes.reflect.TypeRepr, cons: quotes.reflect.Symbol
  ): List[quotes.reflect.TypeRepr] = {
    import quotes.reflect.*
    @annotation.tailrec
    def unpack(tpe: TypeRepr, acc: List[TypeRepr]): List[TypeRepr] =
      if (tpe <:< nil) acc.reverse
      else tpe.baseType(cons) match {
        case AppliedType(_, List(head, tail)) => unpack(tail, head :: acc)
        case _ => report.errorAndAbort(s"${tpe.show} is not an HList or Coproduct type")
      }
    unpack(tpe.dealias, Nil)
  }

  /** Decomposes a labelled field type `FieldType[K, V]` into its key and value types. */
  def unpackFieldType(using Quotes)(tpe: quotes.reflect.TypeRepr): Option[(quotes.reflect.TypeRepr, quotes.reflect.TypeRepr)] = {
    import quotes.reflect.*
    tpe.dealias.baseType(TypeRepr.of[labelled.KeyTag].typeSymbol) match {
      case AppliedType(_, List(k, v)) => Some((k.dealias, v))
      case _ => None
    }
  }

  /** The key, value and index of the field with key `key` in the record field types `fields`. */
  def findField(using Quotes)(
    fields: List[quotes.reflect.TypeRepr], key: quotes.reflect.TypeRepr
  ): Option[(quotes.reflect.TypeRepr, quotes.reflect.TypeRepr, Int)] =
    fields.iterator.zipWithIndex.map { case (f, i) => (unpackFieldType(f), i) }.collectFirst {
      case (Some((k, v)), i) if k =:= key => (k, v, i)
    }

  /** A function literal of type `arg => res` with the given body. */
  def lambda(using Quotes)(arg: quotes.reflect.TypeRepr, res: quotes.reflect.TypeRepr)(
    body: quotes.reflect.Term => quotes.reflect.Term
  ): quotes.reflect.Term = {
    import quotes.reflect.*
    val mt = MethodType(List("x"))(_ => List(arg), _ => res)
    Lambda(Symbol.spliceOwner, mt, (meth, params) => body(params.head.asInstanceOf[Term]).changeOwner(meth))
  }

  /**
   * Dealiases `tpe`, also reducing type projections on refinements (such as those produced by the type providers,
   * eg. ``Witness.`1`.T``) to the types they are aliases for.
   */
  def normalize(using Quotes)(tpe: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr = {
    import quotes.reflect.*
    tpe.dealias match {
      case tr: TypeRef if !tr.typeSymbol.isClassDef =>
        val sup = tr.translucentSuperType
        if (!(sup == tr) && sup =:= tr) normalize(sup) else tr
      case other => other
    }
  }
}
