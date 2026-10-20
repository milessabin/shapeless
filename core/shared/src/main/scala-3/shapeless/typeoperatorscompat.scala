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

import scala.language.dynamics
import scala.quoted.*

/**
 * Type class witnessing the least upper bound of a pair of types and providing conversions from each to their common
 * supertype.
 *
 * On Scala 3 the type parameters are invariant: instances are computed exactly, as on Scala 2, rather than relying on
 * contravariance (which would let Scala 3 widen `A` and `B` before the least upper bound is computed).
 *
 * @author Miles Sabin
 */
trait Lub[A, B, Out] extends Serializable {
  def left(a : A): Out
  def right(b : B): Out
}

object Lub extends LubScalaCompat {
  def instance[A, B, Out](f: A => Out, g: B => Out): Lub[A, B, Out] = new Lub[A, B, Out] {
    def left(a : A): Out = f(a)
    def right(b : B): Out = g(b)
  }
}

trait LubScalaCompat extends LubScalaCompat1 {
  /**
   * On Scala 3 the least upper bound is computed as Scala 3 infers least upper bounds elsewhere, so that eg. the element
   * type of `hlist.toList` is that of a list literal with the same elements.
   */
  transparent inline implicit def lub[A, B, Out]: Lub[A, B, Out] = ${ LubMacros.lubImpl[A, B, Out] }
}

trait LubScalaCompat1 extends LubScalaCompat0 {
  /**
   * For an undetermined upper bound. From Scala 3.10 the expansion of `lub` above must conform to `Lub[A, B, Any]` for
   * an undetermined `Out` (see `Materialized`), which it doesn't, so on Scala 3.10 the bound is computed as the
   * output type of `LubOf`.
   */
  implicit def lubUndetermined[A, B, Out](implicit lubOf: LubOf.Aux[A, B, Out]): Lub[A, B, Out] = lubOf.lub
}

/** The least upper bound of `A` and `B` as an output type. An implementation detail of Scala 3 `Lub`. */
trait LubOf[A, B] extends Serializable {
  type Out
  def lub: Lub[A, B, Out]
}

object LubOf {
  type Aux[A, B, Out0] = LubOf[A, B] { type Out = Out0 }

  transparent inline implicit def materialize[A, B]: LubOf[A, B] = ${ LubMacros.lubOfImpl[A, B] }

  def instance[A, B, Out0](l: Lub[A, B, Out0]): Aux[A, B, Out0] = new LubOf[A, B] {
    type Out = Out0
    def lub: Lub[A, B, Out] = l
  }
}

trait LubScalaCompat0 {
  // An explicitly requested `Any` upper bound can't be distinguished from an undetermined one in `lub` above
  implicit def lubAny[A, B, Out](implicit ev: Any =:= Out): Lub[A, B, Out] = Lub.instance[A, B, Out](ev(_), ev(_))
}

object LubMacros {
  def lubImpl[A: Type, B: Type, Out: Type](using Quotes): Expr[Lub[A, B, Out]] = {
    import quotes.reflect.*
    val a = TypeRepr.of[A]
    val b = TypeRepr.of[B]
    val out = TypeRepr.of[Out]
    // A requested upper bound is used as is (as Scala 2's `Lub[T, T, T]` allows); an undetermined one arrives as `Any`
    val lub = if (!(out =:= TypeRepr.of[Any]) && a <:< out && b <:< out) out else join(a, b)
    lub.asType match {
      case '[l] => '{ Lub.instance[A, B, l](_.asInstanceOf[l], _.asInstanceOf[l]) }.asInstanceOf[Expr[Lub[A, B, Out]]]
    }
  }

  def lubOfImpl[A: Type, B: Type](using Quotes): Expr[LubOf[A, B]] = {
    import quotes.reflect.*
    join(TypeRepr.of[A], TypeRepr.of[B]).asType match {
      case '[l] => '{ LubOf.instance[A, B, l](Lub.instance[A, B, l](_.asInstanceOf[l], _.asInstanceOf[l])) }
    }
  }

  /**
   * The least upper bound of `a` and `b` as Scala 3 infers it: the join of their common base classes ignoring `Product`
   * and `Serializable` (eg. `Fruit` for two case classes extending it), or else their union (eg. `Int | String`).
   */
  def join(using Quotes)(a0: quotes.reflect.TypeRepr, b0: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr = {
    import quotes.reflect.*
    val a = MacroUtils.normalize(a0)
    val b = MacroUtils.normalize(b0)
    if (a <:< b) b
    else if (b <:< a) a
    else if (a.isSingleton || b.isSingleton) join(a.widen, b.widen)
    else {
      val trivial = List(
        TypeRepr.of[Any], TypeRepr.of[AnyRef], TypeRepr.of[AnyVal], TypeRepr.of[Matchable], TypeRepr.of[Product],
        TypeRepr.of[java.io.Serializable], TypeRepr.of[Equals]
      ).map(_.typeSymbol)
      val common = a.baseClasses.filter(c => !trivial.contains(c) && b.derivesFrom(c))
      val minimal = common.filterNot(c => common.exists(d => d != c && d.typeRef.derivesFrom(c)))
      minimal.map(c => joinArgs(a.baseType(c), b.baseType(c))) match {
        case Nil => OrType(a, b)
        case parts => parts.reduceLeft(AndType(_, _))
      }
    }
  }

  def joinArgs(using Quotes)(a: quotes.reflect.TypeRepr, b: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr = {
    import quotes.reflect.*
    (a, b) match {
      case _ if a =:= b => a
      case (AppliedType(tycon, as), AppliedType(_, bs)) if as.size == bs.size =>
        val params = tycon.typeSymbol.declaredTypes.filter(_.isTypeParam)
        val args = as.zip(bs).zipWithIndex.map { case ((x, y), i) =>
          val flags = params.lift(i).map(_.flags)
          if (x =:= y) x
          else if (flags.exists(_.is(Flags.Covariant))) join(x, y)
          else if (flags.exists(_.is(Flags.Contravariant))) AndType(x, y)
          else TypeBounds(AndType(lower(x), lower(y)), OrType(upper(x), upper(y)))
        }
        tycon.appliedTo(args)
      case _ => a
    }
  }

  def lower(using Quotes)(t: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr = {
    import quotes.reflect.*
    t match {
      case TypeBounds(lo, _) => lo
      case other => other
    }
  }

  def upper(using Quotes)(t: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr = {
    import quotes.reflect.*
    t match {
      case TypeBounds(_, hi) => hi
      case other => other
    }
  }
}

trait TheScalaCompat {
  transparent inline def apply[T](implicit t: T): t.type = t

  transparent inline def selectDynamic(inline tpeSelector: String): Any = ${ TheMacros.implicitlyImpl('tpeSelector) }
}

object TheMacros {
  def implicitlyImpl(tpeSelector: Expr[String])(using Quotes): Expr[Any] = {
    import quotes.reflect.*
    val tpeString = tpeSelector.valueOrAbort
    val tpe = TypeParser.parseTypeOrAbort(tpeString)
    Implicits.search(tpe) match {
      case s: ImplicitSearchSuccess => TypeParser.carrier(s.tree.tpe.widenTermRefByName, Some(s.tree))
      // As reported on Scala 2, where the type provider expands to `the.apply[T]`
      case _: ImplicitSearchFailure =>
        report.errorAndAbort(s"could not find implicit value for parameter t: ${tpe.show(using Printer.TypeReprShortCode)}")
    }
  }
}

/**
 * Provides the type of an expression via a type provider, eg. ``TypeOf.`Foo.mkFoo`.T``.
 *
 * The expression is typechecked at the call site, as on Scala 2, which depends on compiler internals. With a compiler
 * for which shapeless has no `CompilerShim`, only literals, stable paths (eg. `Foo.mkFoo`) and type ascriptions (eg.
 * `Foo.mkFoo: Foo`) are supported, names being resolved as described for [[TypeCarrier]].
 */
object TypeOf extends Dynamic {
  transparent inline def selectDynamic(inline code: String): Any = ${ TypeOfMacros.selectDynamic('code) }
}

object TypeOfMacros {
  def selectDynamic(code: Expr[String])(using Quotes): Expr[Any] = {
    import quotes.reflect.*
    val codeString = code.valueOrAbort
    val tpe = CompilerShims.optionally(_.typeCheck(codeString, isType = false)) match {
      case Some(typed) => typed.fold(report.errorAndAbort(_), _.widenTermRefByName)
      case None => parseTypeOf(codeString)
    }
    val symbol = tpe.typeSymbol
    if (symbol.isClassDef && (symbol == defn.IntClass || symbol == defn.BooleanClass || symbol == defn.UnitClass ||
      symbol == defn.LongClass || symbol == defn.DoubleClass || symbol == defn.FloatClass || symbol == defn.CharClass ||
      symbol == defn.ShortClass || symbol == defn.ByteClass) && !tpe.isSingleton)
      report.errorAndAbort(s"Primitive type ${tpe.show} may not be used in this context")
    TypeParser.carrier(tpe)
  }

  /** The type of a literal or stable path, possibly ascribed a type, for compilers with no `CompilerShim`. */
  def parseTypeOf(codeString: String)(using Quotes): quotes.reflect.TypeRepr = {
    import quotes.reflect.*
    val (exprString, ascription) = TypeParser.commaSeparated(codeString.replace(':', ',')) match {
      case List(e, t) if !codeString.contains("::") => (e, Some(t))
      case _ => (codeString, None)
    }
    val exprTpe = TypeParser.parseLiteral(exprString).map(lit => ConstantType(lit.constant))
      .orElse(TypeParser.resolveTermPath(exprString.trim).map(_.widenTermRefByName))
      .getOrElse(report.errorAndAbort(
        s"Unable to type expression $exprString (only literals and stable paths are supported with Scala ${
          CompilerShims.runningCompilerVersion.getOrElse("?")})"
      ))
    ascription.foreach { t =>
      val base = TypeParser.parseTypeOrAbort(t)
      if (!(exprTpe <:< base)) report.errorAndAbort(s"${exprTpe.show} does not conform to ${base.show}")
    }
    exprTpe
  }
}
