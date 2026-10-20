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

import scala.language.implicitConversions
import scala.quoted.*

trait WitnessScalaCompat {
  transparent inline def selectDynamic(inline tpeSelector: String): Any =
    ${ SingletonTypeMacros.witnessTypeImpl('tpeSelector) }
}

trait NatWithScalaCompat extends NatWithScalaCompat0 {
  transparent inline implicit def apply[TC[_ <: Nat]](inline i: Int): NatWith[TC] =
    ${ SingletonTypeMacros.convertInstanceImplNat[TC]('i) }
}

trait NatWithScalaCompat0 {
  transparent inline implicit def apply2[B, T <: B, TC[_ <: B, _ <: Nat]](inline i: Int): NatWith[[t <: Nat] =>> TC[T, t]] =
    ${ SingletonTypeMacros.convertInstanceImplNat1[B, T, TC]('i) }
}

trait WidenScalaCompat extends WidenScalaCompat0 {
  implicit def apply1[TC[_], T](t: T)(implicit tc: TC[t.type]): WitnessWith.Lt[TC, T] =
    WitnessWith.instance[TC, t.type](t, tc)

  // See `Materialized` for why there are two macros
  transparent inline implicit def materialize[T, Out >: T]: Widen.Aux[T, Out] =
    ${ SingletonTypeMacros.materializeWidenAux[T, Out] }

  transparent inline implicit def materializeWiden[T]: Materialized[Widen[T]] =
    ${ SingletonTypeMacros.materializedWiden[T] }
}

trait WidenScalaCompat0 {
  implicit def materializeAux[T, Out >: T](implicit widen: Materialized[Widen[T]], ev: widen.value.Out =:= Out): Widen.Aux[T, Out] =
    ev.substituteCo[[o] =>> Widen[T] { type Out = o }](widen.value)
}

object SingletonTypeMacros {
  def witnessTypeImpl(tpeSelector: Expr[String])(using Quotes): Expr[Any] = {
    import quotes.reflect.*
    val str = tpeSelector.valueOrAbort
    val lit = TypeParser.parseLiteral(str).getOrElse(report.errorAndAbort(s"Malformed literal $str"))
    val tpe = ConstantType(lit.constant)
    val witness = tpe.asType match {
      case '[t] => '{ Witness.mkWitness[t](${ lit.asExprOf[t] }) }
    }
    val witnessTpe = Refinement(TypeRepr.of[Witness], "T", TypeBounds(tpe, tpe))
    TypeParser.carrier(AndType(witnessTpe, TypeParser.fieldTypeCarrier(tpe)), Some(witness.asTerm))
  }

  def convertInstanceImplNat[TC[_ <: Nat]: Type](i: Expr[Int])(using Quotes): Expr[NatWith[TC]] = {
    import quotes.reflect.*
    val tc = TypeRepr.of[TC]
    convertInstanceImplNatAux(i, tc, n => tc.appliedTo(n)).asExprOf[NatWith[TC]]
  }

  def convertInstanceImplNat1[B: Type, T <: B: Type, TC[_ <: B, _ <: Nat]: Type](i: Expr[Int])(using Quotes): Expr[NatWith[[t <: Nat] =>> TC[T, t]]] = {
    import quotes.reflect.*
    val tc = TypeRepr.of[TC]
    val t = TypeRepr.of[T]
    val lambda = TypeLambda(List("t"), _ => List(TypeBounds.upper(TypeRepr.of[Nat])), tl => tc.appliedTo(List(t, tl.param(0))))
    convertInstanceImplNatAux(i, lambda, n => tc.appliedTo(List(t, n))).asInstanceOf[Expr[NatWith[[t <: Nat] =>> TC[T, t]]]]
  }

  def convertInstanceImplNatAux(using Quotes)(
    i: Expr[Int], tcTpe: quotes.reflect.TypeRepr, applyTc: quotes.reflect.TypeRepr => quotes.reflect.TypeRepr
  ): Expr[Any] = {
    import quotes.reflect.*
    val nTpe = NatMacros.mkNatTpe(NatMacros.natLiteral(i))
    val instTpe = applyTc(nTpe)
    val inst = Implicits.search(instTpe) match {
      case s: ImplicitSearchSuccess => s.tree
      case f: ImplicitSearchFailure => report.errorAndAbort(s"Unable to resolve implicit value of type ${instTpe.show}")
    }
    val natWith = Ref(Symbol.requiredModule("shapeless.NatWith"))
    val ctor = if (instTpe <:< TypeRepr.of[AnyRef]) "depInstance" else "instance"
    Select.unique(natWith, ctor).appliedToTypes(List(tcTpe, nTpe)).appliedTo(inst).asExpr
  }

  def materializeWidenAux[T: Type, Out >: T: Type](using Quotes): Expr[Widen.Aux[T, Out]] =
    materializeWiden[T].asInstanceOf[Expr[Widen.Aux[T, Out]]]

  def materializedWiden[T: Type](using Quotes): Expr[Materialized[Widen[T]]] =
    MacroUtils.materialized(materializeWiden[T])

  def materializeWiden[T: Type](using Quotes): Expr[Widen[T]] = {
    import quotes.reflect.*
    val tpe = MacroUtils.normalize(TypeRepr.of[T])
    val wideTpe = tpe.widen
    if (wideTpe =:= tpe) report.errorAndAbort(s"Don't know how to widen ${tpe.show}")
    val widen = Ref(Symbol.requiredModule("shapeless.Widen"))
    val identity = '{ (t: T) => t }.asTerm
    Select.unique(widen, "instance").appliedToTypes(List(tpe, wideTpe)).appliedTo(identity).asExprOf[Widen[T]]
  }
}
