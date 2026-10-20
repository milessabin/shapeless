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
package ops
package record

import scala.quoted.*

// See `shapeless.Materialized` for why each type class has two macros

trait SelectorScalaCompat extends SelectorScalaCompat0 {
  transparent inline implicit def materialize[R <: HList, K, O]: record.Selector.Aux[R, K, O] =
    ${ RecordOpsMacros.selector[R, K, O] }

  transparent inline implicit def materializeSelector[R <: HList, K]: Materialized[record.Selector[R, K]] =
    ${ RecordOpsMacros.materializedSelector[R, K] }
}

trait SelectorScalaCompat0 {
  implicit def materializeAux[R <: HList, K, O](
    implicit selector: Materialized[record.Selector[R, K]], ev: selector.value.Out =:= O
  ): record.Selector.Aux[R, K, O] =
    ev.substituteCo[[o] =>> record.Selector[R, K] { type Out = o }](selector.value)
}

trait UpdaterScalaCompat extends UpdaterScalaCompat0 {
  transparent inline implicit def meterialize[L <: HList, F, O <: HList]: Updater.Aux[L, F, O] =
    ${ RecordOpsMacros.updater[L, F, O] }

  transparent inline implicit def materializeUpdater[L <: HList, F]: Materialized[Updater[L, F]] =
    ${ RecordOpsMacros.materializedUpdater[L, F] }
}

trait UpdaterScalaCompat0 {
  implicit def materializeAux[L <: HList, F, O <: HList](
    implicit updater: Materialized[Updater[L, F]], ev: updater.value.Out =:= O
  ): Updater.Aux[L, F, O] =
    ev.substituteCo[[o] =>> Updater[L, F] { type Out = o }](updater.value)
}

trait ModifierScalaCompat extends ModifierScalaCompat0 {
  transparent inline implicit def materialize[R <: HList, K, A, B, O <: HList]: Modifier.Aux[R, K, A, B, O] =
    ${ RecordOpsMacros.modifier[R, K, A, B, O] }

  transparent inline implicit def materializeModifier[R <: HList, K, A, B]: Materialized[Modifier[R, K, A, B]] =
    ${ RecordOpsMacros.materializedModifier[R, K, A, B] }
}

trait ModifierScalaCompat0 {
  implicit def materializeAux[R <: HList, K, A, B, O <: HList](
    implicit modifier: Materialized[Modifier[R, K, A, B]], ev: modifier.value.Out =:= O
  ): Modifier.Aux[R, K, A, B, O] =
    ev.substituteCo[[o] =>> Modifier[R, K, A, B] { type Out = o }](modifier.value)
}

trait RemoverScalaCompat extends RemoverScalaCompat0 {
  transparent inline implicit def materialize[R <: HList, K, V, O <: HList]: Remover.Aux[R, K, (V, O)] =
    ${ RecordOpsMacros.remover[R, K, V, O] }

  transparent inline implicit def materializeRemover[R <: HList, K]: Materialized[Remover[R, K]] =
    ${ RecordOpsMacros.materializedRemover[R, K] }
}

trait RemoverScalaCompat0 {
  implicit def materializeAux[R <: HList, K, V, O <: HList](
    implicit remover: Materialized[Remover[R, K]], ev: remover.value.Out =:= (V, O)
  ): Remover.Aux[R, K, (V, O)] =
    ev.substituteCo[[o] =>> Remover[R, K] { type Out = o }](remover.value)
}

trait LacksKeyScalaCompat {
  transparent inline implicit def materialize[R <: HList, K]: LacksKey[R, K] = ${ RecordOpsMacros.mkLacksKey[R, K] }
}

object RecordOpsMacros {
  def materializedSelector[R <: HList: Type, K: Type](using Quotes): Expr[Materialized[record.Selector[R, K]]] =
    MacroUtils.materialized(mkSelector[R, K])

  def materializedUpdater[L <: HList: Type, F: Type](using Quotes): Expr[Materialized[Updater[L, F]]] =
    MacroUtils.materialized(mkUpdater[L, F])

  def materializedModifier[R <: HList: Type, K: Type, A: Type, B: Type](using Quotes): Expr[Materialized[Modifier[R, K, A, B]]] =
    MacroUtils.materialized(mkModifier[R, K, A, B])

  def materializedRemover[R <: HList: Type, K: Type](using Quotes): Expr[Materialized[Remover[R, K]]] =
    MacroUtils.materialized(mkRemover[R, K])

  def selector[R <: HList: Type, K: Type, O: Type](using Quotes): Expr[record.Selector.Aux[R, K, O]] =
    mkSelector[R, K].asInstanceOf[Expr[record.Selector.Aux[R, K, O]]]

  def updater[L <: HList: Type, F: Type, O <: HList: Type](using Quotes): Expr[Updater.Aux[L, F, O]] =
    mkUpdater[L, F].asInstanceOf[Expr[Updater.Aux[L, F, O]]]

  def modifier[R <: HList: Type, K: Type, A: Type, B: Type, O <: HList: Type](using Quotes): Expr[Modifier.Aux[R, K, A, B, O]] =
    mkModifier[R, K, A, B].asInstanceOf[Expr[Modifier.Aux[R, K, A, B, O]]]

  def remover[R <: HList: Type, K: Type, V: Type, O <: HList: Type](using Quotes): Expr[Remover.Aux[R, K, (V, O)]] =
    mkRemover[R, K].asInstanceOf[Expr[Remover.Aux[R, K, (V, O)]]]

  def recordFields[R: Type](using Quotes): List[quotes.reflect.TypeRepr] = {
    import quotes.reflect.*
    val record = TypeRepr.of[R].dealias
    if (!(record <:< TypeRepr.of[HList])) report.errorAndAbort(s"${record.show} is not a record type")
    MacroUtils.unpackHList(record)
  }

  def cast[T: Type](using Quotes)(value: Expr[Any], tpe: quotes.reflect.TypeRepr): Expr[T] =
    tpe.asType match {
      case '[t] => '{ $value.asInstanceOf[t] }.asExprOf[T]
    }

  def mkSelector[R <: HList: Type, K: Type](using Quotes): Expr[record.Selector[R, K]] = {
    import quotes.reflect.*
    val key = TypeRepr.of[K].dealias
    MacroUtils.findField(recordFields[R], key) match {
      case Some((k, v, i)) =>
        val auxTpe = Refinement(TypeRepr.of[record.Selector].appliedTo(List(TypeRepr.of[R], k)), "Out", TypeBounds(v, v))
        cast[record.Selector[R, K]]('{ new UnsafeSelector(${ Expr(i) }) }, auxTpe)
      case None =>
        report.errorAndAbort(s"No field ${key.show} in record type ${TypeRepr.of[R].show}")
    }
  }

  def mkUpdater[L <: HList: Type, F: Type](using Quotes): Expr[Updater[L, F]] = {
    import quotes.reflect.*
    val element = TypeRepr.of[F].dealias
    val elements = recordFields[L]
    val i = elements.indexWhere(_ =:= element)
    val (updated, idx) =
      if (i < 0) (elements :+ element, elements.length)
      else (elements.updated(i, element), i)
    val out = MacroUtils.mkHListTpe(updated)
    val auxTpe = Refinement(TypeRepr.of[Updater].appliedTo(List(TypeRepr.of[L], element)), "Out", TypeBounds(out, out))
    cast[Updater[L, F]]('{ new UnsafeUpdater(${ Expr(idx) }) }, auxTpe)
  }

  def mkModifier[R <: HList: Type, K: Type, A: Type, B: Type](using Quotes): Expr[Modifier[R, K, A, B]] = {
    import quotes.reflect.*
    val key = TypeRepr.of[K].dealias
    val fields = recordFields[R]
    MacroUtils.findField(fields, key) match {
      case Some((k, v, i)) if v <:< TypeRepr.of[A] =>
        val out = MacroUtils.mkHListTpe(fields.updated(i, MacroUtils.mkFieldTpe(k, TypeRepr.of[B])))
        val auxTpe = Refinement(
          TypeRepr.of[Modifier].appliedTo(List(TypeRepr.of[R], k, TypeRepr.of[A], TypeRepr.of[B])), "Out", TypeBounds(out, out)
        )
        cast[Modifier[R, K, A, B]]('{ new UnsafeModifier(${ Expr(i) }) }, auxTpe)
      case _ =>
        report.errorAndAbort(s"No field ${key.show} in record type ${TypeRepr.of[R].show}")
    }
  }

  def mkRemover[R <: HList: Type, K: Type](using Quotes): Expr[Remover[R, K]] = {
    import quotes.reflect.*
    val key = TypeRepr.of[K].dealias
    val fields = recordFields[R]
    MacroUtils.findField(fields, key) match {
      case Some((k, v, i)) =>
        val (prefix, suffix) = fields.splitAt(i)
        val rest = MacroUtils.mkHListTpe(prefix ++ suffix.tail)
        val out = TypeRepr.of[Tuple2].appliedTo(List(v, rest))
        val auxTpe = Refinement(TypeRepr.of[Remover].appliedTo(List(TypeRepr.of[R], k)), "Out", TypeBounds(out, out))
        cast[Remover[R, K]]('{ new UnsafeRemover(${ Expr(i) }) }, auxTpe)
      case None =>
        report.errorAndAbort(s"No field ${key.show} in record type ${TypeRepr.of[R].show}")
    }
  }

  def mkLacksKey[R <: HList: Type, K: Type](using Quotes): Expr[LacksKey[R, K]] = {
    import quotes.reflect.*
    val key = TypeRepr.of[K].dealias
    MacroUtils.findField(recordFields[R], key) match {
      case None => '{ new LacksKey[R, K] }
      case _ => report.errorAndAbort(s"Record type ${TypeRepr.of[R].show} contains field ${key.show}")
    }
  }
}
