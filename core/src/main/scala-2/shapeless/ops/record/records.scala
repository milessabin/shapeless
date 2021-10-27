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

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

trait SelectorScalaCompat {

  implicit def materialize[R <: HList, K, O]: Selector.Aux[R, K, O] =
    macro SelectorMacros.materialize[R, K]
}

class SelectorMacros(val c: whitebox.Context) extends CaseClassMacros {
  import c.universe._

  def materialize[R <: HList: WeakTypeTag, K: WeakTypeTag]: Tree = {
    val record = weakTypeOf[R].dealias
    val key = weakTypeOf[K].dealias
    if (!(record <:< hlistTpe))
      abort(s"$record is not a record type")

    findField(record, key) match {
      case Some((k, v, i)) =>
        q"new ${typeOf[UnsafeSelector]}($i).asInstanceOf[${reify(Selector)}.Aux[$record, $k, $v]]"
      case _ =>
        abort(s"No field $key in record type $record")
    }
  }
}

trait UpdaterScalaCompat {

  implicit def meterialize[L <: HList, F, O]: Updater.Aux[L, F, O] =
    macro UpdaterMacros.materialize[L, F]
}

class UpdaterMacros(val c: whitebox.Context) extends CaseClassMacros {
  import c.universe._

  def materialize[L <: HList: WeakTypeTag, E: WeakTypeTag]: Tree = {
    val list = weakTypeOf[L].dealias
    val element = weakTypeOf[E].dealias
    if (!(list <:< hlistTpe))
      abort(s"$list is not a record type")

    val (updated, i) = {
      val elements = unpackHList(list)
      val i = elements.indexWhere(_ =:= element)
      if (i < 0) (elements :+ element, elements.length)
      else (elements.updated(i, element), i)
    }

    q"new ${typeOf[UnsafeUpdater]}($i).asInstanceOf[${reify(Updater)}.Aux[$list, $element, ${mkHListTpe(updated)}]]"
  }
}

trait ModifierScalaCompat {
  implicit def materialize[R <: HList, K, A, B, O <: HList]: Modifier.Aux[R, K, A, B, O] =
    macro ModifierMacros.materialize[R, K, A, B]
}

class ModifierMacros(val c: whitebox.Context) extends CaseClassMacros {
  import c.universe._

  def materialize[R <: HList: WeakTypeTag, K: WeakTypeTag, A: WeakTypeTag, B: WeakTypeTag]: Tree = {
    val record = weakTypeOf[R].dealias
    val key = weakTypeOf[K].dealias
    if (!(record <:< hlistTpe))
      abort(s"$record is not a record type")

    val a = weakTypeOf[A]
    val b = weakTypeOf[B]
    val fields = unpackHList(record)
    findField(fields, key) match {
      case Some((k, v, i)) if v <:< a =>
        val out = mkHListTpe(fields.updated(i, FieldType(k, b)))
        q"new ${typeOf[UnsafeModifier]}($i).asInstanceOf[${reify(Modifier)}.Aux[$record, $k, $a, $b, $out]]"
      case _ =>
        abort(s"No field $key in record type $record")
    }
  }
}

trait RemoverScalaCompat {
  implicit def materialize[R <: HList, K, V, O <: HList]: Remover.Aux[R, K, (V, O)] =
    macro RemoverMacros.materialize[R, K]
}

class RemoverMacros(val c: whitebox.Context) extends CaseClassMacros {
  import c.universe._

  def materialize[R <: HList: WeakTypeTag, K: WeakTypeTag]: Tree = {
    val record = weakTypeOf[R].dealias
    val key = weakTypeOf[K].dealias
    if (!(record <:< hlistTpe))
      abort(s"$record is not a record type")

    val fields = unpackHList(record)
    findField(fields, key) match {
      case Some((k, v, i)) =>
        val (prefix, suffix) = fields.splitAt(i)
        val out = mkHListTpe(prefix ++ suffix.tail)
        q"new ${typeOf[UnsafeRemover]}($i).asInstanceOf[${reify(Remover)}.Aux[$record, $k, ($v, $out)]]"
      case _ =>
        abort(s"No field $key in record type $record")
    }
  }
}

trait LacksKeyScalaCompat {
  implicit def materialize[R <: HList, K]: LacksKey[R, K] =
    macro LacksKeyMacros.materialize[R, K]
}

class LacksKeyMacros(val c: whitebox.Context) extends CaseClassMacros {
  import c.universe._

  def materialize[R <: HList: WeakTypeTag, K: WeakTypeTag]: Tree = {
    val record = weakTypeOf[R].dealias
    val key = weakTypeOf[K].dealias
    if (!(record <:< hlistTpe))
      abort(s"$record is not a record type")

    findField(record, key) match {
      case None => q"new ${weakTypeOf[LacksKey[R, K]]}"
      case _ => abort(s"Record type $record contains field $key")
    }
  }
}
