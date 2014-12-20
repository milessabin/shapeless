/*
 * Copyright (c) 2014 Miles Sabin
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

import scala.reflect.macros.whitebox

object labelled {
  /**
   * The type of fields with keys of singleton type `K` and value type `V`.
   */
  type FieldType[K, V] = V with KeyTag[K, V]
  trait KeyTag[K, V]

  /**
   * Yields a result encoding the supplied value with the singleton type `K' of its key.
   */
  def field[K] = new FieldBuilder[K]

  class FieldBuilder[K] {
    def apply[V](v : V): FieldType[K, V] = v.asInstanceOf[FieldType[K, V]]
  }
}

/**
 * Polymorphic function that allows modifications on record fields while preserving the
 * original key types.
 *
 * @author Dario Rexin
 */
trait FieldPoly extends Poly1 {
  import labelled._

  class FieldCaseBuilder[A, T] {
    def apply[Res](fn: A => Res) = new Case[FieldType[T, A]] {
      type Result = FieldType[T, Res]
      val value: Function1[A :: HNil, FieldType[T, Res]] =
        (l: A :: HNil) => field[T](fn(l.head))
    }
  }

  def atField[A](w: Witness) = new FieldCaseBuilder[A, w.T]
}

/**
 * Field with values of type `V`.
 *
 * Record keys of this form should be objects which extend this trait. Keys may also be arbitrary singleton typed
 * values, however keys of this form enforce the type of their values.
 *
 * @author Miles Sabin
 */
trait FieldOf[V] {
  import labelled._

  def ->>(v: V): FieldType[this.type, V] = field[this.type](v)
}

class LabelledMacros(val c: whitebox.Context) extends SingletonTypeUtils {
  import labelled._
  import c.universe._

  val hconsTpe = typeOf[::[_, _]].typeConstructor
  val hnilTpe = typeOf[HNil]
  val cconsTpe = typeOf[:+:[_, _]].typeConstructor
  val cnilTpe = typeOf[CNil]
  val fieldTypeTpe = typeOf[FieldType[_, _]].typeConstructor

  def recordTypeImpl(tpeSelector: c.Tree): c.Tree =
    labelledTypeImpl(tpeSelector, "record", hnilTpe, hconsTpe)

  def unionTypeImpl(tpeSelector: c.Tree): c.Tree =
    labelledTypeImpl(tpeSelector, "union", cnilTpe, cconsTpe)

  def labelledTypeImpl(tpeSelector: c.Tree, variety: String, nilTpe: Type, consTpe: Type): c.Tree = {
    def mkFieldTpe(keyTpe: Type, valueTpe: Type): Type =
      appliedType(fieldTypeTpe, List(keyTpe, valueTpe))

    val q"${tpeString: String}" = tpeSelector
    val fields =
      if (tpeString.trim.isEmpty)
        Array.empty[(c.Type, c.Type)]
      else
        tpeString.split(",").map(_.trim).map(_.split("->").map(_.trim)).map {
          case Array(key, value) =>
            val keyTpe = 
              parseLiteralType(key)
                .getOrElse(c.abort(c.enclosingPosition, s"Malformed literal type $key"))
            
            val valueTpe =
              parseType(value)
                .getOrElse(c.abort(c.enclosingPosition, s"Malformed literal or standard type $value"))
            
            (keyTpe, valueTpe)
            
          case other =>
            c.abort(c.enclosingPosition, s"Malformed $variety type $tpeString")
        }

    val labelledTpe =
      fields.foldRight(nilTpe) { case ((keyTpe, valueTpe), acc) =>
        val fieldTpe = mkFieldTpe(keyTpe, valueTpe)
        appliedType(consTpe, List(fieldTpe, acc))
      }

    typeCarrier(labelledTpe)
  }
}
