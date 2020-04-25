/*
 * Copyright (c) 2014-16 Miles Sabin
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

import scala.reflect.macros.whitebox

object labelled {
  /**
   * The type of fields with keys of singleton type `K` and value type `V`.
   */
  type FieldType[K, +V] = V with KeyTag[K, V]
  trait KeyTag[K, +V] extends Any

  /**
   * Yields a result encoding the supplied value with the singleton type `K` of its key.
   */
  def field[K] = new FieldBuilder[K]

  class FieldBuilder[K] {
    def apply[V](v : V): FieldType[K, V] = v.asInstanceOf[FieldType[K, V]]
  }
}

trait DefaultSymbolicLabelling[T] extends DepFn0 with Serializable { type Out <: HList }

object DefaultSymbolicLabelling {
  type Aux[T, Out0] = DefaultSymbolicLabelling[T] { type Out = Out0 }

  def apply[T](implicit lab: DefaultSymbolicLabelling[T]): Aux[T, lab.Out] = lab

  implicit def mkDefaultSymbolicLabelling[T]: DefaultSymbolicLabelling[T] =
    macro LabelledMacros.mkDefaultSymbolicLabellingImpl[T]

  def instance[T, L <: HList](labels: L): Aux[T, L] =
    new DefaultSymbolicLabelling[T] {
      type Out = L
      def apply(): L = labels
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
      val value: (A :: HNil) => FieldType[T, Res] =
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

  type F = FieldType[this.type, V]

  def ->>(v: V): FieldType[this.type, V] = field[this.type](v)
}

class LabelledMacros(val c: whitebox.Context) extends SingletonTypeUtils with CaseClassMacros {
  import c.universe._

  def mkDefaultSymbolicLabellingImpl[T](implicit tTag: WeakTypeTag[T]): Tree = {
    val tTpe = weakTypeOf[T]
    val labels: List[String] =
      if (isProduct(tTpe)) fieldsOf(tTpe).map(f => nameAsString(f._1))
      else if (isCoproduct(tTpe)) ctorsOf(tTpe).map(tpe => nameAsString(nameOf(tpe)))
      else c.abort(c.enclosingPosition, s"$tTpe is not case class like or the root of a sealed family of types")

    val labelTpes = labels.map(SingletonSymbolType(_))
    val labelValues = labels.map(mkSingletonSymbol)

    val labelsTpe = mkHListTpe(labelTpes)
    val labelsValue = mkHListValue(labelValues)

    val defaultSymbolicLabelling = objectRef[DefaultSymbolicLabelling.type]
    q"$defaultSymbolicLabelling.instance[$tTpe, $labelsTpe]($labelsValue)"
  }

  def recordTypeImpl(tpeSelector: Tree): Tree =
    labelledTypeImpl(tpeSelector, "record", hnilTpe, hconsTpe)

  def unionTypeImpl(tpeSelector: Tree): Tree =
    labelledTypeImpl(tpeSelector, "union", cnilTpe, cconsTpe)

  def labelledTypeImpl(tpeSelector: Tree, variety: String, nilTpe: Type, consTpe: Type): Tree = {
    def mkFieldTpe(keyTpe: Type, valueTpe: Type): Type =
      appliedType(fieldTypeTpe, List(keyTpe, valueTpe))

    val q"${tpeString: String}" = (tpeSelector: @unchecked)
    val fields =
      if (tpeString.trim.isEmpty)
        Array.empty[(Type, Type)]
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

          case _ =>
            c.abort(c.enclosingPosition, s"Malformed $variety type $tpeString")
        }

    val labelledTpe =
      fields.foldRight(nilTpe) { case ((keyTpe, valueTpe), acc) =>
        val fieldTpe = mkFieldTpe(keyTpe, valueTpe)
        appliedType(consTpe, List(fieldTpe, acc))
      }

    typeCarrier(labelledTpe)
  }

  def hlistTypeImpl(tpeSelector: Tree): Tree =
    nonLabelledTypeImpl(tpeSelector, "hlist", hnilTpe, hconsTpe)

  def coproductTypeImpl(tpeSelector: Tree): Tree =
    nonLabelledTypeImpl(tpeSelector, "coproduct", cnilTpe, cconsTpe)

  def nonLabelledTypeImpl(tpeSelector: Tree, variety: String, nilTpe: Type, consTpe: Type): Tree = {
    val q"${tpeString: String}" = (tpeSelector: @unchecked)
    val elemTypes =
      if (tpeString.trim.isEmpty)
        Array.empty[Type]
      else
        tpeString.split(",").map(_.trim).map { elemTypeStr =>
          parseType(elemTypeStr)
            .getOrElse(c.abort(c.enclosingPosition, s"Malformed literal or standard type $elemTypeStr"))
        }

    val tpe =
      elemTypes.foldRight(nilTpe) { case (elemTpe, acc) =>
        appliedType(consTpe, List(elemTpe, acc))
      }

    typeCarrier(tpe)
  }
}
