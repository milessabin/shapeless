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

object labelled {

  /** The type of fields with keys of singleton type `K` and value type `V`. */
  type FieldType[K, +V] = V with KeyTag[K, V]
  type KeyTag[K, +V]

  type ->>[K, +V] = FieldType[K, V]

  /** Yields a result encoding the supplied value with the singleton type `K` of its key. */
  def field[K]: FieldBuilder[K] = new FieldBuilder(true)
  class FieldBuilder[K](private val dummy: Boolean) extends AnyVal {
    def apply[V](v: V): FieldType[K, V] = v.asInstanceOf[FieldType[K, V]]
  }
}

trait Labelling[T] extends DepFn0 with Serializable { type Out <: HList }

object Labelling extends LabellingScalaCompat {
  type Aux[T, Out0] = Labelling[T] { type Out = Out0 }
  def apply[T](implicit lab: Labelling[T]): Aux[T, lab.Out] = lab

  def instance[T, L <: HList](labels: L): Aux[T, L] = new Labelling[T] {
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

  final class FieldCaseBuilder[A, T] {
    def apply[Res](fn: A => Res): Case.Aux[FieldType[T, A], FieldType[T, Res]] =
      new Case[FieldType[T, A]] {
        type Result = FieldType[T, Res]
        val value: (A :: HNil) => FieldType[T, Res] =
          l => field[T](fn(l.head))
      }
  }

  def atField[A](w: Witness): FieldCaseBuilder[A, w.T] =
    new FieldCaseBuilder
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
