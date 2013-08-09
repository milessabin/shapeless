/*
 * Copyright (c) 2011 Miles Sabin 
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

/**
 * Record operations on `HList`'s with field-like elements.
 * 
 * @author Miles Sabin
 */
final class RecordOps[L <: HList](l : L) {
  import Record._

  /**
   * Returns the value associated with the singleton typed key k. Only available if this record has a field with
   * with keyType equal to the singleton type k.T.
   */
  def get(k: Witness)(implicit selector : FieldSelector[L, k.T]): selector.Out = selector(l)
  
  /**
   * Returns the value associated with the singleton typed key k. Only available if this record has a field with
   * with keyType equal to the singleton type k.T.
   *
   * Note that this can creates a bogus ambiguity with `HListOps#apply` as described in
   * https://issues.scala-lang.org/browse/SI-5142. If this method is accessible the conflict can be worked around by
   * using HListOps#at instead of `HListOps#apply`.
   */
  def apply(k: Witness)(implicit selector : FieldSelector[L, k.T]): selector.Out = selector(l)

  /**
   * Updates or adds to this record a field with key type F and value type F#valueType.
   */
  def updated[V](k: Witness, v: V)(implicit updater: FieldUpdater[L, FieldType[k.T, V]]) : updater.Out = updater(l, field[k.T](v))

  /**
   * Updates a field having a value with type A by given function.
   */
  def updateWith[A, B](k: Witness)(f: A => B)(implicit modifier: FieldModifier[L, k.T, A, B]): modifier.Out = modifier(l, f)

  /**
   * Remove the field associated with the singleton typed key k, returning both the corresponding value and the updated
   * record. Only available if this record has a field with keyType equal to the singleton type k.T.
   */
  def remove(k : Witness)(implicit remover: FieldRemover[L, k.T]): remover.Out = remover(l)
  
  /**
   * Updates or adds to this record a field of type F.
   */
  def +[F](f: F)(implicit updater : FieldUpdater[L, F]): updater.Out = updater(l, f)
  
  /**
   * Remove the field associated with the singleton typed key k, returning the updated record. Only available if this
   * record has a field with keyType equal to the singleton type k.T.
   */
  def -[V, Out <: HList](k: Witness)(implicit remover : FieldRemover.Aux[L, k.T, (V, Out)]): Out = remover(l)._2

  /**
   * Returns the keys of this record as an HList of singleton typed values.
   */
  def keys(implicit keys: Keys[L]): keys.Out = keys()

  /**
   * Returns an HList of the values of this record.
   */
  def values(implicit values: Values[L]): values.Out = values(l)
}

/**
 * Field with values of type `T`
 * 
 * @author Miles Sabin
 */
/*
trait Field[T] extends FieldAux {
  type valueType = T
}

trait FieldAux {
  type valueType
}
*/

trait FieldT[T] {
  type valueType = T
}

object Record {
  implicit def recordOps[L <: HList](l : L) : RecordOps[L] = new RecordOps(l)

  type FieldEntry[F <: FieldAux] = (F, F#valueType)

  trait KeyType[K, V] {
    type keyType = K
    type valueType = V
  }
  type FieldType[K, V] = V with KeyType[K, V]

  class FieldBuilder[K] {
    def apply[V](v : V): FieldType[K, V] = v.asInstanceOf[FieldType[K, V]]
  }
  
  def field[K] = new FieldBuilder[K]
}

/**
 * Type class supporting record field selection.
 * 
 * @author Miles Sabin
 */
trait FieldSelector[L <: HList, K] {
  type Out
  def apply(l : L): Out
}

trait LowPriorityFieldSelector {
  import Record.FieldType

  type Aux[L <: HList, K, Out0] = FieldSelector[L, K] { type Out = Out0 }

  implicit def hlistSelect[H, T <: HList, K]
    (implicit st : FieldSelector[T, K]): Aux[H :: T, K, st.Out] =
      new FieldSelector[H :: T, K] {
        type Out = st.Out
        def apply(l : H :: T): Out = st(l.tail)
      }
}

object FieldSelector extends LowPriorityFieldSelector {
  import Record.FieldType

  implicit def hlistSelect1[K, V, T <: HList]: Aux[FieldType[K, V] :: T, K, V] =
    new FieldSelector[FieldType[K, V] :: T, K] {
      type Out = V
      def apply(l : FieldType[K, V] :: T): Out = l.head
    }
}

/**
 * Type class supporting record update and extension.
 * 
 * @author Miles Sabin
 */
trait FieldUpdater[L <: HList, F] extends DepFn2[L, F] { type Out <: HList }

trait LowPriorityFieldUpdater {
  import Record.FieldType

  type Aux[L <: HList, F, Out0 <: HList] = FieldUpdater[L, F] { type Out = Out0 }
  
  implicit def hlistUpdater1[H, T <: HList, K, V]
    (implicit ut : FieldUpdater[T, FieldType[K, V]]): Aux[H :: T, FieldType[K, V], H :: ut.Out] =
      new FieldUpdater[H :: T, FieldType[K, V]] {
        type Out = H :: ut.Out
        def apply(l: H :: T, f: FieldType[K, V]): Out = l.head :: ut(l.tail, f)
      }
}

object FieldUpdater extends LowPriorityFieldUpdater {
  import Record.FieldType

  implicit def hnilUpdater[L <: HNil, F]: Aux[L, F, F :: HNil] =
    new FieldUpdater[L, F] {
      type Out = F :: HNil
      def apply(l: L, f: F): Out = f :: HNil
    }

  implicit def hlistUpdater2[K, V, T <: HList]: Aux[FieldType[K, V] :: T, FieldType[K, V], FieldType[K, V] :: T] =
    new FieldUpdater[FieldType[K, V] :: T, FieldType[K, V]] {
      type Out = FieldType[K, V] :: T
      def apply(l: FieldType[K, V] :: T, f: FieldType[K, V]): Out = f :: l.tail
    }
}

/**
 * Type class supporting modification of a record field by given function.
 * 
 * @author Joni Freeman
 */
@annotation.implicitNotFound(msg = "No field ${F} with value of type ${A} in record ${L}")
trait FieldModifier[L <: HList, F, A, B] {
  type Out
  def apply(l: L, f: A => B): Out
}

trait FieldModifierAux[L <: HList, F, A, B, Rem <: HList] {
  def apply(l: L, f: A => B): Rem
}

object FieldModifier {
  implicit def hlistModify[L <: HList, F, A, B, Rem <: HList](implicit aux: FieldModifierAux[L, F, A, B, Rem]) = new FieldModifier[L, F, A, B] {
    type Out = Rem
    def apply(l: L, f: A => B): Rem = aux(l, f)
  }
}

object FieldModifierAux {
  import Record.{FieldType, field}

  implicit def hlistModify1[F, A, B, T <: HList] = new FieldModifierAux[FieldType[F, A] :: T, F, A, B, FieldType[F, B] :: T] {
    def apply(l: FieldType[F, A] :: T, f: A => B): FieldType[F, B] :: T = field[F](f(l.head)) :: l.tail
  }
  
  implicit def hlistModify[H, T <: HList, F, A, B, Rem <: HList](implicit m: FieldModifierAux[T, F, A, B, Rem]) =
    new FieldModifierAux[H :: T, F, A, B, H :: Rem] {
      def apply(l: H :: T, f: A => B): H :: Rem = l.head :: m(l.tail, f)
    }
}

/**
 * Type class supporting record field removal.
 * 
 * @author Miles Sabin
 */
trait FieldRemover[L <: HList, K] extends DepFn1[L]

trait LowPriorityFieldRemover {
  import Record.FieldType

  type Aux[L <: HList, K, Out0] = FieldRemover[L, K] { type Out = Out0 }
  
  implicit def hlistRemove[H, T <: HList, K, V, OutT <: HList]
    (implicit rt: Aux[T, K, (V, OutT)]): Aux[H :: T, K, (V, H :: OutT)] =
      new FieldRemover[H :: T, K] {
        type Out = (V, H :: OutT)
        def apply(l : H :: T): Out = {
          val (v, tail) = rt(l.tail)
          (v, l.head :: tail)
        }
      }
}

object FieldRemover extends LowPriorityFieldRemover {
  import Record.FieldType


  implicit def hlistRemove1[K, V, T <: HList]: Aux[FieldType[K, V] :: T, K, (V, T)] =
    new FieldRemover[FieldType[K, V] :: T, K] {
      type Out = (V, T)
      def apply(l: FieldType[K, V] :: T): Out = (l.head, l.tail)
    }
}

trait Keys[L <: HList] extends DepFn0[L] { type Out <: HList }

object Keys {
  import Record.FieldType
  import Witness.WitnessEq

  type Aux[L <: HList, Out0 <: HList] = Keys[L] { type Out = Out0 }

  implicit def hnilKeys[L <: HNil]: Aux[L, HNil] =
    new Keys[L] {
      type Out = HNil
      def apply(): Out = HNil
    }

  implicit def hlistKeys[K, V, T <: HList](implicit wk: WitnessEq[K], kt: Keys[T]): Aux[FieldType[K, V] :: T, K :: kt.Out] =
    new Keys[FieldType[K, V] :: T] {
      type Out = K :: kt.Out
      def apply(): Out = wk.value :: kt()
    }
}

trait Values[L <: HList] extends DepFn1[L] { type Out <: HList }

object Values {
  import Record.FieldType
  type Aux[L <: HList, Out0 <: HList] = Values[L] { type Out = Out0 }

  implicit def hnilValues[L <: HNil]: Aux[L, HNil] =
    new Values[L] {
      type Out = HNil
      def apply(l: L): Out = HNil
    }

  implicit def hlistValues[K, V, T <: HList](implicit vt: Values[T]): Aux[FieldType[K, V] :: T, V :: vt.Out] =
    new Values[FieldType[K, V] :: T] {
      type Out = V :: vt.Out
      def apply(l: FieldType[K, V] :: T): Out = (l.head: V) :: vt(l.tail)
    }
}
