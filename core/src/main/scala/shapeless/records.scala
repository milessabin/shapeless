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
   * Returns the value associate with the field F. Only available of this record has a field with key type F.
   */
  def get[F <: FieldAux](f : F)(implicit selector : Selector[L, FieldEntry[F]]) : F#valueType = selector(l)._2
  
  // Creates a bogus ambiguity with HListOps#apply. Will be Reinstated once https://issues.scala-lang.org/browse/SI-5142
  // is fixed. Use get for now.
  // def apply[F <: FieldAux](f : F)(implicit selector : Selector[L, FieldEntry[F]]) : F#valueType = selector(l)._2

  /**
   * Updates or adds to this record a field with key type F and value type F#valueType.
   */
  def updated[V, F <: Field[V]](f : F, v : V)(implicit updater : Updater[L, F, V]) : updater.Out = updater(l, f, v)

  /**
   * Remove the first field with key type F from this record, returning both the corresponding value and the updated
   * record.
   */
  def remove[F <: FieldAux](f : F)(implicit remove : Remove[FieldEntry[F], L]) : (F#valueType, remove.Out) = {
    val ((f, v), r) = remove(l)
    (v, r)
  }
  
  /**
   * Updates or adds to this record a field with key type F and value type F#valueType.
   */
  def +[V, F <: Field[V]](fv : (F, V))(implicit updater : Updater[L, F, V]) : updater.Out = updater(l, fv._1, fv._2)
  
  /**
   * Remove the first field with key type F from this record, returning both the corresponding value and the updated
   * record.
   */
  def -[F <: FieldAux](f : F)(implicit remove : Remove[FieldEntry[F], L]) : remove.Out = remove(l)._2
}

/**
 * Field with values of type `T`
 * 
 * @author Miles Sabin
 */
trait Field[T] extends FieldAux {
  type valueType = T
}

trait FieldAux {
  type valueType
}

object Record {
  implicit def recordOps[L <: HList](l : L) : RecordOps[L] = new RecordOps(l)

  type FieldEntry[F <: FieldAux] = (F, F#valueType)
}

/**
 * Type class supporting record update and extension.
 * 
 * @author Miles Sabin
 */
trait Updater[L <: HList, F <: FieldAux, V] {
  type Out <: HList
  def apply(l : L, f : F, v : V) : Out
}

trait UpdaterAux[L <: HList, F <: FieldAux, V, Out <: HList] {
  def apply(l : L, f : F, v : V) : Out
}

object Updater {
  implicit def updater[L <: HList, F <: FieldAux, V, Out0 <: HList](implicit updater : UpdaterAux[L, F, V, Out0]) =
    new Updater[L, F, V] {
      type Out = Out0
      def apply(l : L, f : F, v : V) : Out = updater(l, f, v)
    }
}

trait LowPriorityUpdaterAux {
  implicit def hlistUpdater1[L <: HList, F <: FieldAux, V] = new UpdaterAux[L, F, V, (F, V) :: L] {
    def apply(l : L, f : F, v : V) : (F, V) :: L = (f -> v) :: l
  }
}

object UpdaterAux extends LowPriorityUpdaterAux {
  implicit def hlistUpdater2[T <: HList, F <: FieldAux, V] = new UpdaterAux[(F, V) :: T, F, V, (F, V) :: T] {
    def apply(l : (F, V) :: T, f : F, v : V) : (F, V) :: T = (f -> v) :: l.tail
  }
  
  implicit def hlistUpdater3[H, T <: HList, F <: FieldAux, V, Out <: HList](implicit ut : UpdaterAux[T, F, V, Out]) =
    new UpdaterAux[H :: T, F, V, H :: Out] {
      def apply(l : H :: T, f : F, v : V) : H :: Out = l.head :: ut(l.tail, f, v)
    }
}