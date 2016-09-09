/*
 * Copyright (c) 2011-15 Miles Sabin 
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

import ops.hlist.Selector

/**
 * Type class witnessing that every element of `L` is of the form `FieldType[K, V]` where `K` is an element of `M`.
 */
trait KeyConstraint[L <: HList, M <: HList] extends Serializable

object KeyConstraint {
  import labelled._

  def apply[L <: HList, M <: HList](implicit kc: KeyConstraint[L, M]): KeyConstraint[L, M] = kc

  type Keys[M <: HList] = {
    type λ[L <: HList] = KeyConstraint[L, M] 
  }
  
  implicit def hnilKeys[M <: HList] = new KeyConstraint[HNil, M] {}
  implicit def hlistKeys[K, V, T <: HList, M <: HList]
    (implicit bct : KeyConstraint[T, M], sel : Selector[M, K]) = new KeyConstraint[FieldType[K, V] :: T, M] {}
}

/**
 * Type class witnessing that every element of `L` is of the form `FieldType[K, V]` where `V` is an element of `M`.
 */
trait ValueConstraint[L <: HList, M <: HList] extends Serializable

object ValueConstraint {
  import labelled._

  def apply[L <: HList, M <: HList](implicit vc: ValueConstraint[L, M]): ValueConstraint[L, M] = vc

  type Values[M <: HList] = {
    type λ[L <: HList] = ValueConstraint[L, M] 
  }
  
  implicit def hnilValues[M <: HList] = new ValueConstraint[HNil, M] {}
  implicit def hlistValues[K, V, T <: HList, M <: HList]
    (implicit bct : ValueConstraint[T, M], sel : Selector[M, V]) = new ValueConstraint[FieldType[K, V] :: T, M] {}
}