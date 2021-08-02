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

import shapeless.labelled.FieldType

import scala.compiletime.ops.int.S

type FindField[R <: HList, K] = FindField0[R, K, 0]
type FindField0[R <: HList, K, I <: Int] <: (Any, Int) = R match {
  case FieldType[K, f] :: t => (f, I)
  case _ :: t => FindField0[t, K, S[I]]
}

trait SelectorScalaCompat {

  given[R <: HList, K](
    using idx: ValueOf[scala.Tuple.Elem[FindField[R, K], 1]]
  ): Selector.Aux[R, K, scala.Tuple.Head[FindField[R, K]]] =
    new UnsafeSelector(idx.value).asInstanceOf[Selector.Aux[R, K, scala.Tuple.Head[FindField[R, K]]]]
}

type IndexOf[L <: HList, E] = IndexOf0[L, E, 0]
type IndexOf0[L <: HList, E, I <: Int] <: Int = L match {
  case E :: _ => I
  case _ :: t => IndexOf0[t, E, S[I]]
  case HNil => -1
}

type Append[L <: HList, E] <: HList = L match {
  case h :: t => h :: Append[t, E]
  case HNil => E :: HNil
}

type IfEq[A, B, IfTrue, IfFalse] <: IfTrue | IfFalse = A match {
  case B => IfTrue
  case _ => IfFalse
}

trait UpdaterScalaCompat {

  given [L <: HList, F](
    using idx: ValueOf[IndexOf[L, F]]
  ): Updater.Aux[L, F, IfEq[IndexOf[L, F], -1, Append[L, F], L]] =
    new UnsafeUpdater(idx.value).asInstanceOf[Updater.Aux[L, F, IfEq[IndexOf[L, F], -1, Append[L, F], L]]]
}

type ReplaceField[R <: HList, K, B] <: HList = R match {
  case FieldType[K, _] :: t => FieldType[K, B] :: t
  case h :: t => h :: ReplaceField[t, K, B]
  case HNil => HNil
}

trait ModifierScalaCompat {
  given [R <: HList, K, A, B](
    using ev: scala.Tuple.Head[FindField[R, K]] <:< A,
    idx: ValueOf[scala.Tuple.Elem[FindField[R, K], 1]]
  ): Modifier.Aux[R, K, A, B, ReplaceField[R, K, B]] =
    new UnsafeModifier(idx.value).asInstanceOf[Modifier.Aux[R, K, A, B, ReplaceField[R, K, B]]]
}

type ReversePrependHList[L <: HList, M <: HList] <: HList = L match {
  case HNil => M
  case h :: t => ReversePrependHList[t, h :: M]
}

type RemoveField[R <: HList, K] = RemoveField0[R, K, 0, HNil]
type RemoveField0[R <: HList, K, I <: Int, Acc <: HList] <: (Int, Any, HList) = R match {
  case FieldType[K, f] :: t => (I, f, ReversePrependHList[Acc, t])
  case h :: t => RemoveField0[t, K, S[I], h :: Acc]
}

trait RemoverScalaCompat {
  given [R <: HList, K](
    using idx: ValueOf[scala.Tuple.Head[RemoveField[R, K]]]
  ): Remover.Aux[R, K, scala.Tuple.Tail[RemoveField[R, K]]] =
    new UnsafeRemover(idx.value).asInstanceOf[Remover.Aux[R, K, scala.Tuple.Tail[RemoveField[R, K]]]]
}

type HasNoKey[R <: HList, K] <: Boolean = R match {
  case FieldType[K, _] :: _ => false
  case _ :: t => HasNoKey[t, K]
  case HNil => true
}

trait LacksKeyScalaCompat {
  given [R <: HList, K](using HasNoKey[R, K] =:= true): LacksKey[R, K] = new LacksKey[R, K]
}
