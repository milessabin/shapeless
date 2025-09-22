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
import scala.util.NotGiven

trait SelectorScalaCompat {
  trait FindField[R <: HList, K] {
    type Type
    type Idx <: Int

    def idx: Idx
  }

  object FindField {
    type Aux[R <: HList, K, T, I] = FindField[R, K] {type Type = T; type Idx = I}

    given [T <: HList, K, F]: FindField.Aux[FieldType[K, F] :: T, K, F, 0] = new FindField[FieldType[K, F] :: T, K] {
      type Type = F
      type Idx = 0

      def idx: Idx = 0
    }

    given [T <: HList, F, K, K2](using ev: NotGiven[K =:= K2], rec: FindField[T, K]): FindField.Aux[FieldType[K2, F] :: T, K, rec.Type, S[rec.Idx]] = new FindField[FieldType[K2, F] :: T, K] {
      type Type = rec.Type
      type Idx = S[rec.Idx]

      def idx: S[rec.Idx] = (1 + rec.idx).asInstanceOf[S[rec.Idx]]
    }
  }

  given[R <: HList, K, T, I <: Int](
    using find: FindField.Aux[R, K, T, I]
  ): Selector.Aux[R, K, T] =
    new UnsafeSelector(find.idx).asInstanceOf[Selector.Aux[R, K, T]]
}

trait UpdaterScalaCompat {
  type Append[L <: HList, E] <: HList = L match {
    case h :: t => h :: Append[t, E]
    case HNil => E :: HNil
  }

  type IfEq[A, B, IfTrue, IfFalse] <: IfTrue | IfFalse = A match {
    case B => IfTrue
    case _ => IfFalse
  }
  
  trait IndexOf[L <: HList, E] {
    type Idx <: Int
    type Found <: Boolean
    def idx: Idx
    def found: Found
  }
  object IndexOf {
    type Aux[L <: HList, E, I <: Int, F <: Boolean] = IndexOf[L, E] {type Idx = I; type Found = F}

    given [T <: HList, E]: IndexOf.Aux[E :: T, E, 0, true] = new IndexOf[E :: T, E] {
      type Idx = 0
      type Found = true
      def idx: Idx = 0
      def found: true = true
    }
    
    given [T <: HList, E, E2](
      using ev: NotGiven[E =:= E2],
      rec: IndexOf[T, E]
    ): IndexOf.Aux[E2 :: T, E, S[rec.Idx], rec.Found] = new IndexOf[E2 :: T, E] {
      type Idx = S[rec.Idx]
      type Found = rec.Found
      def idx: Idx = (rec.idx + 1).asInstanceOf[S[rec.Idx]]
      def found: rec.Found = rec.found
    }

    given [E]: IndexOf.Aux[HNil, E, 0, false] = new IndexOf[HNil, E] {
      type Idx = 0
      type Found = false

      def idx: Idx = 0
      def found: false = false
    }
  }
  
  type IfBool[B <: Boolean, T, F] <: T | F = B match {
    case true => T
    case false => F
  }

  given derive[L <: HList, F, I <: Int, Found <: Boolean](
    using idx: IndexOf.Aux[L, F, I, Found]
  ): Updater.Aux[L, F, IfBool[Found, Append[L, F], L]] =
    new UnsafeUpdater(if idx.found then idx.idx else -1).asInstanceOf[Updater.Aux[L, F, IfBool[Found, Append[L, F], L]]]
}

trait ModifierScalaCompat {
  trait ReplaceField[R <: HList, K, B] {
    type Out <: HList
    type Type
    type Idx <: Int

    def idx: Idx
  }

  object ReplaceField {
    type Aux[R <: HList, K, B, O, T, I] = ReplaceField[R, K, B] {type Out = O; type Type = T; type Idx = I}

    given [T <: HList, K, F, B]: ReplaceField.Aux[FieldType[K, F] :: T, K, B, FieldType[K, B] :: T, F, 0] = new ReplaceField[FieldType[K, F] :: T, K, B] {
      type Out = FieldType[K, B] :: T
      type Type = F
      type Idx = 0

      def idx: Idx = 0
    }

    given [T <: HList, K, F, K2, B](
      using ev: NotGiven[K =:= K2],
      rec: ReplaceField[T, K, B]
    ): ReplaceField.Aux[FieldType[K2, F] :: T, K, B, rec.Out, rec.Type, S[rec.Idx]] = new ReplaceField[FieldType[K2, F] :: T, K, B] {
      type Out = rec.Out
      type Type = rec.Type
      type Idx = S[rec.Idx]

      def idx: S[rec.Idx] = (1 + rec.idx).asInstanceOf[S[rec.Idx]]
    }
  }

  given [R <: HList, K, A, B](
    using replace: ReplaceField[R, K, B],
    ev: replace.Type <:< A,
  ): Modifier.Aux[R, K, A, B, replace.Out] =
    new UnsafeModifier(replace.idx).asInstanceOf[Modifier.Aux[R, K, A, B, replace.Out]]
}

trait RemoverScalaCompat {
  trait RemoveField[R <: HList, K] {
    type Out <: HList
    type Idx <: Int

    def idx: Idx
  }
  object RemoveField {
    type Aux[R <: HList, K, O, I] = RemoveField[R, K] {type Out = O; type Idx = I}
    given [T <: HList, K, F]: RemoveField.Aux[FieldType[K, F] :: T, K, T, 0] = new RemoveField[FieldType[K, F] :: T, K] {
      type Out = T
      type Idx = 0

      def idx: Idx = 0
    }
    given [T <: HList, K, F, K2](
      using ev: NotGiven[K =:= K2],
      rec: RemoveField[T, K]
    ): RemoveField.Aux[FieldType[K2, F] :: T, K, FieldType[K2, F] :: rec.Out, S[rec.Idx]] = new RemoveField[FieldType[K2, F] :: T, K] {
      type Out = FieldType[K2, F] :: rec.Out
      type Idx = S[rec.Idx]

      def idx: S[rec.Idx] = (1 + rec.idx).asInstanceOf[S[rec.Idx]]
    }
  }

  given [R <: HList, K](
      using remove: RemoveField[R, K]
  ): Remover.Aux[R, K, remove.Out] =
    new UnsafeRemover(remove.idx).asInstanceOf[Remover.Aux[R, K, remove.Out]]
}

trait LacksKeyScalaCompat {
  trait HasNoKey[R <: HList, K] {
    type Out <: Boolean
  }
  object HasNoKey {
    type Aux[R <: HList, K, O] = HasNoKey[R, K] {type Out = O}
    given HasNoKey.Aux[HNil, _, true] = new HasNoKey[HNil, _] {
      type Out = true
    }

    given [T <: HList, K, F]: HasNoKey.Aux[FieldType[K, F] :: T, K, false] = new HasNoKey[FieldType[K, F] :: T, K] {
      type Out = false
    }

    given [T <: HList, K, F, K2](
      using ev: NotGiven[K =:= K2],
      rec: HasNoKey[T, K]
    ): HasNoKey.Aux[FieldType[K2, F] :: T, K, rec.Out] = new HasNoKey[FieldType[K2, F] :: T, K] {
      type Out = rec.Out
    }
  }

  given [R <: HList, K](using ev1: HasNoKey[R, K], ev2: ev1.Out =:= true): LacksKey[R, K] = new LacksKey[R, K]
}
