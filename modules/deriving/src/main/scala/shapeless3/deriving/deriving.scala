/*
 * Copyright (c) 2019 Miles Sabin
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

package shapeless3.deriving

import scala.collection.immutable.ArraySeq
import scala.compiletime.*
import scala.deriving.*
import scala.reflect.ClassTag

type Id[t] = t
type Const[c] = [t] =>> c

type ~>[A[_], B[_]] = [t] => A[t] => B[t]

inline def summonAsArray[T <: Tuple]: Array[Any] =
  summonAsArray0[T](0, new Array[Any](constValue[Tuple.Size[T]]))

inline def summonAsArray0[T](i: Int, arr: Array[Any]): Array[Any] = inline erasedValue[T] match {
  case _: EmptyTuple => arr
  case _: (a *: b) =>
    arr(i) = summonInline[a]
    summonAsArray0[b](i+1, arr)
}

inline def summonValuesAsArray[T <: Tuple, E: ClassTag]: Array[E] =
  summonValuesAsArray0[T, E](0, new Array[E](constValue[Tuple.Size[T]]))

inline def summonValuesAsArray0[T, E](i: Int, arr: Array[E]): Array[E] = inline erasedValue[T] match {
  case _: EmptyTuple => arr
  case _: (a *: b) =>
    arr(i) = constValue[a & E]
    summonValuesAsArray0[b, E](i+1, arr)
}

case class Labelling[T](label: String, elemLabels: IndexedSeq[String])
object Labelling {
  inline given apply[T0](using mirror: Mirror { type MirroredType = T0 }): Labelling[T0] =
    Labelling[T0](
      constValue[mirror.MirroredLabel & String],
      ArraySeq.unsafeWrapArray(summonValuesAsArray[mirror.MirroredElemLabels, String])
    )
}

type CompleteOr[T] = T | Complete[T]

case class Complete[T](t: T)

object Complete {
  inline def apply[T](c: Boolean)(t: T)(f: T): CompleteOr[T] =
    if(c) Complete(t)
    else f
}

object Continue {
  inline def apply[T](t: T): CompleteOr[T] = t
}
