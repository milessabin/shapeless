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

package shapeless

import scala.collection.mutable.WrappedArray
import scala.compiletime._
import scala.deriving._

type Id[t] = t
type Const[c] = [t] =>> c
case class Wrap[T](t: T)


type ~>[A[_], B[_]] = [t] => A[t] => B[t]

inline def summon[T] = implicit match {
  case t: T => t
}

inline def summonValues[T] <: Tuple = inline erasedValue[T] match {
  case _: Unit => ()
  case _: (a *: b) => constValue[a] *: summonValues[b]
}

inline def summonValuesAsArray[T]: Array[Any] = inline erasedValue[Id[T]] match {
  case _: Unit => Array()
  case _: Tuple1[a] => Array(constValue[a])
  case _: (a, b) => Array(constValue[a], constValue[b])
  case _: (a, b, c) => Array(constValue[a], constValue[b], constValue[c])
  case _: (a, b, c, d) => Array(constValue[a], constValue[b], constValue[c], constValue[d])
  case _: (a, b, c, d, e) => Array(constValue[a], constValue[b], constValue[c], constValue[d], constValue[e])
  // Add fallback for larger sizes
}

case class Labelling[T](label: String, elemLabels: Seq[String])
object Labelling {
  inline given apply[T0] as Labelling[T0] given (mirror: Mirror { type MirroredType = T0 }) =
    Labelling[T0](
      constValue[mirror.MirroredLabel & String],
      WrappedArray.make[String](summonValuesAsArray[mirror.MirroredElemLabels])
    )
}

sealed trait CompleteOr[T]
case class Complete[T](t: T) extends CompleteOr[T]
case class Continue[T](t: T) extends CompleteOr[T]

object Complete {
  inline def apply[T](c: Boolean)(t: T)(f: T): CompleteOr[T] =
    if(c) Complete(t)
    else Continue(f)
}
