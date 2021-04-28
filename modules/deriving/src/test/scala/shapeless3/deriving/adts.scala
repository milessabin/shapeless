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

// ADTs

object adts {
  case class ISB(i: Int, s: String, b: Boolean) derives Monoid, Eq, Empty, Show, Read

  case class Box[A](x: A) derives Monoid, Eq, Show, Read, Functor, Pure, Ord

  sealed trait OptionInt derives Eq, Empty, Show, Read, Ord
  case object NoneInt extends OptionInt
  case class SomeInt(value: Int) extends OptionInt

  sealed trait Opt[+A] derives Eq, Show, Read, Functor, EmptyK, Pure, Ord, Traverse
  case object Nn extends Opt[Nothing]
  case class Sm[+A](value: A) extends Opt[A]

  sealed trait CList[+A] derives Eq, Show, Read, Functor, EmptyK, Traverse
  case class CCons[+A](hd: A, tl: CList[A]) extends CList[A]
  case object CNil extends CList[Nothing]

  case class Order[F[_]](
    item: F[String],
    quantity: F[Int]
  ) derives FunctorK

  sealed trait OptionD[T] {
    def fold: T = this match {
      case Given(t) => t
      case Default(t) => t
    }
  }
  object OptionD {
    val fold: OptionD ~> Id = [t] => (ot: OptionD[t]) => ot.fold
  }

  case class Given[T](value: T) extends OptionD[T]
  case class Default[T](value: T) extends OptionD[T]

  sealed trait ListF[+A, +R] derives Bifunctor
  object ListF {
    type List[A] = Fix[ListF, A]
  }
  case class ConsF[+A, +R](hd: A, tl: R) extends ListF[A, R]
  case object NilF extends ListF[Nothing, Nothing]

  case class BI(b: Boolean, i: Int)
}
