/*
 * Copyright (c) 2015 Miles Sabin
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

package shapeless.examples

import shapeless._

// An ADT+shapeless as a drop-in replacement for a standard Scala Enumeration.
//
// First the unsafe standard Scala Enumeration ...
//
object ScalaEnumDemo /*extends App*/ {
  // Example from scala.Enumeration scaladoc. Terse ...
  object WeekDay extends Enumeration {
    type WeekDay = Value
    val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
  }

  import WeekDay._

  def isWorkingDay(d: WeekDay) = ! (d == Sat || d == Sun)

  assert((WeekDay.values filter isWorkingDay) == Set(Mon, Tue, Wed, Thu, Fri))

  // However ...

  def isWeekend(d: WeekDay) = (d: @unchecked) match {
    case Sat | Sun => true
    // Oops! Missing case ... still compiles (although as of 2.13.5 only with @unchecked)
  }

  assert(!isWeekend(Mon)) // MatchError at run time
}

// A safer ADT+shapeless alternative ...
//
object ShapelessEnumDemo extends App {
  // ADT as an enumeration. Barely any more boilerplate ...
  sealed abstract class WeekDay(val ordinal: Int) extends Serializable
  object WeekDay {
    private var ordinal = 0
    val Mon, Tue, Wed, Thu, Fri, Sat, Sun =
      try new WeekDay(ordinal) {}
      finally ordinal += 1
    val values: Set[WeekDay] = Values
  }

  import WeekDay._

  def isWorkingDay(d: WeekDay) = ! (d == Sat || d == Sun)

  assert((WeekDay.values filter isWorkingDay) == Set(Mon, Tue, Wed, Thu, Fri))

  // ... the payoff ...

  def isWeekend(d: WeekDay) = d match {
    case Sat | Sun => true
    case _ => false // compile time non-exhaustive match warning/error without this case
  }

  assert(!isWeekend(Mon))
  assert(values.size == values.map(_.ordinal).size)
}

// Infrastructure for the above. Original version due to Travis Brown,
//
//   http://stackoverflow.com/questions/25838411
//
object Values {
  implicit def conv[T](self: this.type)(implicit v: MkValues[T]): Set[T] = Values[T]

  def apply[T](implicit v: MkValues[T]): Set[T] = v.values.toSet

  trait MkValues[T] {
    def values: List[T]
  }

  object MkValues {
    implicit def values[T, Repr <: Coproduct]
      (implicit gen: Generic.Aux[T, Repr], v: Aux[T, Repr]): MkValues[T] =
        new MkValues[T] { def values = v.values }

    trait Aux[T, Repr] {
      def values: List[T]
    }

    object Aux {
      implicit def cnilAux[A]: Aux[A, CNil] =
        new Aux[A, CNil] { def values = Nil }

      implicit def cconsAux[T, L <: T, R <: Coproduct]
        (implicit l: Witness.Aux[L], r: Aux[T, R]): Aux[T, L :+: R] =
        new Aux[T, L :+: R] { def values = l.value :: r.values }
    }
  }
}
