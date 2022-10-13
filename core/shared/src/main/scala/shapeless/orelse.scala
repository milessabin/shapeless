/*
 * Copyright (c) 2017 Georgi Krastev
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
 * Like `Option.orElse` on the type level and like `Either` on the value level.
 *
 * Instead of left and right constructors `OrElse` has primary and secondary implicits that lazily
 * try to resolve first a value of type `A` or otherwise a value of type `B`.
 */
sealed trait OrElse[+A, +B] {
  def fold[C](prim: A => C, sec: B => C): C
  def unify[C >: A](implicit ev: B <:< C): C = fold(identity, ev)
}

final class Primary[+A](value: A) extends OrElse[A, Nothing] {
  def fold[C](prim: A => C, sec: Nothing => C) = prim(value)
}

final class Secondary[+B](value: => B) extends OrElse[Nothing, B] {
  def fold[C](prim: Nothing => C, sec: B => C) = sec(value)
}

object OrElse extends OrElse0 {
  implicit def primary[A, B](implicit a: A): A OrElse B =
    new Primary(a)
}

private[shapeless] abstract class OrElse0 {
  implicit def secondary[A, B](implicit b: Lazy[B]): A OrElse B =
    new Secondary(b.value)
}
