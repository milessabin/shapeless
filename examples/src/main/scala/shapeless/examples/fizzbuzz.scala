/*
 * Copyright (c) 2011-14 Miles Sabin
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

/** A type-level implementation of [[http://en.wikipedia.org/wiki/Fizz_buzz fizz buzz]]
 * based on `Nat` and `HList` */
object FizzBuzzExample {
  import shapeless._
  import nat._
  import ops.nat._
  import ops.hlist._
  import FizzBuzz._

  sealed trait FizzBuzz

  object FizzBuzz {
    case object Fizz extends FizzBuzz
    case object Buzz extends FizzBuzz
    case object FizzAndBuzz extends FizzBuzz
    final class Other[N <: Nat] extends FizzBuzz
  }

  sealed trait NatToFizzBuzz[N <: Nat] extends DepFn0 {
    type Out <: FizzBuzz
  }

  sealed trait NatToFizzBuzzInstances1 {
    type Aux[N <: Nat, FB <: FizzBuzz] = NatToFizzBuzz[N] { type Out = FB }

    implicit def other[N <: Nat]: Aux[N, Other[N]] =
      new NatToFizzBuzz[N] {
        type Out = Other[N]
        def apply = new Other[N]
      }
  }

  sealed trait NatToFizzBuzInstances0 extends NatToFizzBuzzInstances1 {
    implicit def fizz[N <: Nat](implicit ev: Mod.Aux[N, _3, _0]): Aux[N, Fizz.type] =
      new NatToFizzBuzz[N] {
        type Out = Fizz.type
        def apply = Fizz
      }

    implicit def buzz[N <: Nat](implicit ev: Mod.Aux[N, _5, _0]): Aux[N, Buzz.type] =
      new NatToFizzBuzz[N] {
        type Out = Buzz.type
        def apply = Buzz
      }
  }

  /** A type class that can translate a natural number into a particular type of FizzBuzz.
   *
   * Note: instances are separated into a type class hierarchy to prioritize
   * implicit resolution.
   */
  object NatToFizzBuzz extends NatToFizzBuzInstances0 {
    implicit def fizzAndBuzz[N <: Nat](implicit fizz: Aux[N, Fizz.type], buzz: Aux[N, Buzz.type]): Aux[N, FizzAndBuzz.type] =
      new NatToFizzBuzz[N] {
        type Out = FizzAndBuzz.type
        def apply = FizzAndBuzz
      }
  }

  /**
   * Creates an HList that is the reverse solution to the fizzbuzz challenge for
   * the number N. Each element of the HList is a subtype of FizzBuzz.
   */
  sealed trait RevFizzBuzz[N <: Nat] extends DepFn0 { type Out <: HList }

  object RevFizzBuzz {
    type Aux[N <: Nat, L <: HList] = RevFizzBuzz[N] { type Out = L }
    implicit def revFizzBuzzOne: Aux[_1, Other[_1] :: HNil] =
      new RevFizzBuzz[_1] {
        type Out = Other[_1] :: HNil
        def apply = new Other[_1] :: HNil
      }

    implicit def succRevFizzBuzz[N <: Nat](implicit f: RevFizzBuzz[N], n: NatToFizzBuzz[Succ[N]]): Aux[Succ[N], n.Out :: f.Out] =
      new RevFizzBuzz[Succ[N]] {
        type Out = n.Out :: f.Out
        def apply = n.apply :: f.apply
      }
  }

  /**
   * Creates an HList that is the solution to the fizzbuzz challenge for the
   * number N. Each element of the HList is a subtype of FizzBuzz.
   */
  sealed trait FizzBuzzResult[N <: Nat] extends DepFn0 { type Out <: HList }

  object FizzBuzzResult {
    type Aux[N <: Nat, L <: HList] = FizzBuzzResult[N] { type Out = L }

    implicit def fizzBuzzResult[N <: Nat, L <: HList](implicit rfb: RevFizzBuzz.Aux[N, L], r: Reverse[L]): Aux[N, r.Out] =
      new FizzBuzzResult[N] {
        type Out = r.Out
        def apply() = r(rfb())
      }
  }

  /** Converts subtypes of FizzBuzz into string representations */
  object FizzBuzzToString extends Poly1 {
    implicit val fizz = at[Fizz.type](_ => "fizz")
    implicit val buzz = at[Buzz.type](_ => "buzz")
    implicit val fizzAndBuzz = at[FizzAndBuzz.type](_ => "fizzbuzz")
    implicit def other[N <: Nat](implicit t: ToInt[N]) = at[Other[N]](n => t().toString)
  }

  /**
   * Creates an HList that is the solution to the fizzbuzz challenge for the
   * number N. Each element of the HList is a subtype of FizzBuzz.
   *
   * Example call:
   * {{{
   * import shapeless.nat._
   * import shapeless.examples.FizzBuzzExample._
   *
   * val r = fizzBuzz[_15]
   * val s = r.map(FizzBuzzToString).mkString("", "\n", "")
   * print(s)
   * }}}
   *
   * Which produces the output:
   * <pre>
   * 1
   * 2
   * fizz
   * 4
   * buzz
   * fizz
   * 7
   * 8
   * fizz
   * buzz
   * 11
   * fizz
   * 13
   * 14
   * fizzbuzz
   * </pre>
   */
  def fizzBuzz[N <: Nat](implicit f: FizzBuzzResult[N]): f.Out = f()

}
