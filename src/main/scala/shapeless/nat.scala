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
 * Base trait for type level natural numbers.
 * 
 * @author Miles Sabin
 */
trait Nat

/**
 * Encoding of successor.
 * 
 * @author Miles Sabin
 */
case class Succ[P <: Nat]() extends Nat

/**
 * Type level encoding of the natural numbers.
 * 
 * @author Miles Sabin
 */
object Nat extends Nats {
  class _0 extends Nat
  val _0 = new _0
  
  def toInt[N <: Nat](implicit toIntN : ToInt[N]) = toIntN() 

  def toInt[N <: Nat](n : N)(implicit toIntN : ToInt[N]) = toIntN() 
}

/**
 * Type class witnessing that `B` is the predecessor of `A`.
 * 
 * @author Miles Sabin
 */
trait Pred[A <: Nat, B <: Nat]

object Pred {
  implicit def pred[B <: Nat] = new Pred[Succ[B], B] {}
}

/**
 * Type class witnessing that `C` is the sum of `A` and `B`.
 * 
 * @author Miles Sabin
 */

trait Sum[A <: Nat, B <: Nat, C <: Nat]

object Sum {
  import Nat._0
  implicit def sum1[B <: Nat] = new Sum[_0, B, B] {}
  implicit def sum2[A <: Nat, B <: Nat, C <: Nat]
    (implicit ev : Sum[A, Succ[B], C]) = new Sum[Succ[A], B, C] {}
}

/**
 * Type class witnessing that `C` is the product of `A` and `B`.
 * 
 * @author Miles Sabin
 */
trait Prod[A <: Nat, B <: Nat, C <: Nat]

object Prod {
  import Nat._0

  implicit def prod1[B <: Nat] = new Prod[_0, B, _0] {}
  implicit def prod2[A <: Nat, B <: Nat, C <: Nat, D <: Nat]
    (implicit ev1 : Prod[A, B, C], ev2 : Sum[B, C, D]) = new Prod[Succ[A], B, D] {}
}

/**
 * Type class witnessing that `A` is less than `B`.
 * 
 * @author Miles Sabin
 */
trait LT[A <: Nat, B <: Nat]

object LT {
  import Nat._0

  type <[A <: Nat, B <: Nat] = LT[A, B]

  implicit def lt1[B <: Nat] = new <[_0, Succ[B]] {}
  implicit def lt2[A <: Nat, B <: Nat](implicit lt : A < B) = new <[Succ[A], Succ[B]] {}
}

/**
 * Type class supporting conversion of type-level Nats to value level Ints.
 * 
 * @author Miles Sabin
 */
trait ToInt[N <: Nat] {
  def apply() : Int
}

object ToInt {
  import Nat._0

  implicit val toInt0 = new ToInt[_0] {
    def apply() = 0 
  }
  implicit def toIntSucc[N <: Nat](implicit toIntN : ToInt[N]) = new ToInt[Succ[N]] {
    def apply() = toIntN()+1
  }
}
