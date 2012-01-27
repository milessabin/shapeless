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
trait Pred[A <: Nat] {
  type Out <: Nat
}

trait PredAux[A <: Nat, B <: Nat]

object Pred {
  implicit def pred[A <: Nat, B <: Nat](implicit pred : PredAux[A, B]) = new Pred[A] {
    type Out = B
  }
}

object PredAux {
  implicit def pred[B <: Nat] = new PredAux[Succ[B], B] {}
}

/**
 * Type class witnessing that `C` is the sum of `A` and `B`.
 * 
 * @author Miles Sabin
 */
trait Sum[A <: Nat, B <: Nat] {
  type Out <: Nat
}

trait SumAux[A <: Nat, B <: Nat, C <: Nat]

object Sum {
  implicit def sum[A <: Nat, B <: Nat, C <: Nat](implicit diff : SumAux[A, B, C]) = new Sum[A, B] {
    type Out = C
  }
}

object SumAux {
  import Nat._0
  implicit def sum1[B <: Nat] = new SumAux[_0, B, B] {}
  implicit def sum2[A <: Nat, B <: Nat, C <: Nat]
    (implicit ev : SumAux[A, Succ[B], C]) = new SumAux[Succ[A], B, C] {}
}

/**
 * Type class witnessing that `C` is the difference of `A` and `B`.
 * 
 * @author Miles Sabin
 */
trait Diff[A <: Nat, B <: Nat] {
  type Out <: Nat
}

trait DiffAux[A <: Nat, B <: Nat, C <: Nat]

object Diff {
  implicit def diff[A <: Nat, B <: Nat, C <: Nat](implicit diff : DiffAux[A, B, C]) = new Diff[A, B] {
    type Out = C
  }
}

object DiffAux {
  import Nat._0
  implicit def diff1[A <: Nat] = new DiffAux[A, _0, A] {}
  implicit def diff2[A <: Nat, B <: Nat, C <: Nat]
    (implicit ev : DiffAux[A, B, C]) = new DiffAux[Succ[A], Succ[B], C] {}
}

/**
 * Type class witnessing that `C` is the product of `A` and `B`.
 * 
 * @author Miles Sabin
 */
trait Prod[A <: Nat, B <: Nat] {
  type Out <: Nat
}

trait ProdAux[A <: Nat, B <: Nat, C <: Nat]

object Prod {
  implicit def prod[A <: Nat, B <: Nat, C <: Nat](implicit diff : ProdAux[A, B, C]) = new Prod[A, B] {
    type Out = C
  }
}

object ProdAux {
  import Nat._0

  implicit def prod1[B <: Nat] = new ProdAux[_0, B, _0] {}
  implicit def prod2[A <: Nat, B <: Nat, C <: Nat, D <: Nat]
    (implicit ev1 : ProdAux[A, B, C], ev2 : SumAux[B, C, D]) = new ProdAux[Succ[A], B, D] {}
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
 * Type class witnessing that `A` is less than or equal to `B`.
 * 
 * @author Miles Sabin
 */
trait LTEq[A <: Nat, B <: Nat]

object LTEq {
  import Nat._0

  type <=[A <: Nat, B <: Nat] = LTEq[A, B]

  implicit def ltEq1 = new <=[_0, _0] {}
  implicit def ltEq2[B <: Nat] = new <=[_0, Succ[B]] {}
  implicit def ltEq3[A <: Nat, B <: Nat](implicit lt : A <= B) = new <=[Succ[A], Succ[B]] {}
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
