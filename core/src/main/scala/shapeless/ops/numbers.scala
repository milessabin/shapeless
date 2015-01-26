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

package shapeless
package ops

object numbers {

  /**
   * Type class witnessing that `B` is the predecessor of `A`.
   *
   * @author Olivier Mélois
   */
  trait Predecessor[A <: RInt] { type Out <: RInt }

  object Predecessor {
    def apply[A <: RInt](implicit pred: Predecessor[A]): Aux[A, pred.Out] = pred

    type Aux[A <: RInt, B <: RInt] = Predecessor[A] { type Out = B }

    implicit val pred0 = new Predecessor[_0] { type Out = Minus[Succ[_0]] }
    implicit def pred1[B <: Nat]: Aux[Succ[B], B] = new Predecessor[Succ[B]] { type Out = B }
    implicit def pred2[B <: SPos]: Aux[Minus[B], Minus[Succ[B]]] = new Predecessor[Minus[B]] { type Out = Minus[Succ[B]] }
  }

  /**
   * Type class witnessing that `B` is the successor of `A`.
   *
   * @author Olivier Mélois
   */
  trait Successor[A <: RInt] { type Out <: RInt }

  object Successor {
    def apply[A <: RInt](implicit succ: Successor[A]): Aux[A, succ.Out] = succ

    type Aux[A <: RInt, B <: RInt] = Successor[A] { type Out = B }

    implicit val succ0 : Aux[Minus[Succ[_0]], _0] = new Successor[Minus[Succ[_0]]] { type Out = _0 }
    implicit def succ1[B <: Nat]: Aux[B, Succ[B]] = new Successor[B] { type Out = Succ[B] }
    implicit def succ2[B <: SPos]: Aux[Minus[Succ[B]], Minus[B]] = new Successor[Minus[Succ[B]]] { type Out = Minus[B] }
  }

  /**
   * Type class supporting conversion of type-level RInts to value level RInts.
   *
   * @author Olivier Mélois
   */
  trait ToInt[N <: RInt] {
    def apply(): Int
  }

  object ToInt {
    def apply[N <: RInt](implicit toInt: ToInt[N]): ToInt[N] = toInt

    implicit val toInt0 = new ToInt[_0] {
      def apply() = 0
    }

    implicit def toIntSucc[N <: Nat](implicit toIntN: ToInt[N]) = new ToInt[Succ[N]] {
      def apply() = toIntN() + 1
    }

    implicit def toIntMinus[N <: SPos](implicit toIntN: ToInt[N]) = new ToInt[Minus[N]] {
      def apply() = -toIntN()
    }
  }

  /**
   * Type class witnessing that B and A are opposite integers.
   *
   * @author Olivier Mélois
   */
  trait Opposite[A <: Number] { type Out <: RInt }

  object Opposite {
    def apply[A <: Number](implicit opposite: Opposite[A]): Aux[A, opposite.Out] = opposite

    type Aux[A <: Number, B <: Number] = Opposite[A] { type Out = B }
    
    implicit val opposite0 = new Opposite[_0] { type Out = _0 }
    implicit def opposite1[B <: SPos]: Aux[B, Minus[B]] = new Opposite[B] { type Out = Minus[B] }
    implicit def opposite2[B <: SPos]: Aux[Minus[B], B] = new Opposite[Minus[B]] { type Out = B }
  }

  /**
   * Type class witnessing that `C` is the sum of `A` and `B`.
   *
   * @author Olivier Mélois
   */
  trait Sum[A <: Number, B <: Number] { type Out <: Number }

  object Sum {
    def apply[A <: Number, B <: Number](implicit sum: Sum[A, B]): Aux[A, B, sum.Out] = sum

    type Aux[A <: Number, B <: Number, C <: Number] = Sum[A, B] { type Out = C }

    implicit def sum0[B <: Number]: Aux[_0, B, B] = new Sum[_0, B] { type Out = B }
    implicit def sum1[A <: SPos, PA <: Nat, B <: RInt, SB <: RInt](implicit predA: Predecessor.Aux[A, PA], succB: Successor.Aux[B, SB], sum: Sum[PA, SB]): Aux[A, B, sum.Out] = new Sum[A, B] { type Out = sum.Out }
    implicit def sum2[A <: SNeg, OA <: SPos, B <: RInt, OB <: RInt, C <: RInt](implicit oppA: Opposite.Aux[A, OA], oppB: Opposite.Aux[B, OB], sum: Sum.Aux[OA, OB, C], oppC: Opposite[C]): Aux[A, B, oppC.Out] = new Sum[A, B] { type Out = oppC.Out }
  }

  /**
   * Type class witnessing that `C` is the sum of `A` and `B`.
   *
   * @author Olivier Mélois
   */
  trait Diff[A <: Number, B <: Number] { type Out <: Number }

  object Diff {
    def apply[A <: Number, B <: Number](implicit sum: Diff[A, B]): Aux[A, B, sum.Out] = sum
    
    type Aux[A <: Number, B <: Number, C <: Number] = Diff[A, B] { type Out = C }

    implicit def diff[A <: Number, B <: Number, OB <: Number](implicit opposite: Opposite.Aux[B, OB], sum: Sum[A, OB]): Aux[A, B, sum.Out] = new Diff[A, B] { type Out = sum.Out }
  }

  /**
   * Type class witnessing that `C` is the product of `A` and `B`.
   *
   * @author Olivier Mélois
   */
  trait Prod[A <: Number, B <: Number] { type Out <: Number }

  object Prod {
    def apply[A <: Number, B <: Number](implicit prod: Prod[A, B]): Aux[A, B, prod.Out] = prod

    type Aux[A <: Number, B <: Number, C <: Number] = Prod[A, B] { type Out = C }

    implicit def prod0[B <: Number]: Aux[_0, B, _0] = new Prod[_0, B] { type Out = _0 }
    implicit def prod1[A <: SPos, PA <: Nat, B <: Number, C <: Number](implicit pred: Predecessor.Aux[A, PA], prod: Prod.Aux[PA, B, C], sum: Sum[B, C]): Aux[A, B, sum.Out] = new Prod[A, B] { type Out = sum.Out }
    implicit def prod2[A <: SNeg, OA <: Number, B <: Number, C <: Number](implicit oppA: Opposite.Aux[A, OA], prod: Prod.Aux[OA, B, C], oppC: Opposite[C]): Aux[A, B, oppC.Out] = new Prod[A, B] { type Out = oppC.Out }
  }

  /**
   * Type class witnessing that `A` is less than `B`.
   *
   * @author Olivier Mélois
   */
  trait LT[A <: Number, B <: Number]

  object LT {
    def apply[A <: Number, B <: Number](implicit lt: A < B): LT[A, B] = lt

    type <[A <: Number, B <: Number] = LT[A, B]

    implicit def lt0[A <: Neg, B <: SPos] = new <[A, B] {}
    implicit def lt1[A <: SNeg] = new <[A, _0] {}
    implicit def lt2[A <: Nat, B <: Nat](implicit lt: A < B) = new <[Succ[A], Succ[B]] {}
    implicit def lt3[A <: SPos, OA <: RInt, B <: SPos, OB <: RInt](implicit oppa: Opposite.Aux[A, OA], oppb: Opposite.Aux[B, OB], lt: A < B) = new <[OB, OA] {}
  }

  /**
   * Type class witnessing that `A` is less than or equal to `B`.
   *
   *
   * @author Olivier Mélois
   */
  trait LTEq[A <: Number, B <: Number]

  object LTEq {
    def apply[A <: Number, B <: Number](implicit lteq: A <= B): LTEq[A, B] = lteq

    type <=[A <: Number, B <: Number] = LTEq[A, B]

    implicit def equal[A <: Number] = new <=[A, A] {}
    implicit def lt[A <: Number, B <: Number](implicit lt: LT[A, B]) = new <=[A, B] {}
  }


  /**
   * Type class witnessing that `A` and `B` are either both positive of both negative.
   *
   * @author Olivier Mélois
   */
  trait SameSign[A <: Number, B <: Number]

  object SameSign {
    def apply[A <: Number, B <: Number](implicit ss: SameSign[A,B]): SameSign[A, B] = ss
  
    implicit def ss0[A <: SPos, B <: SPos] = new SameSign[A, B]{}
    implicit def ss1[A <: SNeg, B <: SNeg] = new SameSign[A, B]{}
  }

  /**
   * Type class witnessing that `A` and `B` are neither both positive or both negative.
   *
   * @author Olivier Mélois
   */
  trait OppositeSign[A <: Number, B <: Number]

  object OppositeSign {
    def apply[A <: Number, B <: Number](implicit os: OppositeSign[A,B]): OppositeSign[A, B] = os
  
    implicit def os0[A <: SPos, B <: SNeg] = new OppositeSign[A, B]{}
    implicit def os1[A <: SNeg, B <: SPos] = new OppositeSign[A, B]{}
  }

  /**
   * Type class witnessing that `Out` is the absolute value of `A`.
   *
   * @author Olivier Mélois
   */
  trait Absolute[A <: Number] { type Out <: Number}
  
  object Absolute {
    def apply[A <: Number](implicit absolute: Absolute[A]): Aux[A, absolute.Out] = absolute

    type Aux[A <: Number, B <: Number] = Absolute[A] { type Out = B }
    
    implicit val absolute0 = new Absolute[_0] { type Out = _0 }
    implicit def absolute1[B <: SPos]: Aux[B, B] = new Absolute[B] { type Out = B }
    implicit def absolute2[B <: SNeg, OB <: SPos](implicit oppb : Opposite.Aux[B, OB]) = new Absolute[B] { type Out = OB }
  }


  /**
   * Type class witnessing that `Out` is the quotient of `A` and `B`.
   *
   * @author Olivier Mélois
   */
  trait Div[A <: Number, B <: Number] { type Out <: Number }

  object Div {
    def apply[A <: Number, B <: Number](implicit div: Div[A, B]): Aux[A, B, div.Out] = div

    import LT._

    type _1 = RInt._1

    type Aux[A <: Number, B <: Number, C <: Number] = Div[A, B] { type Out = C }

    implicit def div0[A <: Number]: Aux[_0, A, _0] = new Div[_0, A] { type Out = _0 }
    implicit def div1[A <: SPos] : Aux[A, A, _1] = new Div[A, A] {type Out = _1}
    implicit def div2[A <: SPos, B <: SPos](implicit lt: A < B): Aux[A, B, _0] =
      new Div[A, B] { type Out = _0 }
    implicit def div3[A <: Number, B <: Number, AA <: Number, AB <: Number, C <: Nat, D <: Nat](implicit sameSign : SameSign[A,B], absa : Absolute.Aux[A, AA], absb : Absolute.Aux[B,AB], lt : AB < AA, diff: Diff.Aux[AA, AB, C], div: Div.Aux[C, AB, D]): Aux[A, B, Succ[D]] =
      new Div[A, B] { type Out = Succ[D] }
    implicit def div4[A <: Number, B <: Number, AA <: Number, AB <: Number, C <: Number](implicit oppSign : OppositeSign[A,B], absa : Absolute.Aux[A, AA], absb : Absolute.Aux[B,AB], div : Div.Aux[AA, AB, C], oppc : Opposite[C]) : Aux[A, B, oppc.Out] =
      new Div[A, B] { type Out = oppc.Out}

  }

}

