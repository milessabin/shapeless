/*
 * Copyright (c) 2012-13 Miles Sabin 
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

/*
 * Proof of concept implementation of typesafe vectors of arbitrary dimension.
 * 
 * @author Miles Sabin
 */
object LinearAlgebraExamples extends App {
  import shapeless._
  import nat._
  import newtype._
  import ops.hlist.{ Mapper, Transposer }
  import ops.product.ProductLength

  
  def typed[T](t : => T) {}

  abstract class VectorOps[N <: Nat, P <: Product](p : P) {
    type Self = Newtype[P, VectorOps[N, P]]
    def tupled = p
    def +(other : Self) : Self
  }
  
  object VectorOps {
    type HomPair[T] = (T, T)
    object sum extends Poly1 {
      implicit def caseDouble = at[Double :: Double :: HNil]{ case a :: b :: HNil => a+b }
    }
    
    implicit def pointOps1(p : Tuple1[Double]) : VectorOps[_1, Tuple1[Double]] = new VectorOps[_1, Tuple1[Double]](p) {
      def +(other : Self) : Self = newtype(Tuple1(p._1+other.tupled._1))
    }
    
    implicit def pointOpsN[N <: Nat, LN <: HList, PN <: Product, ZLN <: HList]
      (implicit
        gen : Generic.Aux[PN, LN],
        zipper : Transposer.Aux[LN :: LN :: HNil, ZLN],
        mapper : Mapper.Aux[sum.type, ZLN, LN]) : PN => VectorOps[N, PN] =
          (p : PN) =>
            new VectorOps[N, PN](p) {
              def +(other : Self) : Self =
                newtype(gen.from((gen.to(p) :: gen.to(other.tupled) :: HNil).transpose.map(sum)))
            }
  }

  def Vector(p : Double) = newtype[Tuple1[Double], VectorOps[_1, Tuple1[Double]]](Tuple1(p))
  def Vector[P <: Product, N <: Nat](p : P)(implicit ar : ProductLength.Aux[P, N]) = newtype[P, VectorOps[N, P]](p)
  
  type V1 = Newtype[Tuple1[Double], VectorOps[_1, Tuple1[Double]]]
  type V2 = Newtype[(Double, Double), VectorOps[_2, (Double, Double)]]
  type V3 = Newtype[(Double, Double, Double), VectorOps[_3, (Double, Double, Double)]]
  
  val v1 = Vector(1.0)
  typed[V1](v1)
  v1.tupled
  
  val v2 = Vector(1.0, 1.0)
  typed[V2](v2)
  v2.tupled
  
  val v3a = Vector(1.0, 1.0, 1.0)
  typed[V3](v3a)
  v3a.tupled

  val v3b = Vector(0.0, 1.0, -1.0)
  typed[V3](v3b)
  v3b.tupled
  
  val v3c = v3a+v3b 
  typed[V3](v3c)
  
  assert((1.0, 2.0, 0.0) == v3c.tupled)
}
