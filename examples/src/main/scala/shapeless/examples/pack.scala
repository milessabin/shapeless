/*
 * Copyright (c) 2012 Miles Sabin 
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

/**
 * Demonstration of automatic packaging of multiple type class instances at call sites and
 * automatic unpackaging of them at call targets. 
 * 
 * @author Miles Sabin
 */
object PackExamples extends App {
  import shapeless._
  
  sealed trait Pack[F[_], L <: HList]
  final case class PCons[F[_], H, T <: HList](head: F[H], tail: Pack[F, T]) extends Pack[F, H :: T]
  final case class PNil[F[_]]() extends Pack[F, HNil]
  
  object Pack {
    implicit def packHNil[F[_]]: PNil[F] = PNil[F]()

    implicit def packHList[F[_], H, T <: HList]
      (implicit fh: F[H], pt: Pack[F, T]): Pack[F, H :: T] = PCons(fh, pt)

    implicit def unpack[F[_], E, L <: HList](implicit pack: Pack[F, L], unpack: Unpack[F, L, E]): F[E] = unpack(pack)

    trait Unpack[F[_], L <: HList, E] {
      def apply(pack: Pack[F, L]): F[E]
    }

    object Unpack extends {
      implicit def unpack1[F[_], H, T <: HList]
        (implicit pc: IsPCons.Aux[F, H :: T, H, T]): Unpack[F, H :: T, H] =
          new Unpack[F, H :: T, H] {
            def apply(pack: Pack[F, H :: T]): F[H] = pc.split(pack)._1
          }

      implicit def unpack2[F[_], H, T <: HList, E]
        (implicit pc: IsPCons.Aux[F, H :: T, H, T], ut: Unpack[F, T, E]): Unpack[F, H :: T, E] =
          new Unpack[F, H :: T, E] {
            def apply(pack: Pack[F, H :: T]): F[E] = ut(pc.split(pack)._2)
          }
    }

    trait IsPCons[F[_], L <: HList] {
      type H
      type T <: HList
        
      def split(p: Pack[F, L]): (F[H], Pack[F, T])
    }

    object IsPCons {
      type Aux[F[_], L <: HList, H0, T0 <: HList] = IsPCons[F, L] { type H = H0; type T = T0 }
      implicit def hlistIsPCons[F[_], H0, T0 <: HList]: Aux[F, H0 :: T0, H0, T0] =
        new IsPCons[F, H0 :: T0] {
          type H = H0
          type T = T0
        
          def split(p: Pack[F, H :: T]): (F[H], Pack[F, T]) = p match {
            case PCons(fh, pt) => (fh, pt)
          }
        }
    }
  }
  
  import Pack._

  trait A
  trait B
  trait C
  
  trait Show[T] {
    def show : String
  }
  
  def show[T](t : T)(implicit s : Show[T]) = s.show
  
  val a = new A {}
  val b = new B {}
  val c = new C {}

  implicit val sa = new Show[A] { def show = "A" }
  implicit val sb = new Show[B] { def show = "B" }
  implicit val sc = new Show[C] { def show = "C" }

  def use3[T, U, V](t : T, u : U, v : V)(implicit pack : Pack[Show, T :: U :: V :: HNil]) = {
    // Instances automatically unpacked here
    (show(t), show(u), show(v))
  }
  
  // Instances automatically packed here
  assert(use3(a, b, c) == ("A", "B", "C"))
}
