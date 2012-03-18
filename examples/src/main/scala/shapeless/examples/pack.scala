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
 * almost automatic unpackaging of them at call targets. 
 * 
 * @author Miles Sabin
 */
object PackExamples extends App {
  import shapeless._
  
  // 
  trait Pack[F[_], L <: HList] {
    implicit def unpack[E](implicit  u : Unpack[F, E, L]) = u(this)
    
    def split[H, T <: HList](implicit ev : L =:= (H :: T)) : (F[H], Pack[F, T])
  }
  
  object Pack {
    implicit def packHNil[F[_]] =
      new Pack[F, HNil] {
        def split[H, T <: HList](implicit ev : HNil =:= (H :: T)) = {
          // We will never have evidence that HNil =:= (H :: T) so we can never get here,
          // but there doesn't seem to be any way to eliminate this method altogether
          // without doing something at least as bad elsewhere.
          sys.error("The impossible happened!")
        }
      }
    
    implicit def packHList[F[_], H0, T0 <: HList](implicit bh : F[H0], pt : Pack[F, T0]) =
      new Pack[F, H0 :: T0] {
        def split[H, T <: HList](implicit ev : (H0 :: T0) =:= (H :: T)) = {
          // The casts here can't fail, but there doesn't seem to be any way of
          // proving that from the evidence we have.
          (bh.asInstanceOf[F[H]], pt.asInstanceOf[Pack[F, T]])
        }
      }
  }

  trait Unpack[F[_], E, L <: HList] {
    def apply(p : Pack[F, L]) : F[E]
  }
  
  object Unpack {
    implicit def unpack1[F[_], H, T <: HList] = new Unpack[F, H, H :: T] {
      def apply(p : Pack[F, H :: T]) : F[H] = p.split[H, T]._1
    }

    implicit def unpack2[F[_], E, H, T <: HList](implicit ut : Unpack[F, E, T]) = new Unpack[F, E, H :: T] {
      def apply(p : Pack[F, H :: T]) : F[E] = ut(p.split[H, T]._2)
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
    import pack._

    (show(t), show(u), show(v))
  }
  
  assert(use3(a, b, c) == ("A", "B", "C"))
}
