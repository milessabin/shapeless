/*
 * Copyright (c) 2026 Miles Sabin
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

import org.junit.Test
import org.junit.Assert._

package Generic1TestsAux {
  trait Pure[G[_]] { def pure[A](a: A): G[A] }

  object Pure {
    implicit val optionPure: Pure[Option] = new Pure[Option] { def pure[A](a: A): Option[A] = Some(a) }
  }

  // A type class of two type constructors, derived via instances for partial applications of it
  trait Lift[F[_], G[_]] { def apply[A](fa: F[A]): F[G[A]] }

  object Lift extends Lift0 {
    implicit def hnil[G[_]]: Lift[Const[HNil]#λ, G] =
      new Lift[Const[HNil]#λ, G] { def apply[A](fa: HNil): HNil = fa }

    implicit def hcons[L[_], G[_]](
      implicit ihc: IsHCons1[L, Functor, ({ type λ[t[_]] = Lift[t, G] })#λ], g: Pure[G]
    ): Lift[L, G] =
      new Lift[L, G] {
        def apply[A](la: L[A]): L[G[A]] = {
          val (hd, tl) = ihc.unpack(la)
          ihc.pack((ihc.fh.map(hd)(g.pure), ihc.ft(tl)))
        }
      }
  }

  trait Lift0 {
    implicit def generic[F[_], G[_]](implicit gen: Generic1[F, ({ type λ[t[_]] = Lift[t, G] })#λ]): Lift[F, G] =
      new Lift[F, G] { def apply[A](fa: F[A]): F[G[A]] = gen.from(gen.fr(gen.to(fa))) }
  }
}

/** Derivations of type classes of two type constructors via `Generic1` instances for partial applications of them */
class Generic1LiftTests {
  import Generic1TestsAux._

  @Test
  def testPartiallyAppliedDerivation: Unit = {
    type L[t] = t :: List[t] :: HNil
    assertEquals(Some(1) :: List(Some(2), Some(3)) :: HNil, implicitly[Lift[L, Option]].apply(1 :: List(2, 3) :: HNil))
    assertEquals(Prod(Some(1), List(Some(2), Some(3))), implicitly[Lift[Prod, Option]].apply(Prod(1, List(2, 3))))
  }
}
