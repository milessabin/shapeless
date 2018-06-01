/*
 * Copyright (c) 2013-16 Miles Sabin
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

import scala.language.reflectiveCalls

import org.junit.Test
import org.junit.Assert._

class LazyStrictTestsJVM {

  case class CC(l: List[CC])

  trait TC[T] {
    def repr(depth: Int): String
  }

  object TC {
    def apply[T](implicit tc: TC[T]): TC[T] = tc

    def instance[T](repr0: Int => String): TC[T] =
      new TC[T] {
        def repr(depth: Int) =
          if (depth < 0)
            "…"
          else
            repr0(depth)
      }
  }

  object TC0 extends TCImplicits[Lazy,   Strict, Strict]
  object TC1 extends TCImplicits[Strict, Lazy,   Strict]
  object TC2 extends TCImplicits[Strict, Strict, Lazy  ]
  object TC3 extends TCImplicits[Strict, Strict, Strict]

  trait TCImplicits[A[T] <: { def value: T }, B[T] <: { def value: T }, C[T] <: { def value: T }] {
    implicit def listTC[T](implicit underlying: A[TC[T]]): TC[List[T]] =
      TC.instance(depth => s"List(${underlying.value.repr(depth - 1)})")

    implicit def hnilTC: TC[HNil] =
      TC.instance(_ => "HNil")

    implicit def hconsTC[H, T <: HList]
     (implicit
       headTC: B[TC[H]],
       tailTC: TC[T]
     ): TC[H :: T] =
      TC.instance(depth => s"${headTC.value.repr(depth - 1)} :: ${tailTC.repr(depth)}")

    implicit def genericTC[F, G]
     (implicit
       gen: Generic.Aux[F, G],
       underlying: C[TC[G]]
     ): TC[F] =
      TC.instance(depth => s"Generic(${underlying.value.repr(depth - 1)})")
  }

  /** Illustrates that a single `Lazy` is enough to break a cycle */
  @Test
  def testCycle: Unit = {
    val (ccTC0, genTC0, listTC0) = {
      import TC0._
      (TC[CC], TC[List[CC] :: HNil], TC[List[CC]])
    }

    val (ccTC1, genTC1, listTC1) = {
      import TC1._
      (TC[CC], TC[List[CC] :: HNil], TC[List[CC]])
    }

    val (ccTC2, genTC2, listTC2) = {
      import TC2._
      (TC[CC], TC[List[CC] :: HNil], TC[List[CC]])
    }

    val (ccTC3SO, genTC3SO, listTC3SO) = {
      import TC3._
      def throwsStackOverflow[T](f: => T): Boolean =
        try { f; false }
        catch { case _: StackOverflowError => true }

      (throwsStackOverflow(TC[CC]), throwsStackOverflow(TC[List[CC] :: HNil]), throwsStackOverflow(TC[List[CC]]))
    }

    val expectedCCRepr = "Generic(List(Generic(List(Generic(… :: HNil)) :: HNil)) :: HNil)"
    val expectedGenRepr = "List(Generic(List(Generic(List(…) :: HNil)) :: HNil)) :: HNil"
    val expectedListRepr = "List(Generic(List(Generic(List(Generic(…)) :: HNil)) :: HNil))"

    assert(ccTC0.repr(7) == expectedCCRepr)
    assert(genTC0.repr(7) == expectedGenRepr)
    assert(listTC0.repr(7) == expectedListRepr)

    assert(ccTC1.repr(7) == expectedCCRepr)
    assert(genTC1.repr(7) == expectedGenRepr)
    assert(listTC1.repr(7) == expectedListRepr)

    assert(ccTC2.repr(7) == expectedCCRepr)
    assert(genTC2.repr(7) == expectedGenRepr)
    assert(listTC2.repr(7) == expectedListRepr)

    assert(ccTC3SO)
    assert(genTC3SO)
    assert(listTC3SO)
  }
}

