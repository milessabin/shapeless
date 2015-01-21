/*
 * Copyright (c) 2013-14 Lars Hupel
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

import shapeless._

object ShowExamples extends App {
  import ShowSyntax._

  sealed trait Super
  case class Foo(i: Int, s: String) extends Super
  case class Bar(i: Int) extends Super
  case class BarRec(i: Int, rec: Super) extends Super

  object Super {
    implicit val instance = Show[Super]
  }

  sealed trait MutualA
  case class MutualA1(x: Int) extends MutualA
  case class MutualA2(b: MutualB) extends MutualA

  sealed trait MutualB
  case class MutualB1(x: Int) extends MutualB
  case class MutualB2(b: MutualA) extends MutualB

  object MutualA {
    implicit val aInstance = Show[MutualA]
  }

  object MutualB {
    implicit val bInstance = Show[MutualB]
  }

  val bar: Super = Bar(0)
  val rec: Super = BarRec(1, Foo(0, "foo"))

  assert(bar.show == "Bar(i = 0)")
  assert(rec.show == "BarRec(i = 1, rec = Foo(i = 0, s = foo))")

  val mutual: MutualA = MutualA2(MutualB2(MutualA1(0)))

  assert(mutual.show == "MutualA2(b = MutualB2(b = MutualA1(x = 0)))")
}

trait ShowSyntax {
  def show: String
}

object ShowSyntax {
  implicit def showSyntax[T](a: T)(implicit st: Show[T]): ShowSyntax = new ShowSyntax {
    def show = st.show(a)
  }
}

trait Show[T] {
  def show(t: T): String
}

object Show extends LabelledTypeClassCompanion[Show] {
  implicit def stringShow: Show[String] = new Show[String] {
    def show(t: String) = t
  }

  implicit def intShow: Show[Int] = new Show[Int] {
    def show(n: Int) = n.toString
  }

  object typeClass extends LabelledTypeClass[Show] {
    def emptyProduct = new Show[HNil] {
      def show(t: HNil) = ""
    }

    def product[F, T <: HList](name: String, sh: Show[F], st: Show[T]) = new Show[F :: T] {
      def show(ft: F :: T) = {
        val head = sh.show(ft.head)
        val tail = st.show(ft.tail)
        if (tail.isEmpty)
          s"$name = $head"
        else
          s"$name = $head, $tail"
      }
    }

    def emptyCoproduct = new Show[CNil] {
      def show(t: CNil) = ""
    }

    def coproduct[L, R <: Coproduct](name: String, sl: => Show[L], sr: => Show[R]) = new Show[L :+: R] {
      def show(lr: L :+: R) = lr match {
        case Inl(l) => s"$name(${sl.show(l)})"
        case Inr(r) => s"${sr.show(r)}"
      }
    }

    def project[F, G](instance: => Show[G], to: F => G, from: G => F) = new Show[F] {
      def show(f: F) = instance.show(to(f))
    }
  }
}
