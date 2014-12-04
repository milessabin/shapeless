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

import shapeless._, labelled.FieldType, syntax.singleton._

object ShowExamples extends App {
  import ShowSyntax._

  sealed trait Super
  case class Foo(i : Int, s : String) extends Super
  case class Bar(i : Int) extends Super
  case class BarRec(i : Int, rec: Super) extends Super

  sealed trait MutualA
  case class MutualA1(x: Int) extends MutualA
  case class MutualA2(b: MutualB) extends MutualA

  sealed trait MutualB
  case class MutualB1(x: Int) extends MutualB
  case class MutualB2(b: MutualA) extends MutualB

  val bar: Super = Bar(0)
  val rec: Super = BarRec(1, Foo(0, "foo"))

  assert(bar.show == "Bar(i = 0)")
  assert(rec.show == "BarRec(i = 1, rec = Foo(i = 0, s = foo))")

  val mutual: MutualA = MutualA2(MutualB2(MutualA1(0)))

  assert(mutual.show == "MutualA2(b = MutualB2(b = MutualA1(x = 0)))")
}

trait ShowSyntax {
  def show : String
}

object ShowSyntax {
  implicit def showSyntax[T](a : T)(implicit st : Show[T]) : ShowSyntax = new ShowSyntax {
    def show = st.show(a)
  }
}

trait Show[T] {
  def show(t: T): String
}

object Show {
  def apply[T](implicit st: Lazy[Show[T]]): Show[T] = st.value

  implicit def stringShow: Show[String] = new Show[String] {
    def show(t: String) = t
  }

  implicit def intShow: Show[Int] = new Show[Int] {
    def show(n: Int) = n.toString
  }

  implicit def deriveHNil: Show[HNil] =
    new Show[HNil] {
      def show(p: HNil): String = ""
    }

  implicit def deriveHCons[K <: Symbol, V, T <: HList]
    (implicit
      key: Witness.Aux[K],
      sv: Lazy[Show[V]],
      st: Lazy[Show[T]]
    ): Show[FieldType[K, V] :: T] =
      new Show[FieldType[K, V] :: T] {
        def show(p: FieldType[K, V] :: T): String = {
          val head = s"${key.value.name} = ${sv.value.show(p.head)}"
          val tail = st.value.show(p.tail)
          if(tail.isEmpty) head else s"$head, $tail"
        }
      }

  implicit def deriveCNil: Show[CNil] =
    new Show[CNil] {
      def show(p: CNil): String = ""
    }

  implicit def deriveCCons[K <: Symbol, V, T <: Coproduct]
    (implicit
      key: Witness.Aux[K],
      sv: Lazy[Show[V]],
      st: Lazy[Show[T]]
    ): Show[FieldType[K, V] :+: T] =
      new Show[FieldType[K, V] :+: T] {
        def show(c: FieldType[K, V] :+: T): String =
          c match {
            case Inl(l) => s"${key.value.name}(${sv.value.show(l)})"
            case Inr(r) => st.value.show(r)
          }
      }

  implicit def deriveInstance[F, G](implicit gen: LabelledGeneric.Aux[F, G], sg: Lazy[Show[G]]): Show[F] =
    new Show[F] {
      def show(f: F) = sg.value.show(gen.to(f))
    }
}
