/*
 * Copyright (c) 2014 Miles Sabin 
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
package examples

import org.junit.Test
import org.junit.Assert.assertEquals

import labelled.{ field, FieldType }
import ops.hlist.ToCoproduct
import ops.union.Keys
import syntax.typeable._
import test._
import union._

package DistributeTestsAux {
  sealed trait MyTrait
  case class Foo(d: Double) extends MyTrait
  case class Bar(d: Double) extends MyTrait

  trait Thing[T] {
    def thing(t: T): Double
    def unthing(d: Double): T
  }

  object Thing {
    implicit val tf: Thing[Foo] =
      new Thing[Foo] {
        def thing(t: Foo): Double = t.d
        def unthing(d: Double): Foo = Foo(d)
      }
    implicit val tb: Thing[Bar] =
      new Thing[Bar] {
        def thing(t: Bar): Double = t.d
        def unthing(d: Double): Bar = Bar(d)
      }
  }

  trait Distribute[C <: Coproduct] extends DepFn1[Any] { type Out = Option[C] }

  object Distribute {
    def apply[C <: Coproduct](implicit dist: Distribute[C]) = dist
    
    implicit val cnilDistibute: Distribute[CNil] =
      new Distribute[CNil] {
        def apply(t: Any): Option[CNil] = None
      }

    implicit def cconsDistribute[CH, CT <: Coproduct]
      (implicit th: Typeable[CH], dt: Distribute[CT]): Distribute[CH :+: CT] =
        new Distribute[CH :+: CT] {
          def apply(t: Any): Option[CH :+: CT] = t.cast[CH].map(Inl(_)).orElse(dt(t).map(Inr(_)))
        }
  }

  trait Cram[T] {
    def apply(s: String, d: Double): Option[T]
  }

  object Cram {
    implicit def mkCram[T, C <: Coproduct](implicit lgen: LabelledGeneric.Aux[T, C], cramc: CramC[C]): Cram[T] =
      new Cram[T] {
        def apply(s: String, d: Double): Option[T] = cramc(Symbol(s), d).map(lgen.from)
      }
  }

  trait CramC[C <: Coproduct] {
    def apply(s: Symbol, d: Double): Option[C]
  }

  object CramC {
    implicit val cnilCramC: CramC[CNil] =
      new CramC[CNil] {
        def apply(s: Symbol, d: Double): Option[CNil] = None
      }

    implicit def cconsCramC[HK, HV, CT <: Coproduct]
      (implicit key: Typeable[HK], thing: Thing[HV], cct: CramC[CT]): CramC[FieldType[HK, HV] :+: CT] =
        new CramC[FieldType[HK, HV] :+: CT] {
          def apply(s: Symbol, d: Double): Option[FieldType[HK, HV] :+: CT] =
            s.cast[HK].map(_ => Inl(field[HK](thing.unthing(d)))).orElse(cct(s, d).map(Inr(_)))
        }
  }
}

class DistributeTests {
  import DistributeTestsAux._

  val wFoo = Witness("foo")
  val wBar = Witness("bar")
  val wBaz = Witness("baz")

  type Foo = wFoo.T
  type Bar = wBar.T
  type Baz = wBaz.T

  type Choices = Foo :+: Bar :+: Baz :+: CNil

  @Test
  def testBasics {
    val dist = Distribute[Choices]
    val dbar = dist("bar": Any)
    typed[Option[Choices]](dbar)
    assertEquals(Some(Inr(Inl("bar"))), dbar)

    val dwibble = dist("wibble": Any)
    typed[Option[Choices]](dwibble)
    assertEquals(None, dwibble)
  }

  def cram[T](s: String, d: Double)(implicit cram: Cram[T]): Option[T] = cram(s, d)

  @Test
  def testThing {
    val d: Double = 23.0

    val tag: String = "Foo"
    val body: Double = d

    val res = cram[MyTrait](tag, body)
    assertEquals(Some(Foo(d)), res)
  }
}

