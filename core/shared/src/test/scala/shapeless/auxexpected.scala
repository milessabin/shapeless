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

import shapeless.record.Record
import shapeless.testutil.assertTypedEquals

/**
 * Instances of type classes with output types are found where their output types are expected (see
 * https://github.com/scala/scala3/issues/24114).
 */
class AuxExpectedTests {
  @Test
  def testGeneric: Unit = {
    import GenericTestsAux._

    val gen = implicitly[Generic.Aux[Person, String :: String :: Int :: HNil]]
    assertEquals("Joe Soap" :: "Brighton" :: 23 :: HNil, gen.to(Person("Joe Soap", "Brighton", 23)))

    val fruitGen = implicitly[Generic.Aux[Fruit, Apple :+: Banana :+: Orange :+: Pear :+: CNil]]
    assertEquals(Inr(Inl(Banana())), fruitGen.to(Banana()))
  }

  @Test
  def testLabelledGeneric: Unit = {
    import LabelledGenericTestsAux._

    val gen = implicitly[LabelledGeneric.Aux[Book, BookRec]]
    assertEquals(taplRecord, gen.to(tapl))
  }

  @Test
  def testDefault: Unit = {
    import DefaultTestDefinitions._

    type Out = None.type :: Some[String] :: Some[Option[Boolean]] :: HNil
    val expected: Out = None :: Some("b") :: Some(Some(true)) :: HNil

    val default0: Default.Aux[CC, Out] = the[Default[CC]]
    assertTypedEquals[Out](expected, default0())

    val default1 = implicitly[Default.Aux[CC, Out]]
    assertTypedEquals[Out](expected, default1())

    type RecordOut = Record.`"s" -> String, "flagOpt" -> Option[Boolean]`.T
    val asRecord = implicitly[Default.AsRecord.Aux[CC, RecordOut]]
    assertTypedEquals[RecordOut](Record(s = "b", flagOpt = Some(true)), asRecord())

    def defaultsLength[T, L <: HList](implicit default: Default.Aux[T, L], length: ops.hlist.Length[L]): length.Out =
      length()
    assertTypedEquals[Nat._3](Nat._3, defaultsLength[CC, Out])
  }
}
