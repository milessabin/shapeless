
/*
 * Copyright (c) 2016 Miles Sabin
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

object WrappedTests {
  trait TestTag
  case class AvWrapper(stringValue: String) extends AnyVal
}
import WrappedTests._

class UnwrappedTests {

  sealed trait Pass[T] {
    type U
    def actual(t: T): T
    def unwrapped(t: T): U
    def wrapped(u: U): T
  }
  object Pass {
    type Aux[T, U0] = Pass[T] { type U = U0 }
    implicit def unwrappedPasses[W, U0](implicit uw: Unwrapped.Aux[W, U0]): Pass.Aux[W, U0] =
      new Pass[W] {
        type U = U0
        def actual(w: W): W = w
        def unwrapped(w: W): U = uw.unwrap(w)
        def wrapped(u: U): W = uw.wrap(u)
      }
  }

  @Test
  def testAnyVal: Unit = {
    val pass = the[Pass[AvWrapper]]
    the[pass.U =:= String]
    val avw = AvWrapper("testing")
    val actual = pass.actual(avw)
    val wrapped = pass.wrapped(avw.stringValue)
    val unwrapped = pass.unwrapped(avw)
    assert((actual: AvWrapper) == avw)
    assert((wrapped: AvWrapper) == avw)
    assert((unwrapped: String) == avw.stringValue)
  }

  @Test
  def testNewtype: Unit = {
    import newtype._
    case class MyStringOps(s: String) {
      def stringValue: String = s
    }
    type MyString = Newtype[String, MyStringOps]
    def MyString(s : String) : MyString = newtype(s)
    implicit val mkOps = MyStringOps

    val pass = the[Pass[MyString]]
    the[pass.U =:= String]
    val ms = MyString("testing")
    val actual = pass.actual(ms)
    val wrapped = pass.wrapped(ms.stringValue)
    val unwrapped = pass.unwrapped(ms)
    assert((actual: MyString) == ms)
    assert((wrapped: MyString) == ms)
    assert((unwrapped: String) == ms.stringValue)
  }

  @Test
  def testScalazTagged: Unit = {

    type Tagged[A, T] = { type Tag = T; type Self = A }
    type @@[T, Tag] = Tagged[T, Tag]

    def tag[U] = new Tagger[U]
    class Tagger[U] {
      def apply[T](t : T) : T @@ U = t.asInstanceOf[T @@ U]
    }
    def value[T](t: Tagged[T, _]): T = t.asInstanceOf[T]

    implicit def taggedUnwrapped[UI, T, UF](implicit chain: Lazy[Unwrapped.Aux[UI, UF]]) =
      new Unwrapped[UI @@ T] {
        type U = UF
        def unwrap(w: UI @@ T) = chain.value.unwrap(value(w))
        def wrap(u: UF) = tag[T](chain.value.wrap(u))
      }

    val pass = the[Pass[String @@ TestTag]]
    the[pass.U =:= String]
    val tagged = tag[TestTag]("testing")
    val actual = pass.actual(tagged)
    val wrapped = pass.wrapped(value(tagged))
    val unwrapped = pass.unwrapped(tagged)
    assert((actual: String @@ TestTag) == tagged)
    assert((wrapped: String @@ TestTag) == tagged)
    assert((unwrapped: String) == value(tagged))
    test.illTyped("unwrapped: String @@ TestTag")
  }

  @Test
  def testAlreadyUnwrapped: Unit = {

    val pass = the[Pass[String]]
    the[pass.U =:= String]
    val raw = "testing"
    val actual = pass.actual(raw)
    val wrapped = pass.wrapped(raw)
    val unwrapped = pass.unwrapped(raw)
    assert((actual: String) == raw)
    assert((wrapped: String) == raw)
    assert((unwrapped: String) == raw)
  }

  @Test
  def unwrapsChain: Unit = {
    import newtype._

    case class MyStringOps(s: AvWrapper) {
      def stringValue: String = s.stringValue
    }
    type MyString = Newtype[AvWrapper, MyStringOps]
    def MyString(a: AvWrapper): MyString = newtype(a)
    implicit val mkOps = MyStringOps


    val ms = MyString(AvWrapper("testing"))

    val pass = the[Pass[MyString]]
    the[pass.U =:= String]
    val actual = pass.actual(ms)
    val wrapped = pass.wrapped(ms.stringValue)
    val unwrapped = pass.unwrapped(ms)
    assert((actual: MyString) == ms)
    assert((wrapped: MyString) == ms)
    assert((unwrapped: String) == ms.stringValue)
  }

  @Test
  def testSyntax: Unit = {
    import syntax.unwrapped._
    val w = "testing".wrap[AvWrapper]
    test.typed(w: AvWrapper)
    val u = w.unwrap
    test.typed(u: String)
  }

}
