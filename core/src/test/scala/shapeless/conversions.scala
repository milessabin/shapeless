/*
 * Copyright (c) 2011-14 Miles Sabin 
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

class ConversionTests {
  import ops.function.{ FnToProduct, FnFromProduct }
  import syntax.std.function._
  import syntax.std.tuple._
  import test._

  @Test
  def testTuples: Unit = {
    val t1 = (23, "foo", 2.0, true)
    
    val h1 = t1.productElements
    typed[Int :: String :: Double :: Boolean :: HNil](h1)
    assertEquals(23 :: "foo" :: 2.0 :: true :: HNil, h1)
    
    val h2 = productElements(t1)
    typed[Int :: String :: Double :: Boolean :: HNil](h2)
    assertEquals(23 :: "foo" :: 2.0 :: true :: HNil, h2)
    
    val l2 = 23 :: "foo" :: 2.0 :: true :: HNil
    
    val t3 = l2.tupled
    typed[(Int, String, Double, Boolean)](t3)
    assertEquals((23, "foo", 2.0, true), t3)
    
    val t4 = tupled(l2)
    typed[(Int, String, Double, Boolean)](t4)
    assertEquals((23, "foo", 2.0, true), t4)
    
    val t5 = (23, "foo")
    val t6 = (false, 3.0)
    
    val t7 = (t5.productElements ::: t6.productElements).tupled
    typed[(Int, String, Boolean, Double)](t7)
    assertEquals((23, "foo", false, 3.0), t7)
    
    val t8 = (Set(2), Set("foo"))
    val t8b = (t8.productElements map choose).tupled
    typed[(Option[Int], Option[String])](t8b)
    assertEquals((Option(2), Option("foo")), t8b)
  }
  
  @Test
  def testFunctions: Unit = {
    val sum : (Int, Int) => Int = _+_
    val prd : (Int, Int, Int) => Int = _*_*_

    val hlsum = sum.toProduct
    typed[(Int :: Int :: HNil) => Int](hlsum)

    val hlprd = prd.toProduct
    typed[(Int :: Int :: Int :: HNil) => Int](hlprd)

    trait A
    trait B extends A
    trait C extends A

    val a = new A {}
    val b = new B {}

    val ab : A => B = (a : A) => b

    val hlab = ab.toProduct
    typed[(A :: HNil) => B](hlab)

    def foo[F, L <: HList, R](f : F, l : L)(implicit fntp: FnToProduct.Aux[F, L => R]) = fntp(f)(l)
    val s2 = foo(sum, 2 :: 3 :: HNil)
    val ab2 = foo(ab, a :: HNil)

    class HListSyntax[A <: HList, F <: AnyRef](a: A) {
      def applied[U](f: F)(implicit cftp: FnToProduct.Aux[f.type, A => U]): U = cftp(f)(a)
    }

    implicit def mkSyntax[A <: HList, F <: AnyRef](a: A)
      (implicit ffp: FnFromProduct.Aux[A => Any, F]): HListSyntax[A, F] =
      new HListSyntax[A, F](a)

    val res = (2 :: "a" :: 1.3 :: HNil) applied ((i, s, d) => (s * i, d * i)) // Function argument types inferred

    assert((res: (String, Double)) == ("aa", 2.6))

  }
  
  @Test
  def testCaseClasses: Unit = {
    case class Foo(a : Int, b : String, c : Double)
    
    val f1 = Foo(23, "foo", 2.3)
    val t1 = Foo.unapply(f1).get
    val hf = t1.productElements
    val f2 = Foo.tupled(hf.tupled)
    assertEquals(f1, f2)
  }
}
