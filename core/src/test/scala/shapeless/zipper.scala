/*
 * Copyright (c) 2011 Miles Sabin 
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

class ZipperTests {
  import HList._
  import Nat._
  import Zipper._
  
  def typed[T](t : => T) {}

  @Test
  def testTraversal {
    val l = 1 :: "foo" :: 3.0 :: HNil

    val z = l.toZipper
    
    val i = z.get
    typed[Int](i)
    assertEquals(1, i)
    
    val s = z.right.get
    typed[String](s)
    assertEquals("foo", s)
    
    val d = z.right.right.get
    typed[Double](d)
    assertEquals(3.0, d, Double.MinPositiveValue)
    
    val zl = z.last
    
    val d2 = zl.left.get
    typed[Double](d2)
    assertEquals(3.0, d2, Double.MinPositiveValue)
    
    val zf = zl.first
    
    val i2 = zf.get
    typed[Int](i2)
    assertEquals(1, i2)
  }

  @Test
  def testUpdate {
    val l = 1 :: "foo" :: 3.0 :: HNil

    val l2 = l.toZipper.right.put("wibble", 45).reify
    typed[Int :: (String, Int) :: Double :: HNil](l2)
    assertEquals(1 :: ("wibble", 45) :: 3.0 :: HNil, l2)
  
    val l3 = l.toZipper.right.delete.reify
    typed[Int :: Double :: HNil](l3)
    assertEquals(1 :: 3.0 :: HNil, l3)

    val l4 = l.toZipper.insert("bar").reify
    typed[String :: Int :: String :: Double :: HNil](l4)
    assertEquals("bar" :: 1 :: "foo" :: 3.0 :: HNil, l4)

    val l5 = l.toZipper.right.right.right.insert("bar").reify
    typed[Int :: String :: Double :: String :: HNil](l5)
    assertEquals(1 :: "foo" :: 3.0 :: "bar" :: HNil, l5)
  }

  @Test
  def testTypeIndexing {
    val l = 1 :: "foo" :: 3.0 :: HNil

    val l6 = l.toZipper.rightTo[Double]
    val d6 = l6.get
    typed[Double](d6)
    assertEquals(3.0, d6, Double.MinPositiveValue)

    val l7 = l.toZipper.last.leftTo[Int]
    val i7 = l7.get
    typed[Int](i7)
    assertEquals(1, i7)
  }
  
  @Test
  def testNatIndexing {
    val l = 1 :: "foo" :: 3.0 :: HNil

    val l8 = l.toZipper.rightBy(_2)
    val d8 = l8.get
    typed[Double](d8)
    assertEquals(3.0, d8, Double.MinPositiveValue)
    
    val l9 = l8.leftBy(_1)
    val s9 = l9.get
    typed[String](s9)
    assertEquals("foo", s9)
  }
  
  @Test
  def testEmpty {
    val l = HNil
    val z = l.toZipper
    
    val l2 = z.insert(23).insert("foo").insert(true).reify
    typed[Int :: String :: Boolean :: HNil](l2)
    assertEquals(23 :: "foo" :: true :: HNil, l2)
  }

  case class Address(street : String, city : String, postcode : String)
  case class Person(name : String, age : Int, address : Address)
  
  val p1 = Person("Joe Grey", 37, Address("Southover Street", "Brighton", "BN2 9UA"))
  
  case class Dept[E <: HList](manager : Employee, employees : E)
  case class Employee(name : String, salary : Int)
  
  type D = Dept[Employee :: Employee :: Employee :: HNil]

  val dept =
    Dept(
      Employee("Agamemnon", 5000),
      Employee("Menelaus", 3000) ::
      Employee("Achilles", 2000) ::
      Employee("Odysseus", 2000) ::
      HNil
    )
  
  @Test
  def testCaseClasses {
    val z = p1.toZipper
    
    val name = z.get
    typed[String](name)
    assertEquals("Joe Grey", name)
    
    val age = z.right.get
    typed[Int](age)
    assertEquals(37, age)
    
    val address = z.right.right.get
    typed[Address](address)
    assertEquals(Address("Southover Street", "Brighton", "BN2 9UA"), address)
    
    val street = z.right.right.down.get
    typed[String](street)
    assertEquals("Southover Street", street)
    
    val city = z.right.right.down.right.get
    typed[String](city)
    assertEquals("Brighton", city)
    
    val postcode = z.right.right.down.right.right.get
    typed[String](postcode)
    assertEquals("BN2 9UA", postcode)
    
    val updatedName = z.put("Fred Bloggs").reify
    typed[Person](updatedName)
    assertEquals(Person("Fred Bloggs", 37, Address("Southover Street", "Brighton", "BN2 9UA")), updatedName)
    
    val updatedCity = z.right.right.down.right.put("London")
    typed[Address](updatedCity.reify)
    assertEquals(Address("Southover Street", "London", "BN2 9UA"), updatedCity.reify)
    
    typed[Person](updatedCity.up.reify)
    assertEquals(Person("Joe Grey", 37, Address("Southover Street", "London", "BN2 9UA")), updatedCity.up.reify)

    typed[Person](updatedCity.root.reify)
    assertEquals(Person("Joe Grey", 37, Address("Southover Street", "London", "BN2 9UA")), updatedCity.up.reify)
    
    val reifiedAddress = z.right.right.down.reify
    typed[Address](reifiedAddress)
    
    val reifiedCity = z.right.right.down.right.reify
    typed[Address](reifiedCity)
    
    val underAddress = z.right.right.down
    val addressAgain = underAddress.up.get
    typed[Address](addressAgain)
    
    val root1 = z.root
    typed[Person](root1.reify)
    
    val root2 = underAddress.root
    typed[Person](root2.reify)
    
    val z2 = dept.toZipper
    
    val z3 = z2.down.put("King Agamemnon").right.put(8000).up.right.down.right.down.right.put(3000).root.reify
    typed[D](z3)
    assertEquals(
     Dept(
       Employee("King Agamemnon", 8000),
       Employee("Menelaus", 3000) ::
       Employee("Achilles", 3000) ::
       Employee("Odysseus", 2000) ::
       HNil), z3)
  }
}
