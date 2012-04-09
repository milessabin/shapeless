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

/*
 * Examples of Lens usage.
 * 
 * @author Miles Sabin
 */
object LenseExamples extends App {
  import shapeless._
  import Lens._
  import Nat._

  // A pair of ordinary case classes ...
  case class Address(street : String, city : String, postcode : String)
  case class Person(name : String, age : Int, address : Address)
  
  // One line of boilerplate per case class ...
  implicit val addressIso = HListIso(Address.apply _, Address.unapply _)
  implicit val personIso = HListIso(Person.apply _, Person.unapply _)
  
  // Some lenses over Person/Address ...
  val nameLens     = Lens[Person] >> _0
  val ageLens      = Lens[Person] >> _1
  val addressLens  = Lens[Person] >> _2
  val streetLens   = Lens[Person] >> _2 >> _0
  val cityLens     = Lens[Person] >> _2 >> _1
  val postcodeLens = Lens[Person] >> _2 >> _2

  // 
  val person = Person("Joe Grey", 37, Address("Southover Street", "Brighton", "BN2 9UA"))
  
  val age1 = ageLens.get(person)
  assert(age1 == 37)

  val person2 = ageLens.set(person, 38)
  assert(person2.age == 38)
  
  val person3 = ageLens.modify(person2)(_ + 1)
  assert(person3.age == 39)
  
  val street = streetLens.get(person3)
  assert(street == "Southover Street")
  
  val person4 = streetLens.set(person3, "Montpelier Road")
  assert(person4.address.street == "Montpelier Road")
  
  assert(person4 == Person("Joe Grey", 39, Address("Montpelier Road", "Brighton", "BN2 9UA")))
}
