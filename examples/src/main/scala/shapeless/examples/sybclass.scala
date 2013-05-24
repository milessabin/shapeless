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
 * Examples of Scrap Your Boilerplate in action
 * 
 * @author Miles Sabin
 */
object SybClassExamples extends App {
  import shapeless._
  import Poly._
  import SybClass._

  // Example taken from the original SYB paper: 
  // "Scrap your boilerplate: a practical approach to generic programming", Ralf Laemmel, Simon Peyton Jones
  //   http://research.microsoft.com/en-us/um/people/simonpj/papers/hmap/
  case class Company[D <: HList](depts : D)
  case class Dept[S <: HList](name : Name, manager : Manager, subunits : S)
  case class Employee(person : Person, salary : Salary)
  case class Person(name : Name, address : Address)
  case class Salary(salary : Double)

  type Manager = Employee
  type Name = String
  type Address = String

  object raise extends Poly1 {
    implicit def apply[T] = at[T](identity)
    implicit def caseDouble = at[Double](_*1.1)
  }

  val beforeRaise =
    Company(
      Dept("Research",
        Employee(Person("Ralf", "Amsterdam"), Salary(8000)),
        ( Employee(Person("Joost", "Amsterdam"), Salary(1000)) ::
          Employee(Person("Marlow", "Cambridge"), Salary(2000)) ::
          HNil
        )
      ) ::
      Dept("Strategy",
        Employee(Person("Blair", "London"), Salary(100000)),
        HNil
      ) ::
      HNil
    )
    
  println(beforeRaise)
  /* Output:
   * Company(
   *   Dept(
   *     Research,Employee(Person(Ralf,Amsterdam),Salary(8000.0)),
   *       Employee(Person(Joost,Amsterdam),Salary(1000.0)) ::
   *       Employee(Person(Marlow,Cambridge),Salary(2000.0)) :: HNil) ::
   *   Dept(Strategy,Employee(Person(Blair,London),Salary(100000.0)),HNil) :: HNil)
   */

  // Compute a new company structure with all salaries increased by 10%
  val afterRaise = everywhere(raise)(beforeRaise)
  println(afterRaise)
  /* Output:
   * Company(
   *   Dept(
   *     Research,Employee(Person(Ralf,Amsterdam),Salary(8800.0)),
   *       Employee(Person(Joost,Amsterdam),Salary(1100.0)) ::
   *       Employee(Person(Marlow,Cambridge),Salary(2200.0)) :: HNil) ::
   *   Dept(Strategy,Employee(Person(Blair,London),Salary(110000)),HNil) :: HNil)}
   * 
   */
}
