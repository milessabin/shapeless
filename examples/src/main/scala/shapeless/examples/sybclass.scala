/*
 * Copyright (c) 2012-13 Miles Sabin 
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
import poly._

/*
 * Examples of Scrap Your Boilerplate in action
 * 
 * @author Miles Sabin
 */
object SybClassExamples {

  // Example taken from the original SYB paper: 
  // "Scrap your boilerplate: a practical approach to generic programming", Ralf Laemmel, Simon Peyton Jones
  //   http://research.microsoft.com/en-us/um/people/simonpj/papers/hmap/
  case class Company(depts : List[Dept])
  
  sealed trait Subunit 
  case class Dept(name : Name, manager : Manager, subunits : List[Subunit]) extends Subunit
  case class Employee(person : Person, salary : Salary) extends Subunit

  case class Person(name : Name, address : Address)
  case class Salary(salary : Int)

  type Manager = Employee
  type Name = String
  type Address = String

  def paradise : Unit = {
    val beforeRaise =
      Company(
        List(
          Dept("Research",
            Employee(Person("Ralf", "Amsterdam"), Salary(8000)),
            List(
              Employee(Person("Joost", "Amsterdam"), Salary(1000)),
              Employee(Person("Marlow", "Cambridge"), Salary(2000))
            )
          ),
          Dept("Strategy",
            Employee(Person("Blair", "London"), Salary(100000)),
            List()
          )
        )
      )
  
    // Compute a new company structure with all salaries increased by 10%

    object raise extends ->((i: Int) => i*110/100)
    val afterRaise = everywhere(raise)(beforeRaise)
    println(afterRaise)

    val expected =
      Company(
        List(
          Dept("Research",
            Employee(Person("Ralf", "Amsterdam"), Salary(8800)),
            List(
              Employee(Person("Joost", "Amsterdam"), Salary(1100)),
              Employee(Person("Marlow", "Cambridge"),Salary(2200))
            )
          ),
          Dept("Strategy",
            Employee(Person("Blair", "London"),Salary(110000)),
            List()
          )
        )
      )
      
    assert(afterRaise == expected)
  }
  
  sealed trait Tree[T]
  case class Leaf[T](t: T) extends Tree[T]
  case class Node[T](left: Tree[T], right: Tree[T]) extends Tree[T]
  
  def recursion : Unit = {
    val tree: Tree[Int] =
      Node(
        Node(
          Node(
            Leaf(1),
            Node(
              Leaf(2),
              Leaf(3)
            )
          ),
          Leaf(4)
        ),
        Node(
          Leaf(5),
          Leaf(6)
        )
      )

    object inc extends ->((i: Int) => i+1)
    val result = everywhere(inc)(tree)
    println(result)
    
    val expected: Tree[Int] =
      Node(
        Node(
          Node(
            Leaf(2),
            Node(
              Leaf(3),
              Leaf(4)
            )
          ),
          Leaf(5)
        ),
        Node(
          Leaf(6),
          Leaf(7)
        )
      )

    assert(expected == result)
  }

  def main(args: Array[String]): Unit = {
    paradise
    recursion
  }
}
