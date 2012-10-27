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
  object Company {
    implicit def companyIso[D <: HList] = Iso.hlist(Company.apply[D] _, Company.unapply[D] _)
  }

  case class Dept[S <: HList](name : Name, manager : Manager, subunits : S)
  object Dept {
    implicit def deptIso[S <: HList] = Iso.hlist(Dept.apply[S] _, Dept.unapply[S] _)
  }

  case class Employee(person : Person, salary : Salary)
  object Employee {
    implicit val employeeIso = Iso.hlist(Employee.apply _, Employee.unapply _)
  }

  case class Person(name : Name, address : Address)
  object Person {
    implicit val personIso = Iso.hlist(Person.apply _, Person.unapply _)
  }

  case class Salary(salary : Double)
  object Salary {
    implicit val salaryIso = Iso.hlist(Salary.apply _, Salary.unapply _)
  }

  type Manager = Employee
  type Name = String
  type Address = String

  /*
  // The HListIso definitions for each case class above replace this manually
  // written SYB boilerplate
  
  implicit def companyDataT[F <: Poly, D <: HList](implicit fd : HomAux[F, D]) =
    new DataT[F, Company[D]] {
      def gmapT(c : Company[D]) = c match {
        case Company(depts) => Company(fd(depts))
      }
    }

  implicit def deptDataT[F <: Poly, S <: HList]
    (implicit fn : HomAux[F, Name], fm : HomAux[F, Manager], fs : HomAux[F, S]) =
      new DataT[F, Dept[S]] {
        def gmapT(d : Dept[S]) = d match {
          case Dept(name, manager, subunits) => Dept(fn(name), fm(manager), fs(subunits))
        }
      }

  implicit def employeeDataT[F <: Poly](implicit fp : HomAux[F, Person], fs : HomAux[F, Salary]) =
    new DataT[F, Employee] {
      def gmapT(e : Employee) = e match {
        case Employee(person, salary) => Employee(fp(person), fs(salary))
      }
    }

  implicit def personDataT[F <: Poly](implicit fn : HomAux[F, Name], fa : HomAux[F, Address]) =
    new DataT[F, Person] {
      def gmapT(p : Person) = p match {
        case Person(name, address) => Person(fn(name), fa(address))
      }
    }

  implicit def salaryDataT[F <: Poly](implicit fs : HomAux[F, Double]) =
    new DataT[F, Salary] {
      def gmapT(s : Salary) = s match {
        case Salary(salary) => Salary(fs(salary))
      }
    }
  */
  
  object raise extends Poly1 {
    implicit def apply[T] = at[T](t => t)
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
