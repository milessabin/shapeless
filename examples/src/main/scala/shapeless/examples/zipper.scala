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
 * Examples of Zipper usage.
 * 
 * For some background see "Scrap Your Zippers: A Generic Zipper for
 * Heterogeneous Types", Michael D. Adams, School of Informatics and Computing,
 * Indiana University, http://www.cs.indiana.edu/~adamsmd/, presented at the
 * 2010 Workshop on Generic Programming.
 * 
 * The example below is taken from that paper, but the implementation is based
 * on HLists and so is probably more closely related to the one hypothesized in
 * section 6.1, Related Work, Generic Programming. Adams remarks "... it seems
 * that heterogeneous collections could be used instead of GADTs. Nevertheless,
 * encoding the type constraints on heterogeneous collections is a subtle and
 * delicate task, and it is not clear that these particular constraints are
 * expressible". The implementation here can be taken as a demonstration that
 * in Scala at least the constraints are relatively straightforwardly
 * expressible.
 * 
 * @author Miles Sabin
 */
object ZipperExamples extends App {
  import shapeless._
  import Zipper._

  def typed[T](t : => T) {}

  case class Dept[E <: HList](manager : Employee, employees : E)
  case class Employee(name : String, salary : Int)
  
  val dept =
    Dept(
      Employee("Agamemnon", 5000),
      Employee("Menelaus", 3000) ::
      Employee("Achilles", 2000) ::
      Employee("Odysseus", 2000) ::
      HNil
    )

  type D = Dept[Employee :: Employee :: Employee :: HNil]
    
  val z = dept.toZipper
  
  val g1 = z.reify
  typed[D](g1)
  println(g1)
  // Dept(Employee(Agamemnon,5000),Employee(Menelaus,3000) :: Employee(Achilles,2000) :: Employee(Odysseus,2000) :: HNil)
  
  val g2 = z.right.get
  typed[Employee :: Employee :: Employee :: HNil](g2)
  println(g2)
  // Employee(Menelaus,3000) :: Employee(Achilles,2000) :: Employee(Odysseus,2000) :: HNil
  
  val g3 = z.get
  typed[Employee](g3)
  println(g3)
  // Employee(Agamemnon,5000)
  
  val g4 = z.down.right.get
  typed[Int](g4)
  println(g4)
  // 5000
  
  val g5 = z.down.get
  typed[String](g5)
  println(g5)
  // Agamemnon
  
  val g6 = z.down.put("King Agamemnon")
  
  val g8 = g6.right.put(8000)
  
  val g9 = g8.up
  
  val agamemnon = g9.get
  typed[Employee](agamemnon)
  println(agamemnon)
  // Employee(King Agamemnon,8000)
  
  val updatedDept = g9.reify
  typed[D](updatedDept)
  println(updatedDept)
  // Dept(Employee(King Agamemnon,8000),Employee(Menelaus,3000) :: Employee(Achilles,2000) :: Employee(Odysseus,2000) :: HNil)
  
  val achillesRaise = g9.right.down.right.down.right.put(3000).root.reify
  typed[D](achillesRaise)
  println(achillesRaise)
  // Dept(Employee(King Agamemnon,8000),Employee(Menelaus,3000) :: Employee(Achilles,3000) :: Employee(Odysseus,2000) :: HNil)

  // All together in a single pass ...
  val singlePass =
    z.
      down.put("King Agamemnon").
      right.put(8000).
    up.right.
      down.
      right.
        down.
        right.put(3000).
    root.reify
    
  typed[D](singlePass)
  println(singlePass)
  // Dept(Employee(King Agamemnon,8000),Employee(Menelaus,3000) :: Employee(Achilles,3000) :: Employee(Odysseus,2000) :: HNil)
}
