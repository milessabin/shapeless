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

import shapeless.ops.hlist.{Selector, Prepend}
import shapeless.test.illTyped

object TypeLevelBacktrack extends App {
  // -------------------------------------------------------------------------
  // Problem statement:
  // Given a genealogical tree, compute family relationship on the type level.
  // -------------------------------------------------------------------------

  /** [[Parent]] / [[Child]] relationship, father side. */
  trait FatherOf[Parent, Child]

  /** [[Parent]] / [[Child]] relationship, mother side. */
  trait MotherOf[Parent, Child]

  def fact[P, C](): FatherOf[P, C] = new FatherOf[P, C] {}
  def fact[P, C]()(implicit d: DummyImplicit): MotherOf[P, C] = new MotherOf[P, C] {}

  trait Bob;   trait Bill;  trait Stacy;  trait Ben
  trait Buffy; trait Sarah; trait Philip; trait Julie

  implicit val fact0:  Stacy MotherOf Bob   = fact()
  implicit val fact1: Philip FatherOf Bob   = fact()
  implicit val fact2:    Bob FatherOf Bill  = fact()
  implicit val fact3:   Bill FatherOf Buffy = fact()
  implicit val fact4:    Bob FatherOf Ben   = fact()
  implicit val fact5:  Julie MotherOf Ben   = fact()
  implicit val fact6:    Ben FatherOf Sarah = fact()

  // -------------------------------------------------------------------------
  // A Prolog style, direct encoding of this problem would use 4 rules:
  // - A FatherOf D => A IsAncestor D
  // - A MotherOf D => A IsAncestor D
  // - A FatherOf Z & Z IsAncestor D => A IsAncestor D
  // - A MotherOf Z & Z IsAncestor D => A IsAncestor D
  //
  // Translated to scala, it looks like the `IsAncestor` typeclass below.
  // -------------------------------------------------------------------------

  /** Typeclass witnessing that all [[Ancestor]] is an ancestor of [[Descendant]]. */
  trait IsAncestor[Ancestor, Descendant]

  object IsAncestor {
    def apply[A, D](implicit i: IsAncestor[A, D]): IsAncestor[A, D] = i

    implicit def directFather[A, D]
      (implicit e: FatherOf[A, D]) = new IsAncestor[A, D] {}

    implicit def directMother[A, D]
      (implicit e: MotherOf[A, D]) = new IsAncestor[A, D] {}

    implicit def fatherSideRelation[A, D, Z]
      (implicit e: FatherOf[A, Z], i: IsAncestor[Z, D]) = new IsAncestor[A, D] {}

    implicit def motherSideRelation[A, D, Z]
      (implicit e: MotherOf[A, Z], i: IsAncestor[Z, D]) = new IsAncestor[A, D] {}
  }

  // -------------------------------------------------------------------------
  // This approach kind of works, but it quickly hits an important limitation
  // of scala implicit search: it does not do backtracking. In this example
  // IsAncestor[Stacy, Buffy] is indeed true, but the complier gets confused.
  // -------------------------------------------------------------------------

  // Stacy   Philip
  //      \ /
  //      Bob    Julie
  //      / \   /
  //  Bill   Ben
  //    |     \
  //  Buffy  Sarah

  IsAncestor[Stacy, Bob]
  IsAncestor[Stacy, Bill]
  illTyped("IsAncestor[Stacy, Buffy]") // hmmm... why doesn't this compile?
  illTyped("IsAncestor[Ben, Bill]")

  // -------------------------------------------------------------------------
  // A possible workaround consists in computing *all* the ancestors or a
  // person in a `HList`, and define the family relationship in term of
  // containment in in the list of all ancestors.
  // -------------------------------------------------------------------------

  /** Typeclass computing all the [[Ancestors]] of a [[Person]]. */
  trait AllAncestors[Person, Ancestors <: HList]

  // This is used to lower the priority of the *base case*.
  trait AllAncestorsLowPrio {
    implicit def none[Person] = new AllAncestors[Person, HNil] {}
  }

  object AllAncestors extends AllAncestorsLowPrio {
    implicit def fatherSide[F, P, PA <: HList]
      (implicit m: FatherOf[F, P], a: AllAncestors[F, PA]) = new AllAncestors[P, F :: PA] {}

    implicit def motherSide[M, P, PA <: HList]
      (implicit m: MotherOf[M, P], a: AllAncestors[M, PA]) = new AllAncestors[P, M :: PA] {}

    implicit def bothSides[F, M, P, FA <: HList, MA <: HList, CA <: HList]
      (implicit
        l: FatherOf[F, P],
        r: MotherOf[M, P],
        f: AllAncestors[F, FA],
        m: AllAncestors[M, MA],
        p: Prepend.Aux[FA, MA, CA]
      ) = new AllAncestors[P, F :: M :: CA] {}
  }

  /** Typeclass witnessing family relationship between [[P2]] and [[P1]]. */
  class Relationship[P1, P2]

  object Relationship {
    def apply[D, A](implicit r: Relationship[D, A]): Relationship[D, A] = r

    implicit def caseP2AncestorOfP1[P1, P2, A <: HList]
      (implicit a: AllAncestors[P1, A], s: Selector[A, P2]) = new Relationship[P1, P2] {}

    implicit def caseP1AncestorOfP2[P1, P2, A <: HList]
      (implicit a: AllAncestors[P2, A], s: Selector[A, P1]) = new Relationship[P1, P2] {}
  }

  // Stacy   Philip
  //      \ /
  //      Bob    Julie
  //      / \   /
  //  Bill   Ben
  //    |     \
  //  Buffy  Sarah

  // Bob is in Relationship with everyone except Julie
  Relationship[Bob, Bill]
  Relationship[Bob, Stacy]
  Relationship[Bob, Ben]
  Relationship[Bob, Buffy]
  Relationship[Bob, Sarah]
  Relationship[Bob, Philip]
  illTyped("Relationship[Bob, Julie]")

  // Julie is only in Relationship with Ben and Sarah
  Relationship[Julie, Ben]
  Relationship[Julie, Sarah]
  illTyped("Relationship[Julie, Bob]")
  illTyped("Relationship[Julie, Bill]")
  illTyped("Relationship[Julie, Stacy]")
  illTyped("Relationship[Julie, Buffy]")
  illTyped("Relationship[Julie, Philip]")

  // Original test cases
  Relationship[Bob, Bill]
  Relationship[Stacy, Bill]
  Relationship[Stacy, Buffy]
  illTyped("Relationship[Ben, Bill]")
}
