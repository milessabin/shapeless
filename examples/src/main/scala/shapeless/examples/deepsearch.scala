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

/**
 * Searching arbitrarily nested case classes, tuples, and lists.
 *
 * @author Travis Brown
 */
object DeepSearchExamples extends App {
  import shapeless._

  // Evidence that an A is something that we can look around in for Qs that
  // satisfy some predicate.
  trait Searchable[A, Q] {
    def find(p: Q => Boolean)(a: A): Option[Q]
  }

  implicit def elemSearchable[A] = new Searchable[A, A] {
    def find(p: A => Boolean)(a: A) = if (p(a)) Some(a) else None
  }

  implicit def listSearchable[A, Q](implicit s: Searchable[A, Q]) =
    new Searchable[List[A], Q] {
      def find(p: Q => Boolean)(a: List[A]) = a.flatMap(s.find(p)).headOption
    }

  implicit def hnilSearchable[Q] = new Searchable[HNil, Q] {
    def find(p: Q => Boolean)(a: HNil) = None
  }

  implicit def hlistSearchable[H, T <: HList, Q](
    implicit hs: Searchable[H, Q] = null, ts: Searchable[T, Q]
  ) = new Searchable[H :: T, Q] {
    def find(p: Q => Boolean)(a: H :: T) =
      Option(hs).flatMap(_.find(p)(a.head)) orElse ts.find(p)(a.tail)
  }

  implicit def hlistishSearchable[A, L <: HList, Q](
    implicit iso: Iso[A, L], s: Searchable[L, Q]
  ) = new Searchable[A, Q] {
    def find(p: Q => Boolean)(a: A) = s.find(p)(iso to a)
  }

  case class SearchableWrapper[A](a: A) {
    def deepFind[Q](p: Q => Boolean)(implicit s: Searchable[A, Q]) =
      s.find(p)(a)
  }

  implicit def wrapSearchable[A](a: A) = SearchableWrapper(a)

  // An example predicate:
  val p = (_: String) endsWith "o"

  // On strings:
  assert("hello".deepFind(p) == Some("hello"))
  assert("hell".deepFind(p) == None)

  // On lists:
  assert(List("yes", "maybe", "no").deepFind(p) == Some("no"))

  // On arbitrarily sized and nested tuples:
  assert(("yes", "maybe", ("no", "why")).deepFind(p) == Some("no"))
  assert(("a", ("b", "c"), "d").deepFind(p) == None)

  // On tuples with non-string elements:
  assert((1, "two", ('three, '4')).deepFind(p) == Some("two"))

  // Search the same tuple for a specific character instead:
  assert((1, "two", ('three, '4')).deepFind((_: Char) == 52) == Some('4'))

  // Our case class:
  case class Foo(a: String, b: String, c: List[String])

  // And it works:
  assert(Foo("four", "three", List("two", "one")).deepFind(p) == Some("two"))
  assert(Foo("a", "b", "c" :: Nil).deepFind(p) == None)
}
