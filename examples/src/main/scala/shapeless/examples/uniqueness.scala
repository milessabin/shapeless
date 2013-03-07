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
 * Determining whether an `HList` contains a given type (optionally including
 * subtypes) exactly once.
 *
 * See this Shapeless Dev mailing list question for the original framing and
 * some additional discussion: 
 *
 * https://groups.google.com/d/msg/shapeless-dev/HvV63toSBNM/K84lmwSsYzwJ
 *
 * @author Travis Brown
 */
object UniquenessExample extends App {
  import shapeless._

  /**
   * We'll enrich `HList` with a `unique` method that takes a type parameter,
   * gathers evidence that the list contains that type exactly once, and
   * returns the element of that type. It will not compile if the type does
   * not occur exactly once in the list.
   */
  implicit class Uniqueable[L <: HList](l: L) {
    def unique[A](implicit ev: FilterAux[L, A, A :: HNil]) = ev(l).head
  }

  // A simple type hierarchy for demonstration purposes.
  class Foo
  class Bar extends Foo

  // Some example instances.
  val foo = new Foo
  val bar = new Bar

  // An example `HList`.
  val stuff = (1 :: bar :: foo :: "a" :: HNil)

  /**
   * Because `filter` does not select subtypes of the query type, both `Foo`
   * and `Bar` occur uniquely in the list, and we can write the following:
   */
  val uniqueFoo = stuff.unique[Foo]
  val uniqueBar = stuff.unique[Bar]

  assert(foo == uniqueFoo)
  assert(bar == uniqueBar)

  /**
   * If we wanted to confirm that the list uniquely contains `Foo` or any
   * subtype of `Foo`, we could first use `unifySubtypes` to upcast any
   * subtypes of `Foo` in the list to `Foo`.
   *
   * The following would not compile, for example:
   */
  //stuff.unifySubtypes[Foo].unique[Foo]
}
