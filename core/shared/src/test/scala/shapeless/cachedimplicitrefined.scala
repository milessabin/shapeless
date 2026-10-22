/*
 * Copyright (c) 2026 Miles Sabin
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

/**
 * An implicit val whose refined type depends on a val initialized by `cachedImplicit` is found by a search made
 * before the definitions alongside it are typed.
 */
class CachedImplicitRefinedTests {
  // Searched for before the definitions in Quux3 are typed
  object Quux3Use {
    val gen = Generic[Quux3]
  }

  case class Quux3(i: Int, s: String)
  object Quux3 {
    val gen0 = cachedImplicit[Generic[Quux3]]
    implicit val gen: Generic.Aux[Quux3, gen0.Repr] = gen0
  }

  @Test
  def testRefinedUsedEarlier: Unit = {
    assert(Quux3Use.gen eq Quux3.gen0)
    assert(Generic[Quux3] eq Quux3.gen0)
  }
}
