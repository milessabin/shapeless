/*
 * Copyright (c) 2015 Miles Sabin
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

import test.illTyped

class CachedTest212 {
  import CachedTest._

  trait Foo[A]

  @Test
  def testAmbiguous {
    @annotation.implicitAmbiguous("Custom ambiguous message")
    implicit def a = new Foo[String] { }
    implicit def b = new Foo[String] { }
    illTyped(
      "cachedImplicit[Foo[String]]",
      "Custom ambiguous message"
    )
  }

}
