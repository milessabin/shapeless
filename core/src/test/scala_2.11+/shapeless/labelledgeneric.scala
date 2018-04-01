/*
 * Copyright (c) 2016 Miles Sabin
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

import record._

object ShapelessTaggedAux {
  import tag.@@

  trait CustomTag
  case class Dummy(i: Int @@ CustomTag)
}

class LabelledGeneric211Tests {

  @Test
  def testShapelessTagged: Unit = {
    import ShapelessTaggedAux._

    val lgen = LabelledGeneric[Dummy]
    val s = s"${lgen from Record(i=tag[CustomTag](0))}"
    assertEquals(s, "Dummy(0)")
  }

  @Test
  def testScalazTagged: Unit = {
    import ScalazTaggedAux._

    type R = Record.`'i -> Int @@ CustomTag`.T
    val lgen = LabelledGeneric[Dummy]
    implicitly[lgen.Repr =:= R]
    implicitly[TC[R]]

    type RT = Record.`'b -> Boolean, 'i -> Int @@ CustomTag`.T
    val lgent = LabelledGeneric[DummyTagged]
    implicitly[lgent.Repr =:= RT]
    implicitly[TC[RT]]
  }

}
