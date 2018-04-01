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

class Tuple211Tests {
  import nat._
  import syntax.std.tuple._

  @Test
  def testToCoproduct: Unit = {
    import ops.tuple._

    type PISB = (Int, String, Boolean)
    type CISBa = Int :+: String :+: Boolean :+: CNil
    type CISBb = the.`ToCoproduct[PISB]`.Out
    implicitly[CISBa =:= CISBb]
  }

  @Test
  def testToSum: Unit = {
    import ops.tuple._

    type PISB = (Int, String, Boolean)
    type CISBa = Int :+: String :+: Boolean :+: CNil
    type SISBa = the.`ToSum[PISB]`.Out
    implicitly[CISBa =:= SISBa]

    type PIISSB = (Int, Int, String, String, Boolean)
    type SISBb = the.`ToSum[PIISSB]`.Out
    implicitly[CISBa =:= SISBb]
  }

  @Test
  def testGrouper: Unit = {
    object toInt extends Poly1 {
      implicit def default[N <: Nat](implicit toi: ops.nat.ToInt[N]) = at[N](_ => toi())
    }

    def range[R <: HList, T, OutL <: HList](a: Nat, b: Nat)(implicit
                                                            range: ops.nat.Range.Aux[a.N, b.N, R],
                                                            mapper: ops.hlist.Mapper.Aux[toInt.type, R, OutL],
                                                            tupler: ops.hlist.Tupler.Aux[OutL, T]
      ) = tupler(mapper(range()))

    // group Unit
    assertEquals( (), () group (2,1) )

    // partition a Tuple of 4 items into 2 (4/2) tuples of 2 items
    assertEquals(
      ((0, 1), (2, 3)),
      range(0, 4) group(2, 2)
    )

    // partition a Tuple of 5 items into 2 (5/2) tuples of 2 items
    // the last item does not make a complete partition and is dropped.
    assertEquals(
      ((0, 1), (2, 3)),
      range(0, 5) group(2, 2)
    )

    // uses the step to select the starting point for each partition
    assertEquals(
      ((0, 1), (4, 5)),
      range(0, 6) group(2, 4)
    )

    // if the step is smaller than the partition size, items will be reused
    assertEquals(
      ((0, 1), (1, 2), (2, 3)),
      range(0, 4) group(2, 1)
    )

    // when there are not enough items to fill the last partition, a pad can be supplied.
    assertEquals(
      ((0, 1), (2, 3), (4, 'a')),
      range(0, 5) group(2, 2, Tuple1('a'))
    )

    // but only as many pad elements are used as necessary to fill the final partition.
    assertEquals(
      ((0, 1), (2, 3), (4, 'a')),
      range(0, 5) group(2, 2, ('a', 'b', 'c'))
    )

  }
}
