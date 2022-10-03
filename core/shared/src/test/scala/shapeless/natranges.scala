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

import org.junit.Assert._
import org.junit.Test

class natranges {

  import nat._
  import shapeless.testutil._

  //
  // Nil ranges: "X to same X"

  @Test
  def smallest_closed_range_symbolic_syntax = {
    import syntax.nat._
    assertTypedEquals[
      _1 :: HNil
    ](
      _1 :: HNil,
      the[_1 *--* _1].apply()
    )
  }

  @Test
  def smallest_closed_range_word_syntax = {
    import ops.nat.BoundedRange
    import BoundedRange.{Inclusive, Exclusive}
    assertTypedEquals[
      _1 :: HNil
    ](
      _1 :: HNil,
      the[BoundedRange[Inclusive[_1], Inclusive[_1]]].apply()
    )
  }

  @Test
  def smallest_open_range_symbolic_syntax = {
    import syntax.nat._
    assertTypedEquals[
      HNil
    ](
      HNil,
      the[_1 :--: _1].apply()
    )
  }

    @Test
  def smallest_open_range_word_syntax = {
    import ops.nat.BoundedRange
    import BoundedRange.{Inclusive, Exclusive}
    assertTypedEquals[
      HNil
    ](
      HNil,
      the[BoundedRange[Exclusive[_1], Exclusive[_1]]].apply()
    )
  }

  @Test
  def smallest_leftOpenRightClosed_range_symbolic_syntax = {
    import syntax.nat._
    assertTypedEquals[
      _1 :: HNil
    ](
      _1 :: HNil,
      the[_1 :--* _1].apply()
    )
  }

  @Test
  def smallest_leftOpenRightClosed_range_word_syntax = {
    import ops.nat.BoundedRange
    import BoundedRange.{Inclusive, Exclusive}
    assertTypedEquals[
      _1 :: HNil
    ](
      _1 :: HNil,
      the[BoundedRange[Exclusive[_1], Inclusive[_1]]].apply()
    )
  }

  @Test
  def smallest_leftClosedRightOpen_range_symbolic_syntax = {
    import syntax.nat._
    assertTypedEquals[
      _1 :: HNil
    ](
      _1 :: HNil,
      the[_1 *--: _1].apply()
    )
  }

  @Test
  def smallest_leftClosedRightOpen_range_word_syntax = {
    import ops.nat.BoundedRange
    import BoundedRange.{Inclusive, Exclusive}
    assertTypedEquals[
      _1 :: HNil
    ](
      _1 :: HNil,
      the[BoundedRange[Inclusive[_1], Exclusive[_1]]].apply()
    )
  }

  //
  // regular ranges: "X to Y"

  @Test
  def larger_closed_range_symbolic_syntax = {
    import syntax.nat._
    assertTypedEquals[
      _1 :: _2 :: _3 :: _4 :: HNil
    ](
      _1 :: _2 :: _3 :: _4 :: HNil,
      the[_1 *--* _4].apply()
    )
  }

  @Test
  def larger_closed_range_word_syntax = {
    import ops.nat.BoundedRange
    import BoundedRange.{Inclusive, Exclusive}
    assertTypedEquals[
      _1 :: _2 :: _3 :: _4 :: HNil
    ](
      _1 :: _2 :: _3 :: _4 :: HNil,
      the[BoundedRange[Inclusive[_1], Inclusive[_4]]].apply()
    )
  }

  @Test
  def larger_open_range_symbolic_syntax = {
    import syntax.nat._
    assertTypedEquals[
      _2 :: _3 :: HNil
    ](
      _2 :: _3 :: HNil,
      the[_1 :--: _4].apply()
    )
  }

  @Test
  def larger_open_range_word_syntax = {
    import ops.nat.BoundedRange
    import BoundedRange.{Inclusive, Exclusive}
    assertTypedEquals[
      _2 :: _3 :: HNil
    ](
      _2 :: _3 :: HNil,
      the[BoundedRange[Exclusive[_1], Exclusive[_4]]].apply()
    )
  }

  @Test
  def larger_leftClosedRightOpen_range_symbolic_syntax = {
    import syntax.nat._
    assertTypedEquals[
      _1 :: _2 :: _3 :: HNil
    ](
      _1 :: _2 :: _3 :: HNil,
      the[_1 *--: _4].apply()
    )
  }

  @Test
  def larger_leftClosedRightOpen_range_word_syntax = {
    import ops.nat.BoundedRange
    import BoundedRange.{Inclusive, Exclusive}
    assertTypedEquals[
      _1 :: _2 :: _3 :: HNil
    ](
      _1 :: _2 :: _3 :: HNil,
      the[BoundedRange[Inclusive[_1], Exclusive[_4]]].apply()
    )
  }

  @Test
  def larger_leftOpenRightClosed_range_symbolic_syntax = {
    import syntax.nat._
    assertTypedEquals[
      _2 :: _3 :: _4 :: HNil
    ](
      _2 :: _3 :: _4 :: HNil,
      the[_1 :--* _4].apply()
    )
  }

  @Test
  def larger_leftOpenRightClosed_range_word_syntax = {
    import ops.nat.BoundedRange
    import BoundedRange.{Inclusive, Exclusive}
    assertTypedEquals[
      _2 :: _3 :: _4 :: HNil
    ](
      _2 :: _3 :: _4 :: HNil,
      the[BoundedRange[Exclusive[_1], Inclusive[_4]]].apply()
    )
  }

  //
  // Reversed ranges: "X down to Y"

  @Test
  def reversed_closed_range_symbolic_syntax = {
    import syntax.nat._
    assertTypedEquals[
      _4 :: _3 :: _2 :: _1 :: HNil
    ](
      _4 :: _3 :: _2 :: _1 :: HNil,
      the[_4 *--* _1].apply()
    )
  }

  @Test
  def reversed_closed_range_word_syntax = {
    import ops.nat.BoundedRange
    import BoundedRange.{Inclusive, Exclusive}
    assertTypedEquals[
      _4 :: _3 :: _2 :: _1 :: HNil
    ](
      _4 :: _3 :: _2 :: _1 :: HNil,
      the[BoundedRange[Inclusive[_4], Inclusive[_1]]].apply()
    )
  }

  @Test
  def reversed_open_range_symbolic_syntax = {
    import syntax.nat._
    assertTypedEquals[
      _3 :: _2 :: HNil
    ](
      _3 :: _2 :: HNil,
      the[_4 :--: _1].apply()
    )
  }

  @Test
  def reversed_open_range_word_syntax = {
    import ops.nat.BoundedRange
    import BoundedRange.{Inclusive, Exclusive}
    assertTypedEquals[
      _3 :: _2 :: HNil
    ](
      _3 :: _2 :: HNil,
      the[BoundedRange[Exclusive[_4], Exclusive[_1]]].apply()
    )
  }

  @Test
  def reversed_leftClosedRightOpen_range_symbolic_syntax = {
    import syntax.nat._
    assertTypedEquals[
      _4 :: _3 :: _2 :: HNil
    ](
      _4 :: _3 :: _2 :: HNil,
      the[_4 *--: _1].apply()
    )
  }

  @Test
  def reversed_leftClosedRightOpen_range_word_syntax = {
    import ops.nat.BoundedRange
    import BoundedRange.{Inclusive, Exclusive}
    assertTypedEquals[
      _4 :: _3 :: _2 :: HNil
    ](
      _4 :: _3 :: _2 :: HNil,
      the[BoundedRange[Inclusive[_4], Exclusive[_1]]].apply()
    )
  }

  @Test
  def reversed_leftOpenRightClosed_range_symbolic_syntax = {
    import syntax.nat._
    assertTypedEquals[
      _3 :: _2 :: _1 :: HNil
    ](
      _3 :: _2 :: _1 :: HNil,
      the[_4 :--* _1].apply()
    )
  }

  @Test
  def reversed_leftOpenRightClosed_range_word_syntax = {
    import ops.nat.BoundedRange
    import BoundedRange.{Inclusive, Exclusive}
    assertTypedEquals[
      _3 :: _2 :: _1 :: HNil
    ](
      _3 :: _2 :: _1 :: HNil,
      the[BoundedRange[Exclusive[_4], Inclusive[_1]]].apply()
    )
  }

}
