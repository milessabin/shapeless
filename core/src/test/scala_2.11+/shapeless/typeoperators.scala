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

import test._

class TypeOperator211Tests {

  trait Bar[T] {
    type U
    val tu: Either[T, U]
  }

  object Bar {
    implicit def mkBar1: Bar[Boolean] { type U = Int } = new Bar[Boolean] { type U = Int ; val tu = Right(23) }
    implicit def mkBar2: Bar[String] { type U = Double } = new Bar[String] { type U = Double ; val tu = Right(13.0) }
  }

  @Test
  def testTheQuantifiers: Unit = {
    def bar0[T, U0](implicit b: Bar[T] { type U = U0 }): Bar[T] { type U = U0 } = {
      val res = the[Bar[T]]
      res
    }

    def bar1[T, U0](implicit b: Bar[T] { type U = U0 }): Option[b.U] = {
      val res: Option[the.`Bar[T]`.U] = None
      res
    }

    val b0 = bar0[Boolean, Int]
    typed[Bar[Boolean] { type U = Int }](b0)

    val b1 = bar1[Boolean, Int]
    typed[Option[Int]](b1)
  }

}
