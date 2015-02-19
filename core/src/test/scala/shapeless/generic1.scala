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

import testutil.assertTypedEquals

package Generic1TestsAux {
  trait Box[T]

  case class Foo[T](t: T)
  case class Bar[T](t: Box[T])
  case class Baz[T](t: T, s: String)

  sealed trait Cp[+T]
  case class CpA[+T](t: T) extends Cp[T]
  case class CpB[+T](t: T) extends Cp[T]
  case object CpC extends Cp[Nothing]
  case class CpD[+T](t: T, n: Cp[T]) extends Cp[T]
}

class FunctorTests {
  import Generic1TestsAux._

  @Test
  def testBasics: Unit = {
    Generic1[Id]
    Generic1[Const[None.type]#λ]
    Generic1[Cp]
    Generic1[Some]
    Generic1[Option]
    Generic1[List]
  }
}
