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

import shapeless.test._

class SingletonTypes211Tests {

  class NestingBug {
    val o: AnyRef = new Object {}

    val wO = {
      final class W extends _root_.shapeless.Witness {
        type T = o.type
        val value: T = o
      }
      new W
    }

    // This fails on 2.10.x unless W is moved out of the block
    // on the RHS of wO
    val x1: o.type = wO.value
  }

  def testSingletonWitness: Unit = {
    trait Bound
    object Foo extends Bound
    val bar = "bar"
    val wFoo = Witness(Foo)
    val wBar = Witness(bar)

    typed[Foo.type](wFoo.value)
    typed[bar.type](wBar.value)
  }

  class PathDependentSingleton1 {
    val o: AnyRef = new Object {}
    val wO = Witness(o)
    type OT = wO.T
    implicitly[OT =:= o.type]

    val x0: OT = wO.value
    val x1: o.type = wO.value

    val x2 = wO.value
    typed[o.type](x2)
    typed[OT](x2)
  }

  object PathDependentSingleton2 {
    val o: AnyRef = new Object {}
    val wO = Witness(o)
    type OT = wO.T
    implicitly[OT =:= o.type]

    val x0: OT = wO.value
    val x1: o.type = wO.value

    val x2 = wO.value
    typed[o.type](x2)
    typed[OT](x2)
  }

}
