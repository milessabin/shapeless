/*
 * Copyright (c) 2016 Miles Sabin, Dale Wijnand
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

import scala.reflect.macros.{ blackbox, whitebox }

trait DerivationContextCreator {
  type Aux[C] = DerivationContext { val c: C }

  def apply(c1: whitebox.Context): Aux[c1.type] = {
    val cc = c1.asInstanceOf[macrocompat.CompatContext[c1.type]]
    class DerivationContextClass(val c: macrocompat.CompatContext[c1.type]) extends DerivationContext {
      type C = c1.type
    }
    establish(new DerivationContextClass(cc), c1)
  }

  def establish(dc: DerivationContext, c: whitebox.Context): Aux[c.type] =
    dc.asInstanceOf[Aux[c.type]]
}

trait MacroCompatLite {
  val c: blackbox.Context
  import c.universe._

  implicit class TypeOps(tpe: Type) {
    def dealias: Type = tpe.normalize
  }

  object internal {
    def constantType(c: Constant): ConstantType = ConstantType(c)
  }
}
