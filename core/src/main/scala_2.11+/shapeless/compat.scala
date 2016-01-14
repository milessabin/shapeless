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

  def apply(c1: whitebox.Context): Aux[c1.type] =
    new DerivationContext { val c: c1.type = c1 }

  def establish(dc: DerivationContext, c: whitebox.Context): Aux[c.type] =
    dc.asInstanceOf[Aux[c.type]]
}

trait MacroCompatLite {
  val c: blackbox.Context
}

trait TypeableMacrosMixin {
  val c: blackbox.Context
  import c.universe._

  implicit class TypeWrapperForExistentialType(tpe: Type) {
    def dealiasForExistentialType: Type = tpe.dealias
    def typeArgsForExistentialType: List[Type] = tpe.typeArgs
  }
}
