/*
 * Copyright (c) 2016 Jan Bessai, Miles Sabin
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

package shapeless.examples
import shapeless._

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

object LiftableExample {

  sealed trait Exp
  case class Plus(x: Exp, y: Exp) extends Exp
  case class IntConst(x: Int) extends Exp
  case class DoubleConst(x: Double) extends Exp


  val test1 = Plus(IntConst(21), IntConst(21))
  val test2 = Plus(DoubleConst(21.1), DoubleConst(21.1))

  def materialize: Exp = macro LiftMacro.materialize
  def materializeCustom: Exp = macro CustomLiftMacro.materialize
  def materializeIllTyped: Exp = macro ViolatingLiftMacro.materialize

  @macrocompat.bundle
  class LiftMacro(val c: whitebox.Context) extends Liftables {
    import c.universe._

    // Simple lifting
    def materialize: Tree = {
      implicit val lift = GenericLiftable[Plus]
      q"$test1"
    }
  }

  @macrocompat.bundle
  class CustomLiftMacro(val c: whitebox.Context) extends Liftables {
    val universe: c.universe.type = c.universe
    import universe._

    // Custom lifting - we will round DoubleConst and change it to an IntConst
    implicit final def liftExp(implicit gen: GenericLiftable[Exp]): GenericLiftable[Exp] =
      new GenericLiftable[Exp] {
        val liftable =
          new Liftable[Exp] {
            def apply(x: Exp) = x match {
              case DoubleConst(x) => q"_root_.shapeless.examples.LiftableExample.IntConst(${x.round.toInt})"
              case other => gen.liftable(other)
            }
        }
      }

    def materialize: Tree = {
      implicit val lift = GenericLiftable[Plus]
      q"$test2"
    }
  }

  @macrocompat.bundle
  class ViolatingLiftMacro(val c: whitebox.Context) extends Liftables {
    val universe: c.universe.type = c.universe
    import universe._

    // Generic quotation will ensure types do not change,
    // so this will cause a compile time type error
    implicit val liftDoubleConst: GenericLiftable[DoubleConst] =
      new GenericLiftable[DoubleConst] {
        val liftable =
          new Liftable[DoubleConst] {
            def apply(x: DoubleConst) =
              x match {
                case DoubleConst(x) => q"_root_.shapeless.examples.LiftableExample.IntConst(${x.round.toInt})"
              }
        }
      }

    def materialize: Tree = {
      implicit val lift = GenericLiftable[Plus]
      q"$test2"
    }
  }

}
