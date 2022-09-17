/*
 * Copyright (c) 2011-16 Miles Sabin
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

import scala.language.experimental.macros

import scala.annotation.tailrec
import scala.reflect.macros.whitebox

/**
 * Base trait for type level natural numbers.
 *
 * @author Miles Sabin
 */
trait Nat {
  type N <: Nat
}

/**
 * Encoding of successor.
 *
 * @author Miles Sabin
 */
case class Succ[P <: Nat]() extends Nat {
  type N = Succ[P]
}

/**
 * Encoding of zero.
 *
 * @author Miles Sabin
 */
class _0 extends Nat with Serializable {
  type N = _0
}

/**
 * Type level encoding of the natural numbers.
 *
 * @author Miles Sabin
 */
object Nat extends Nats {
  import ops.nat._
  import syntax.NatOps

  implicit def apply(i: Int): Nat = macro NatMacros.materializeWidened

  /** The natural number 0 */
  type _0 = shapeless._0
  val _0: _0 = new _0

  def toInt[N <: Nat](implicit toIntN : ToInt[N]) = toIntN()

  def toInt(n : Nat)(implicit toIntN : ToInt[n.N]) = toIntN()

  implicit def natOps[N <: Nat](n : N) : NatOps[N] = new NatOps(n)
}

class NatMacros(val c: whitebox.Context) extends NatMacroDefns {
  import c.universe._

  def materializeWidened(i: Tree): Tree =
    i match {
      case NatLiteral(n) => mkNatValue(n)
      case _ =>
        c.abort(c.enclosingPosition, s"Expression $i does not evaluate to a non-negative Int literal")
    }
}

trait NatMacroDefns {
  val c: whitebox.Context
  import c.universe._

  object NatLiteral {
    def unapply(i: Tree): Option[Int] =
      i match {
        case Literal(Constant(n: Int)) if n >= 0 => Some(n)
        case _ => None
      }
  }

  def mkNatTpt(i: Int): Tree = {
    val succSym = typeOf[Succ[_]].typeConstructor.typeSymbol
    val _0Sym = typeOf[_0].typeSymbol

    @tailrec
    def loop(i: Int, acc: Tree): Tree = {
      if(i == 0) acc
      else loop(i-1, AppliedTypeTree(Ident(succSym), List(acc)))
    }

    loop(i, Ident(_0Sym))
  }

  def mkNatTpe(i: Int): Type = {
    val succTpe = typeOf[Succ[_]].typeConstructor
    val _0Tpe = typeOf[_0]

    @tailrec
    def loop(i: Int, acc: Type): Type = {
      if(i == 0) acc
      else loop(i-1, appliedType(succTpe, acc))
    }

    loop(i, _0Tpe)
  }

  def mkNatValue(i: Int): Tree =
    q""" new ${mkNatTpt(i)} """
}
