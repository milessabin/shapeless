/*
 * Copyright (c) 2011-15 Miles Sabin
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
import scala.reflect.macros.Context

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

  def apply(i: Int): Nat = macro NatMacros.materializeWidened

  /** The natural number 0 */
  type _0 = shapeless._0
  val _0: _0 = new _0

  def toInt[N <: Nat](implicit toIntN : ToInt[N]) = toIntN()

  def toInt(n : Nat)(implicit toIntN : ToInt[n.N]) = toIntN()

  implicit def materialize(i: Int): Nat = macro NatMacros.materializeSingleton

  implicit def natOps[N <: Nat](n : N) : NatOps[N] = new NatOps(n)
}

object NatMacros {
  def mkNatTpt(c: Context)(i: c.Expr[Int]): c.Tree = {
    import c.universe._

    val n = i.tree match {
      case Literal(Constant(n: Int)) => n
      case _ =>
        c.abort(c.enclosingPosition, s"Expression ${i.tree} does not evaluate to an Int constant")
    }

    if (n < 0)
      c.abort(c.enclosingPosition, s"A Nat cannot represent $n")

    val succSym = typeOf[Succ[_]].typeConstructor.typeSymbol
    val _0Sym = typeOf[_0].typeSymbol

    @tailrec
    def mkNatTpt(n: Int, acc: Tree): Tree = {
      if(n == 0) acc
      else mkNatTpt(n-1, AppliedTypeTree(Ident(succSym), List(acc)))
    }

    mkNatTpt(n, Ident(_0Sym))
  }

  def materializeSingleton(c: Context)(i: c.Expr[Int]): c.Expr[Nat] = {
    import c.universe._

    val natTpt = mkNatTpt(c)(i)
    val moduleName = newTermName(c.fresh("nat_"))

    c.Expr[Nat] {
      q"""
        object $moduleName extends $natTpt
        $moduleName
      """
    }
  }

  def materializeWidened(c: Context)(i: c.Expr[Int]): c.Expr[Nat] = {
    import c.universe._
    val natTpt = mkNatTpt(c)(i)

    c.Expr[Nat] {
      q""" new $natTpt """
    }
  }
}
