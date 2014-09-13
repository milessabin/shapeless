/*
 * Copyright (c) 2011-14 Miles Sabin
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
 * Base trait for type level real numbers. 
 * 
 * @author Olivier Mélois
 */
trait Number {
  type N <: Number 
}

/**
 * Base trait for type level rational integers (commonly called integers).
 *
 * @author Olivier Mélois
 */
trait RInt extends Number {
  type N <: RInt 
}

/**
 * Trait for type level negative or zero integers.
 * @author Olivier Mélois 
 *
 */
trait Neg extends RInt {
  type N <: Neg
}

/**
 * Base trait for type level natural numbers.
 *
 * @author Miles Sabin
 */
trait Nat extends RInt{
  type N <: Nat
}

/**
 * Trait for type level strictly negative integers.
 * @author Olivier Mélois 
 *
 */
trait SNeg extends Neg {
  type N <: SNeg
}

/**
 * Trait for type level strictly positive integers.
 * @author Olivier Mélois 
 *
 */
trait SPos extends Nat {
  type N <: SPos
}


/**
 * Encoding of successor.
 *
 * @author Miles Sabin
 */
case class Succ[P <: Nat]() extends SPos {
  type N = Succ[P]
}

/**
 * Encoding of opposite of strictly positive integer.
 *
 *  @author Olivier Mélois
 */
case class Minus[P <: SPos]() extends Neg {
    type N = Minus[P]
}

/**
 * Encoding of zero.
 *
 * @author Miles Sabin
 */
class _0 extends Nat with Neg {
  type N = _0
}

/**
 * Type level encoding of the natural numbers.
 *
 * @author Miles Sabin
 */
object Nat extends Nats {
  import ops.nat._

  def apply(i: Int): Nat = macro NatMacros.materializeWidened

  /** The natural number 0 */
  type _0 = shapeless._0
  val _0: _0 = new _0

  def toInt[N <: Nat](implicit toIntN : ToInt[N]) = toIntN()

  def toInt(n : Nat)(implicit toIntN : ToInt[n.N]) = toIntN()

  implicit def materialize(i: Int): Nat = macro NatMacros.materializeSingleton
}

/**
 * Type level encoding of the rational integers.
 *
 * @author Miles Sabin
 */
object RInt extends RInts {
  import ops.numbers._

  def apply(i: Int): RInt = macro IntMacros.materializeWidened

  /** The natural number 0 */
  type _0 = shapeless._0
  val _0: _0 = new _0

  def toInt[N <: RInt](implicit toIntN : ToInt[N]) = toIntN()

  def toInt(n : RInt)(implicit toIntN : ToInt[n.N]) = toIntN()

  implicit def materialize(i: Int): RInt = macro IntMacros.materializeSingleton
}


object IntMacros {

  def asInt(c: whitebox.Context)(i: c.Expr[Int]) : Int = {
    import c.universe._

    i.tree match {
      case Literal(Constant(n: Int)) => n
      case _ =>
        c.abort(c.enclosingPosition, s"Expression ${i.tree} does not evaluate to an Int constant")
    }
  }

  def mkIntTptAux(c : whitebox.Context)(n : => Int) : c.Tree = {
    import c.universe._

    val oppSym = typeOf[Minus[_]].typeConstructor.typeSymbol
    val succSym = typeOf[Succ[_]].typeConstructor.typeSymbol
    val _0Sym = typeOf[_0].typeSymbol

    @tailrec
    def mkIntTptAux(n: Int, acc: Tree): Tree = {
      if(n == 0) acc
      else mkIntTptAux(n-1, AppliedTypeTree(Ident(succSym), List(acc)))
    }

    if (n >= 0) mkIntTptAux(n, Ident(_0Sym)) else AppliedTypeTree(Ident(oppSym), List(mkIntTptAux(-n, Ident(_0Sym))))
  } 

  def mkIntTpt(c : whitebox.Context)(i: c.Expr[Int]) : c.Tree = mkIntTptAux(c)(asInt(c)(i))


  def materializeSingletonAux(c: whitebox.Context)(i: c.Expr[Int])(intTpt : => c.Tree) : c.Tree = {
    import c.universe._

    val moduleName = TermName(c.freshName("nat_"))

    q"""
      object $moduleName extends $intTpt
      $moduleName
    """
  }

  def materializeSingleton(c : whitebox.Context)(i: c.Expr[Int]) : c.Tree = materializeSingletonAux(c)(i)(mkIntTpt(c)(i))

  def materializeWidenedAux(c: whitebox.Context)(i: c.Expr[Int])(intTpt : => c.Tree): c.Tree = {import c.universe._ ; q""" new $intTpt """}

  def materializeWidened(c: whitebox.Context)(i: c.Expr[Int]) : c.Tree =  materializeWidenedAux(c)(i)(mkIntTpt(c)(i))

}

object NatMacros {
  import IntMacros._ 

  def mkNatTpt(c: whitebox.Context)(i: c.Expr[Int]): c.Tree = mkIntTptAux(c){
     val n = asInt(c)(i)
     if (n < 0)
      c.abort(c.enclosingPosition, s"A Nat cannot represent $n")
     n 
  }

  def materializeSingleton(c: whitebox.Context)(i: c.Expr[Int]): c.Tree = materializeSingletonAux(c)(i)(mkNatTpt(c)(i))

  def materializeWidened(c: whitebox.Context)(i: c.Expr[Int]): c.Tree = materializeWidenedAux(c)(i)(mkNatTpt(c)(i))
}
