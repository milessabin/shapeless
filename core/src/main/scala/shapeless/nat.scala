/*
 * Copyright (c) 2011-13 Miles Sabin 
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

object Succ {
  implicit def witnessN[P <: Nat]: Witness.Aux[Succ[P]] =
    new Witness.Aux[Succ[P]] {
      val value = new Succ[P]()
    }
}

/**
 * Encoding of zero.
 * 
 * @author Miles Sabin
 */
class _0 extends Nat {
  type N = _0
}

/**
 * Type level encoding of the natural numbers.
 * 
 * @author Miles Sabin
 */
object Nat extends Nats {
  def apply(i: Int) = macro NatMacros.materializeWidened

  implicit def materialize(i: Int) = macro NatMacros.materializeSingleton
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

    def mkNatTpt(n: Int): Tree = {
      if(n == 0) Ident(_0Sym)
      else AppliedTypeTree(Ident(succSym), List(mkNatTpt(n-1)))
    }

    mkNatTpt(n)
  }

  def materializeSingleton(c: Context)(i: c.Expr[Int]): c.Expr[Nat] = {
    import c.universe._

    val natTpt = mkNatTpt(c)(i)

    val pendingSuperCall = Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())

    val moduleName = newTermName(c.fresh("nat_"))
    val moduleDef =
      ModuleDef(Modifiers(), moduleName,
        Template(
          List(natTpt),
          emptyValDef,
          List(
            DefDef(
              Modifiers(), nme.CONSTRUCTOR, List(),
              List(List()),
              TypeTree(),
              Block(List(pendingSuperCall), Literal(Constant(()))))
          )
        )
      )

    c.Expr[Nat] {
      Block(
        List(moduleDef),
        Ident(moduleName)
      )
    }
  }

  def materializeWidened(c: Context)(i: c.Expr[Int]): c.Expr[Nat] = {
    import c.universe._
    val natTpt = mkNatTpt(c)(i)

    val valName = newTermName(c.fresh("nat_"))
    val valDef =
      ValDef(Modifiers(), valName,
        natTpt,
        Apply(Select(New(natTpt), nme.CONSTRUCTOR), List())
      )

    c.Expr[Nat] {
      Block(
        List(valDef),
        Ident(valName)
      )
    }
  }
}

/**
 * Type class witnessing that `B` is the predecessor of `A`.
 * 
 * @author Miles Sabin
 */
trait Pred[A <: Nat] { type Out <: Nat }

object Pred {
  type Aux[A <: Nat, B <: Nat] = Pred[A] { type Out = B }

  implicit def pred[B <: Nat]: Aux[Succ[B], B] = new Pred[Succ[B]] { type Out = B }
}

/**
 * Type class witnessing that `C` is the sum of `A` and `B`.
 * 
 * @author Miles Sabin
 */
trait Sum[A <: Nat, B <: Nat] { type Out <: Nat }

object Sum {
  type Aux[A <: Nat, B <: Nat, C <: Nat] = Sum[A, B] { type Out = C }

  implicit def sum1[B <: Nat]: Aux[_0, B, B] = new Sum[_0, B] { type Out = B }
  implicit def sum2[A <: Nat, B <: Nat]
    (implicit sum : Sum[A, Succ[B]]): Aux[Succ[A], B, sum.Out] = new Sum[Succ[A], B] { type Out = sum.Out }
}

/**
 * Type class witnessing that `C` is the difference of `A` and `B`.
 * 
 * @author Miles Sabin
 */
trait Diff[A <: Nat, B <: Nat] { type Out <: Nat }

object Diff {
  type Aux[A <: Nat, B <: Nat, C <: Nat] = Diff[A, B] { type Out = C }

  implicit def diff1[A <: Nat]: Aux[A, _0, A] = new Diff[A, _0] { type Out = A }
  implicit def diff2[A <: Nat, B <: Nat]
    (implicit diff : Diff[A, B]): Aux[Succ[A], Succ[B], diff.Out] = new Diff[Succ[A], Succ[B]] { type Out = diff.Out }
}

/**
 * Type class witnessing that `C` is the product of `A` and `B`.
 * 
 * @author Miles Sabin
 */
trait Prod[A <: Nat, B <: Nat] { type Out <: Nat }

object Prod {
  type Aux[A <: Nat, B <: Nat, C <: Nat] = Prod[A, B] { type Out = C }

  implicit def prod1[B <: Nat]: Aux[_0, B, _0] = new Prod[_0, B] { type Out = _0 }
  implicit def prod2[A <: Nat, B <: Nat, C <: Nat]
    (implicit prod: Prod.Aux[A, B, C], sum: Sum[B, C]): Aux[Succ[A], B, sum.Out] = new Prod[Succ[A], B] { type Out = sum.Out }
}

/**
 * Type class witnessing that `Out` is the quotient of `A` and `B`.
 *
 * @author Tom Switzer
 */
trait Div[A <: Nat, B <: Nat] { type Out <: Nat }

object Div {
  import LT._

  type Aux[A <: Nat, B <: Nat, C <: Nat] = Div[A, B] { type Out = C }

  implicit def div1[A <: Nat]: Aux[_0, A, _0] = new Div[_0, A] { type Out = _0 }

  implicit def div2[A <: Nat, B <: Nat](implicit lt: A < B): Aux[A, B, _0] =
    new Div[A, B] { type Out = _0 }

  implicit def div3[A <: Nat, B <: Nat, C <: Nat, D <: Nat]
    (implicit diff: Diff.Aux[Succ[A], B, C], div: Div.Aux[C, B, D]): Aux[Succ[A], B, Succ[D]] =
      new Div[Succ[A], B] { type Out = Succ[D] }
}

/**
 * Typeclass witnessing that `Out` is `A` mod `B`.
 *
 * @author Tom Switzer
 */
trait Mod[A <: Nat, B <: Nat] { type Out <: Nat }

object Mod {
  type Aux[A <: Nat, B <: Nat, C <: Nat] = Mod[A, B] { type Out = C }

  implicit def modAux[A <: Nat, B <: Nat, C <: Nat, D <: Nat, E <: Nat]
    (implicit div: Div.Aux[A, B, C], prod: Prod.Aux[C, B, D], diff: Diff.Aux[A, D, E]): Aux[A, B, E] =
      new Mod[A, B] { type Out = E }
}

/**
 * Type class witnessing that `A` is less than `B`.
 * 
 * @author Miles Sabin
 */
trait LT[A <: Nat, B <: Nat]

object LT {
  type <[A <: Nat, B <: Nat] = LT[A, B]

  implicit def lt1[B <: Nat] = new <[_0, Succ[B]] {}
  implicit def lt2[A <: Nat, B <: Nat](implicit lt : A < B) = new <[Succ[A], Succ[B]] {}
}

/**
 * Type class witnessing that `A` is less than or equal to `B`.
 * 
 * @author Miles Sabin
 */
trait LTEq[A <: Nat, B <: Nat]

object LTEq {
  type <=[A <: Nat, B <: Nat] = LTEq[A, B]

  implicit def ltEq1 = new <=[_0, _0] {}
  implicit def ltEq2[B <: Nat] = new <=[_0, Succ[B]] {}
  implicit def ltEq3[A <: Nat, B <: Nat](implicit lteq : A <= B) = new <=[Succ[A], Succ[B]] {}
}

/**
 * Type class witnessing that `Out` is `A` min `B`.
 *
 * @author George Leontiev
 */
trait Min[A <: Nat, B <: Nat] { type Out <: Nat }

object Min {
  type Aux[A <: Nat, B <: Nat, C <: Nat] = Min[A, B] { type Out = C }

  implicit def minAux0[A <: Nat, B <: Nat, C <: Nat]
    (implicit lteq: LTEq[A, B]): Aux[A, B, A] = new Min[A, B] { type Out = A }
  implicit def minAux1[A <: Nat, B <: Nat, C <: Nat]
    (implicit lteq: LT[B, A]): Aux[A, B, B] = new Min[A, B] { type Out = B }
}

/**
 * Type class witnessing that `Out` is `X` raised to the power `N`.
 *
 * @author George Leontiev
 */
trait Pow[N <: Nat, X <: Nat] { type Out <: Nat }

object Pow {
  import nat._1

  type Aux[N <: Nat, X <: Nat, Z <: Nat] = Pow[N, X] { type Out = Z }

  implicit def pow1[A <: Nat]: Aux[Succ[A], _0, _0] = new Pow[Succ[A], _0] { type Out = _0 }
  implicit def pow2[A <: Nat]: Aux[_0, Succ[A], _1] = new Pow[_0, Succ[A]] { type Out = _1 }
  implicit def pow3[N <: Nat, X <: Nat, Z <: Nat, Y <: Nat]
    (implicit ev : Pow.Aux[N, X, Z], ev2 : Prod.Aux[Z, X, Y]): Aux[Succ[N], X, Y] = new Pow[Succ[N], X] { type Out = Y }
}

/**
 * Type class supporting conversion of type-level Nats to value level Ints.
 * 
 * @author Miles Sabin
 */
trait ToInt[N <: Nat] {
  def apply() : Int
}

object ToInt {
  implicit val toInt0 = new ToInt[_0] {
    def apply() = 0 
  }
  implicit def toIntSucc[N <: Nat](implicit toIntN : ToInt[N]) = new ToInt[Succ[N]] {
    def apply() = toIntN()+1
  }
}
