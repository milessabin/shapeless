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

    val n: Int = SingletonTypeMacros.eval[Int](c)(i.tree).getOrElse(
      c.abort(c.enclosingPosition, s"Expression ${i.tree} does not evaluate to an Int constant")
    )

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

    val moduleName = TermName(c.freshName("nat_"))
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

    val valName = TermName(c.freshName("nat_"))
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
trait Pred[A <: Nat] {
  type Out <: Nat
}

trait PredAux[A <: Nat, B <: Nat]

object Pred {
  implicit def pred[A <: Nat, B <: Nat](implicit pred : PredAux[A, B]) = new Pred[A] {
    type Out = B
  }
}

object PredAux {
  implicit def pred[B <: Nat] = new PredAux[Succ[B], B] {}
}

/**
 * Type class witnessing that `C` is the sum of `A` and `B`.
 * 
 * @author Miles Sabin
 */
trait Sum[A <: Nat, B <: Nat] {
  type Out <: Nat
}

trait SumAux[A <: Nat, B <: Nat, C <: Nat]

object Sum {
  implicit def sum[A <: Nat, B <: Nat, C <: Nat](implicit sum: SumAux[A, B, C]) = new Sum[A, B] {
    type Out = C
  }
}

object SumAux {
  implicit def sum1[B <: Nat] = new SumAux[_0, B, B] {}
  implicit def sum2[A <: Nat, B <: Nat, C <: Nat]
    (implicit ev : SumAux[A, Succ[B], C]) = new SumAux[Succ[A], B, C] {}
}

/**
 * Type class witnessing that `C` is the difference of `A` and `B`.
 * 
 * @author Miles Sabin
 */
trait Diff[A <: Nat, B <: Nat] {
  type Out <: Nat
}

trait DiffAux[A <: Nat, B <: Nat, C <: Nat]

object Diff {
  implicit def diff[A <: Nat, B <: Nat, C <: Nat](implicit diff: DiffAux[A, B, C]) = new Diff[A, B] {
    type Out = C
  }
}

object DiffAux {
  implicit def diff1[A <: Nat] = new DiffAux[A, _0, A] {}
  implicit def diff2[A <: Nat, B <: Nat, C <: Nat]
    (implicit ev : DiffAux[A, B, C]) = new DiffAux[Succ[A], Succ[B], C] {}
}

/**
 * Type class witnessing that `C` is the product of `A` and `B`.
 * 
 * @author Miles Sabin
 */
trait Prod[A <: Nat, B <: Nat] {
  type Out <: Nat
}

trait ProdAux[A <: Nat, B <: Nat, C <: Nat]

object Prod {
  implicit def prod[A <: Nat, B <: Nat, C <: Nat](implicit prod: ProdAux[A, B, C]) = new Prod[A, B] {
    type Out = C
  }
}

object ProdAux {
  implicit def prod1[B <: Nat] = new ProdAux[_0, B, _0] {}
  implicit def prod2[A <: Nat, B <: Nat, C <: Nat, D <: Nat]
    (implicit ev1 : ProdAux[A, B, C], ev2 : SumAux[B, C, D]) = new ProdAux[Succ[A], B, D] {}
}

/**
 * Type class witnessing that `Out` is the quotient of `A` and `B`.
 *
 * @author Tom Switzer
 */
trait Div[A <: Nat, B <: Nat] {
  type Out <: Nat
}

trait DivAux[A <: Nat, B <: Nat, C <: Nat]

object Div {
  implicit def div[A <: Nat, B <: Nat, C <: Nat]
    (implicit div: DivAux[A, B, C]) = new Div[A, B] {
      type Out = C
    }
}

object DivAux {
  import LT._

  implicit def div1[A <: Nat] = new DivAux[_0, A, _0] {}

  implicit def div2[A <: Nat, B <: Nat](implicit e: A < B) =
    new DivAux[A, B, _0] {}

  implicit def div3[A <: Nat, B <: Nat, C <: Nat, D <: Nat]
    (implicit diff: DiffAux[Succ[A], B, C], div: DivAux[C, B, D]) =
      new DivAux[Succ[A], B, Succ[D]] {}
}

/**
 * Typeclass witnessing that `Out` is `A` mod `B`.
 *
 * @author Tom Switzer
 */
trait Mod[A <: Nat, B <: Nat] {
  type Out <: Nat
}

trait ModAux[A <: Nat, B <: Nat, C <: Nat]

object Mod {
  implicit def mod[A <: Nat, B <: Nat, C <: Nat]
    (implicit mod: ModAux[A, B, C]) = new Mod[A, B] { type Out = C }
}

object ModAux {
  implicit def modAux[A <: Nat, B <: Nat, C <: Nat, D <: Nat, E <: Nat]
    (implicit div: DivAux[A, B, C], prod: ProdAux[C, B, D], diff: DiffAux[A, D, E]) =
      new ModAux[A, B, E] {}
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
trait Min[A <: Nat, B <: Nat] {
  type Out <: Nat
}

trait MinAux[A <: Nat, B <: Nat, C <: Nat]

object Min {
  implicit def min[A <: Nat, B <: Nat, C <: Nat]
    (implicit aux: MinAux[A, B, C]) = new Min[A, B] { type Out = C }
}

object MinAux {
  implicit def minAux0[A <: Nat, B <: Nat, C <: Nat]
    (implicit lteq: LTEq[A, B]) = new MinAux[A, B, A] {}
  implicit def minAux1[A <: Nat, B <: Nat, C <: Nat]
    (implicit lteq: LT[B, A]) = new MinAux[A, B, B] {}
}


/**
 * Type class witnessing that `Out` is `X` raised to the power `N`.
 *
 * @author George Leontiev
 */
trait Pow[N <: Nat, X <: Nat] {
  type Out <: Nat
}

trait PowAux[N <: Nat, X <: Nat, Z <: Nat]

object Pow {
  implicit def pow[N <: Nat, X <: Nat, Z <: Nat](implicit pow : PowAux[N, X, Z]) = new Pow[N, X] {
    type Out = Z
  }
}

object PowAux {
  import nat.{_0, _1}
  implicit def pow1[A <: Nat] = new PowAux[Succ[A], _0, _0] {}
  implicit def pow2[A <: Nat] = new PowAux[_0, Succ[A], _1] {}
  implicit def pow3[N <: Nat, X <: Nat, Z <: Nat, Y <: Nat]
    (implicit ev : PowAux[N, X, Z], ev2 : ProdAux[Z, X, Y]) = new PowAux[Succ[N], X, Y] {}
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
