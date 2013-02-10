/*
 * Copyright (c) 2013 Miles Sabin 
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

import scala.reflect.macros.{ Context, Macro }

class SNat[N](val value: Int) extends AnyVal {
  override def toString = "SNat[Int("+value+")]("+value+")"
}

object SNat {
  def apply(i: Int): Any = macro SNatMacros.toSNat

  implicit def intToNat[N](i: Int) : SNat[N] = macro SNatMacros.toSNat

  implicit def natToInt[N](snat: SNat[N]): Int = snat.value

  type SInt(i: Int) = macro intSingletonType

  type SBool(b: Boolean) = macro booleanSingletonType

  def eval[A](c: Context)(t: c.Tree) = c.eval(c.Expr[A](c.resetAllAttrs(t.duplicate)))

  def intSingletonType(c: Context)(i: c.Expr[Int]) = {
    import c.universe._

    TypeTree(ConstantType(Constant(
      eval[Int](c)(i.tree)
    )))
  }
  
  def booleanSingletonType(c: Context)(b: c.Expr[Boolean]) = {
    import c.universe._

    TypeTree(ConstantType(Constant(
      eval[Boolean](c)(b.tree)
    )))
  }
}

trait SNatMacros extends Macro {
  import c.universe._

  import SNat.eval

  def toSNat(i: c.Expr[Int]) = {
    val n = eval[Int](c)(i.tree)
    val N = TypeTree(ConstantType(Constant(n)))
    c.Expr(q"new SNat[$N]($n)")
  }

  override def onInfer(tic: c.TypeInferenceContext): Unit = {
    val N = tic.unknowns(0)

    tic.tree match {
      case Apply(_, List(arg)) =>
        val n = eval[Int](c)(arg)
        tic.infer(N, ConstantType(Constant(n)))

      case _ =>
    }
  }
}

trait SSum[A, B, C]

object SSum {
  implicit def mkSsum[A, B, C] = macro SSumMacros.witness[A, B, C]

  def ssum[A, B, C](a: SNat[A], b: SNat[B])(implicit ssum: SSum[A, B, C]) = ssum
}

trait SSumMacros extends Macro with SBinOpMacros {
  import c.universe._

  val op: (Int, Int) => Int = _ + _

  def witness[A : c.WeakTypeTag, B : c.WeakTypeTag, C : c.WeakTypeTag] =
    reify(new SSum[A, B, C] {})
}

trait SDiff[A, B, C]

object SDiff {
  implicit def mkSDiff[A, B, C] = macro SDiffMacros.witness[A, B, C]

  def sdiff[A, B, C](a: SNat[A], b: SNat[B])(implicit sdiff: SDiff[A, B, C]) = sdiff
}

trait SDiffMacros extends Macro with SBinOpMacros {
  import c.universe._

  val op: (Int, Int) => Int = _ - _

  def witness[A : c.WeakTypeTag, B : c.WeakTypeTag, C : c.WeakTypeTag] =
    reify(new SDiff[A, B, C] {})
}

trait SProd[A, B, C]

object SProd {
  implicit def mkSProd[A, B, C] = macro SProdMacros.witness[A, B, C]

  def sprod[A, B, C](a: SNat[A], b: SNat[B])(implicit sprod: SProd[A, B, C]) = sprod
}

trait SProdMacros extends Macro with SBinOpMacros {
  import c.universe._

  val op: (Int, Int) => Int = _ * _

  def witness[A : c.WeakTypeTag, B : c.WeakTypeTag, C : c.WeakTypeTag] =
    reify(new SProd[A, B, C] {})
}

trait SDiv[A, B, C]

object SDiv {
  implicit def mkSDiv[A, B, C] = macro SDivMacros.witness[A, B, C]

  def sdiv[A, B, C](a: SNat[A], b: SNat[B])(implicit sdiv: SDiv[A, B, C]) = sdiv
}

trait SDivMacros extends Macro with SBinOpMacros {
  import c.universe._

  val op: (Int, Int) => Int = _ / _

  def witness[A : c.WeakTypeTag, B : c.WeakTypeTag, C : c.WeakTypeTag] =
    reify(new SDiv[A, B, C] {})
}

trait SMod[A, B, C]

object SMod {
  implicit def mkSMod[A, B, C] = macro SModMacros.witness[A, B, C]

  def smod[A, B, C](a: SNat[A], b: SNat[B])(implicit smod: SMod[A, B, C]) = smod
}

trait SModMacros extends Macro with SBinOpMacros {
  import c.universe._

  val op: (Int, Int) => Int = _ % _

  def witness[A : c.WeakTypeTag, B : c.WeakTypeTag, C : c.WeakTypeTag] =
    reify(new SMod[A, B, C] {})
}

trait SBinOpMacros { self: Macro =>
  import c.universe._

  val op: (Int, Int) => Int

  override def onInfer(tic: c.TypeInferenceContext): Unit = {
    val A = tic.unknowns(0)
    val B = tic.unknowns(1)
    val C = tic.unknowns(2)

    tic.expectedType match {
      case TypeRef(_, _,
        List(aTpe @ ConstantType(Constant(a: Int)), bTpe @ ConstantType(Constant(b: Int)), _)) if(op(a, b) >= 0) => 
        tic.infer(A, aTpe)
        tic.infer(B, bTpe)
        tic.infer(C, ConstantType(Constant(op(a, b))))

      case _ =>
    }
  }
}

trait SLt[A, B, C]

object SLt {
  implicit def mkLt[A, B, C] = macro SLtMacros.witness[A, B, C]

  def slt[A, B, C](a: SNat[A], b: SNat[B])(implicit slt: SLt[A, B, C]) = slt
}

trait SLtMacros extends Macro with SRelOpMacros {
  import c.universe._

  val op: (Int, Int) => Boolean = _ < _

  def witness[A : c.WeakTypeTag, B : c.WeakTypeTag, C : c.WeakTypeTag] =
    reify(new SLt[A, B, C] {})
}

trait SLtEq[A, B, C]

object SLtEq {
  implicit def mkLtEq[A, B, C] = macro SLtEqMacros.witness[A, B, C]

  def slteq[A, B, C](a: SNat[A], b: SNat[B])(implicit slteq: SLtEq[A, B, C]) = slteq
}

trait SLtEqMacros extends Macro with SRelOpMacros {
  import c.universe._

  val op: (Int, Int) => Boolean = _ <= _

  def witness[A : c.WeakTypeTag, B : c.WeakTypeTag, C : c.WeakTypeTag] =
    reify(new SLtEq[A, B, C] {})
}

trait SRelOpMacros { self: Macro =>
  import c.universe._

  val op: (Int, Int) => Boolean

  override def onInfer(tic: c.TypeInferenceContext): Unit = {
    val A = tic.unknowns(0)
    val B = tic.unknowns(1)
    val C = tic.unknowns(2)

    tic.expectedType match {
      case TypeRef(_, _,
        List(aTpe @ ConstantType(Constant(a: Int)), bTpe @ ConstantType(Constant(b: Int)), _)) => 
        tic.infer(A, aTpe)
        tic.infer(B, bTpe)
        tic.infer(C, ConstantType(Constant(op(a, b))))

      case _ =>
    }
  }
}
