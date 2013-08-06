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

import scala.language.existentials
import scala.language.experimental.macros

import scala.reflect.macros.Context
import scala.tools.reflect.ToolBoxError

object SingletonTypes {
  def refine[T](t: T) = macro SingletonTypeMacros.refineImpl[T]

  implicit def mkSingletonOps[T](t: T) = macro SingletonTypeMacros.mkSingletonOps[T]

  import Witness.WitnessEq

  trait SingletonOps {
    import Record.{ FieldType, key }

    type T
    val witness: WitnessEq[T]

    def narrow: T with Singleton = witness.value

    def ->>[V](v: V): (FieldType[T, V], V) = (key[V](witness.value), v)
  }

  type SingletonOpsLt[Lub] = SingletonOps { type T <: Lub }
}

trait Witness {
  type T
  val value: T
}

trait WitnessAux[T] {
  val value: T
}

object WitnessAux {
  implicit def witnessAux[T0](implicit w: Witness { type T = T0 }): WitnessAux[T0] = new WitnessAux[T0] {
    val value = w.value
  }
}

object Witness {
  type WitnessEq[T0] = Witness { type T = T0 }
  type WitnessLt[Lub] = Witness { type T <: Lub }

  implicit def apply[T] = macro SingletonTypeMacros.materializeImpl[T]

  implicit def apply[T](t: T) = macro SingletonTypeMacros.convertImpl[T]
}

import Witness.{ WitnessEq, WitnessLt }

trait SingletonTypeMacros[C <: Context] {
  import SingletonTypes.{ SingletonOps, SingletonOpsLt }

  val c: C

  import c.universe._
  import Flag._

  def eval[T](t: Tree) = try { 
    Some(c.eval(c.Expr[T](c.resetAllAttrs(t.duplicate))))
  } catch {
    case ex: ToolBoxError => None
  }

  def mkSingletonType(s: Any) =
    CompoundTypeTree(
      Template(
        List(
          TypeTree(ConstantType(Constant(s))),
          Ident(typeOf[Singleton].typeSymbol)
        ),
        emptyValDef,
        List()
      )
    )

  def refineImpl[T](t: c.Expr[T]) = {
    val t0 = eval[T](t.tree).getOrElse(
      c.abort(c.enclosingPosition, s"Expression ${t.tree} does not evaluate to a constant")
    )

    c.Expr[T] {
      Typed(
        Literal(Constant(t0)),
        mkSingletonType(t0)
      )
    }
  }

  def mkSingletonType0(s: Any) =
    TypeTree(ConstantType(Constant(s)))

  def mkWitnessT[W](sTpe: Type, s: Any) = mkWitness[W](TypeTree(sTpe), Literal(Constant(s)))

  def mkWitness[W](sTpt: TypTree, s: Tree) = {
    val witnessTpt = Ident(typeOf[Witness].typeSymbol)
    val T = TypeDef(Modifiers(), newTypeName("T"), List(), sTpt)
    val value = ValDef(Modifiers(), newTermName("value"), sTpt, s)
    c.Expr[W] {
      mkImplClass(witnessTpt, List(T, value), List())
    }
  }

  def materializeImpl[T: c.WeakTypeTag]: c.Expr[WitnessEq[T]] = {
    val singletonTpe = typeOf[Singleton]
    weakTypeOf[T] match {
      case t @ ConstantType(Constant(s)) => mkWitnessT(t, s)

      case t @ RefinedType(List(ConstantType(Constant(s)), p2), _) if p2 =:= singletonTpe => mkWitnessT(t, s)

      case t @ RefinedType(List(p1, ConstantType(Constant(s))), _) if p1 =:= singletonTpe => mkWitnessT(t, s)

      case t @ SingleType(p, v) if !v.isParameter =>
        mkWitness(TypeTree(t), TypeApply(Select(Ident(v), newTermName("asInstanceOf")), List(TypeTree(t))))

      case t =>
        c.abort(c.enclosingPosition, s"Type argument $t is not a singleton type")
    }
  }

  def convertImpl[T](t: c.Expr[T]): c.Expr[WitnessLt[T]] = {
    val t0 = eval[T](t.tree).getOrElse(
      c.abort(c.enclosingPosition, s"Expression ${t.tree} does not evaluate to a constant")
    )
    mkWitness(mkSingletonType0(t0), Literal(Constant(t0)))
  }

  def mkOps[O](sTpt: TypTree, w: Tree) = {
    val opsTpt = Ident(typeOf[SingletonOps].typeSymbol)
    val T = TypeDef(Modifiers(), newTypeName("T"), List(), sTpt)
    val value = ValDef(Modifiers(), newTermName("witness"), TypeTree(), w)
    c.Expr[O] {
      mkImplClass(opsTpt, List(T, value), List())
    }
  }

  def mkSingletonOps[T](t: c.Expr[T]): c.Expr[SingletonOpsLt[T]] = {
    val t0 = eval[T](t.tree).getOrElse(
      c.abort(c.enclosingPosition, s"Expression ${t.tree} does not evaluate to a constant")
    )
    val sTpt = mkSingletonType0(t0)
    val witness = mkWitness(sTpt , Literal(Constant(t0))).tree
    mkOps(sTpt, witness)
  }

  def constructor(prop: Boolean) =
    DefDef(
      Modifiers(),
      nme.CONSTRUCTOR,
      List(),
      List(
        if(prop)
          List(
            ValDef(Modifiers(PARAM), newTermName("i"), Ident(typeOf[Int].typeSymbol), EmptyTree)
          )
        else
          Nil
      ),
      TypeTree(),
      Block(
        List(
          Apply(
            Select(
              Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR
            ),
            if(prop)
              List(Ident(newTermName("i")))
            else
              Nil
          )
        ),
        Literal(Constant(()))
      )
    )

  def mkImplClass(parent: Tree, defns: List[Tree], args: List[Tree]): Tree = {
    val name = newTypeName(c.fresh())

    val classDef =
      ClassDef(
        Modifiers(FINAL),
        name,
        List(),
        Template(
          List(parent),
          emptyValDef,
          constructor(args.size > 0) :: defns
        )
      )

    Block(
      List(classDef),
      Apply(Select(New(Ident(name)), nme.CONSTRUCTOR), args)
    )
  }
}

object SingletonTypeMacros {
  import SingletonTypes.SingletonOpsLt

  def inst(c0: Context) = new SingletonTypeMacros[c0.type] { val c: c0.type = c0 }

  def eval[T](c: Context)(t: c.Tree) = inst(c).eval(t)

  def refineImpl[T](c: Context)(t: c.Expr[T]) = inst(c).refineImpl(t)

  def materializeImpl[T: c.WeakTypeTag](c: Context): c.Expr[WitnessEq[T]] = inst(c).materializeImpl[T]

  def convertImpl[T: c.WeakTypeTag](c: Context)(t: c.Expr[T]): c.Expr[WitnessLt[T]] = inst(c).convertImpl[T](t)

  def mkSingletonOps[T: c.WeakTypeTag](c: Context)(t: c.Expr[T]): c.Expr[SingletonOpsLt[T]] = inst(c).mkSingletonOps[T](t)
}
