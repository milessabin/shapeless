/*
 * Copyright (c) 2013-14 Miles Sabin
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

import tag.@@

trait Witness {
  type T
  val value: T {}
}

object Witness {
  type Aux[T0] = Witness { type T = T0 }
  type Lt[Lub] = Witness { type T <: Lub }

  implicit def apply[T]: Witness.Aux[T] = macro SingletonTypeMacros.materializeImpl[T]

  implicit def apply[T](t: T): Witness.Lt[T] = macro SingletonTypeMacros.convertImpl[T]

  implicit val witness0: Witness.Aux[_0] =
    new Witness {
      type T = _0
      val value = Nat._0
    }

  implicit def witnessN[P <: Nat]: Witness.Aux[Succ[P]] =
    new Witness {
      type T = Succ[P]
      val value = new Succ[P]()
    }
}

trait WitnessWith[TC[_]] extends Witness {
  val instance: TC[T]
  type Out
}

trait LowPriorityWitnessWith {
  implicit def apply2[H, TC2[_ <: H, _], S <: H, T](t: T): WitnessWith.Lt[({ type λ[X] = TC2[S, X] })#λ, T] =
    macro SingletonTypeMacros.convertInstanceImpl2[H, TC2, S, T]
}

object WitnessWith extends LowPriorityWitnessWith {
  type Aux[TC[_], T0] = WitnessWith[TC] { type T = T0  }
  type Lt[TC[_], Lub] = WitnessWith[TC] { type T <: Lub }

  implicit def apply1[TC[_], T](t: T): WitnessWith.Lt[TC, T] = macro SingletonTypeMacros.convertInstanceImpl1[TC, T]
}

class SingletonTypeMacros[C <: Context](val c: C) {
  import syntax.SingletonOps
  type SingletonOpsLt[Lub] = SingletonOps { type T <: Lub }

  import c.universe._

  val SymTpe = typeOf[scala.Symbol]

  def mkWitness(sTpe: Type, s: Tree): Tree = {
    val name = newTypeName(c.fresh())

    q"""
      {
        final class $name extends Witness {
          type T = $sTpe
          val value: $sTpe = $s
        }
        new $name
      }
    """
  }

  def mkWitnessWith(parent: Type, sTpe: Type, s: Tree, i: Tree): Tree = {
    val name = newTypeName(c.fresh())
    val iTpe =
      (i.tpe match {
        case NullaryMethodType(resTpe) => resTpe
        case other => other
      }).normalize
    val iOut = iTpe.member(newTypeName("Out")) match {
      case NoSymbol => definitions.NothingClass
      case other => other
    }

    q"""
      {
        final class $name extends $parent {
          val instance: $iTpe = $i
          type T = $sTpe
          type Out = $iOut
          val value: $sTpe = $s
        }
        new $name
      }
    """
  }

  def mkOps(sTpe: Type, w: Tree): Tree = {
    val name = newTypeName(c.fresh())

    q"""
      {
        final class $name extends _root_.shapeless.syntax.SingletonOps {
          type T = $sTpe
          val witness = $w
        }
        new $name
      }
    """
  }

  object LiteralSymbol {
    def unapply(t: Tree): Option[Constant] = t match {
      case q""" scala.Symbol.apply(${Literal(c: Constant)}) """ => Some(c)
      case _ => None
    }
  }

  object SingletonSymbolType {
    val atatTpe = typeOf[@@[_,_]].typeConstructor
    val TaggedSym = typeOf[tag.Tagged[_]].typeConstructor.typeSymbol

    def apply(c: Constant): Type = appliedType(atatTpe, List(SymTpe, ConstantType(c)))

    def unapply(t: Type): Option[Constant] =
      t match {
        case RefinedType(List(SymTpe, TypeRef(_, TaggedSym, List(ConstantType(c)))), _) => Some(c)
        case _ => None
      }
  }

  def mkSingletonSymbol(c: Constant): Tree = {
    val sTpe = SingletonSymbolType(c)
    q"""_root_.scala.Symbol($c).asInstanceOf[$sTpe]"""
  }

  def materializeImpl[T: WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[T].normalize
    val value =
      tpe match {
        case ConstantType(c: Constant) => Literal(c)

        case SingleType(p, v) if !v.isParameter => q"""$v.asInstanceOf[$tpe]"""

        case SingletonSymbolType(c) => mkSingletonSymbol(c)

        case _ =>
          c.abort(c.enclosingPosition, s"Type argument $tpe is not a singleton type")
      }
    mkWitness(tpe, value)
  }

  def extractResult[T](t: Expr[T])(mkResult: (Type, Tree) => Tree): Tree =
    (t.actualType, t.tree) match {
      case (tpe @ ConstantType(c: Constant), _) =>
        mkResult(tpe, Literal(c))

      case (tpe @ SingleType(p, v), tree) if !v.isParameter =>
        mkResult(tpe, tree)

      case (SymTpe, LiteralSymbol(c)) =>
        mkResult(SingletonSymbolType(c), mkSingletonSymbol(c))

      case _ =>
        c.abort(c.enclosingPosition, s"Expression ${t.tree} does not evaluate to a constant or a stable value")
    }

  def convertImpl[T](t: Expr[T]): Tree = extractResult(t)(mkWitness)

  def inferInstance(tci: Type): Tree = {
    val i = c.inferImplicitValue(tci)
    if(i == EmptyTree)
      c.abort(c.enclosingPosition, s"Unable to resolve implicit value of type $tci")
    i
  }

  def convertInstanceImpl1[TC[_], T](t: Expr[T])
    (implicit tcTag: WeakTypeTag[TC[_]]): Tree =
      extractResult(t) { (sTpe, value) =>
        val tc = tcTag.tpe.typeConstructor
        val wwTC = typeOf[WitnessWith[Nothing]].typeConstructor
        val parent = appliedType(wwTC, List(tc))
        val tci = appliedType(tc, List(sTpe))
        val i = inferInstance(tci)
        mkWitnessWith(parent, sTpe, value, i)
      }

  def convertInstanceImpl2[H, TC2[_ <: H, _], S <: H, T](t: Expr[T])
    (implicit tc2Tag: WeakTypeTag[TC2[_, _]], sTag: WeakTypeTag[S]): Tree =
      extractResult(t) { (sTpe, value) =>
        val tc2 = tc2Tag.tpe.typeConstructor
        val s = sTag.tpe

        val parent = weakTypeOf[WitnessWith[({ type λ[X] = TC2[S, X] })#λ]].map {
          case TypeRef(prefix, sym, args) if sym.isFreeType =>
            typeRef(NoPrefix, tc2.typeSymbol, args)
          case tpe => tpe
        }

        val tci = appliedType(tc2, List(s, sTpe))
        val i = inferInstance(tci)
        mkWitnessWith(parent, sTpe, value, i)
      }

  def mkSingletonOps(t: Expr[Any]): Tree =
    extractResult(t) { (tpe, tree) => mkOps(tpe, mkWitness(tpe, tree)) }

  def narrowSymbol[S <: String : WeakTypeTag](t: Expr[scala.Symbol]): Tree = {
    (weakTypeOf[S], t.tree) match {
      case (ConstantType(Constant(s1)), LiteralSymbol(Constant(s2))) if s1 == s2 =>
        mkSingletonSymbol(Constant(s1))
      case _ =>
        c.abort(c.enclosingPosition, s"Expression ${t.tree} is not an appropriate Symbol literal")
    }
  }
}

object SingletonTypeMacros {
  import syntax.SingletonOps

  def inst(c: Context) = new SingletonTypeMacros[c.type](c)

  def materializeImpl[T: c.WeakTypeTag](c: Context): c.Expr[Witness.Aux[T]] =
    c.Expr[Witness.Aux[T]](inst(c).materializeImpl[T])

  def convertImpl[T](c: Context)(t: c.Expr[T]): c.Expr[Witness.Lt[T]] = 
    c.Expr[Witness.Lt[T]](inst(c).convertImpl(t))

  def convertInstanceImpl1[TC[_], T](c: Context)(t: c.Expr[T])
    (implicit tcTag: c.WeakTypeTag[TC[_]]):
      c.Expr[WitnessWith.Lt[TC, T]] = c.Expr[WitnessWith.Lt[TC, T]](inst(c).convertInstanceImpl1[TC, T](t))

  def convertInstanceImpl2[H, TC2[_ <: H, _], S <: H, T](c: Context)(t: c.Expr[T])
    (implicit tc2Tag: c.WeakTypeTag[TC2[_, _]], sTag: c.WeakTypeTag[S]):
      c.Expr[WitnessWith.Lt[({ type λ[X] = TC2[S, X] })#λ, T]] =
        c.Expr[WitnessWith.Lt[({ type λ[X] = TC2[S, X] })#λ, T]](inst(c).convertInstanceImpl2[H, TC2, S, T](t))

  def mkSingletonOps(c: Context)(t: c.Expr[Any]): c.Expr[SingletonOps] =
    c.Expr[SingletonOps](inst(c).mkSingletonOps(t))

  def narrowSymbol[S <: String : c.WeakTypeTag](c: Context)(t: c.Expr[scala.Symbol]):
    c.Expr[scala.Symbol @@ S] = c.Expr[scala.Symbol @@ S](inst(c).narrowSymbol[S](t))
}
