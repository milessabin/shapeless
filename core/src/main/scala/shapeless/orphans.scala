/*
 * Copyright (c) 2015 Miles Sabin
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

case class Orphan[F[_], D, T](instance: F[T])

object Orphan {
  implicit def materializeOrphan[F[_], D, T]: Orphan[F, D, T] = macro OrphanMacros.materializeOrphanImpl[F, D, T]
}

case class WrappedOrphan[T](instance: T)

object WrappedOrphan {
  implicit def apply[T]: WrappedOrphan[T] = macro OrphanMacros.materializeWrapped[T]
}

trait OrphanDeriver[F[_], D] {
  implicit def materialize[T]: F[T] = macro OrphanMacros.materializeImpl[F, D, T]
}

class OrphanMacros[C <: Context](val c: C) extends CaseClassMacros {
  import c.universe._

  def materializeImpl[F[_], D, T]
    (implicit fTag: WeakTypeTag[F[_]], dTag: WeakTypeTag[D], tTag: WeakTypeTag[T]): Tree =
    materializeAux[F, D, T](false)

  def materializeOrphanImpl[F[_], D, T]
    (implicit fTag: WeakTypeTag[F[_]], dTag: WeakTypeTag[D], tTag: WeakTypeTag[T]): Tree = {
    val inst = materializeAux[F, D, T](true)
    val fTpe = fTag.tpe.typeConstructor
    val dTpe = dTag.tpe
    val tTpe = tTag.tpe

    q"""
      new _root_.shapeless.Orphan[$fTpe, $dTpe, $tTpe]($inst)
     """
  }

  case class OpenImplicit(pre: Type, sym: Symbol, pt: Type, tree: Tree)
  def openImplicits: List[OpenImplicit] = {
    val typer = c.asInstanceOf[scala.reflect.macros.runtime.Context].callsiteTyper
    typer.context.openImplicits.map { oi =>
      OpenImplicit(
        oi.info.pre.asInstanceOf[Type],
        oi.info.sym.asInstanceOf[Symbol],
        oi.pt.asInstanceOf[Type],
        oi.tree.asInstanceOf[Tree])
    }
  }

  def finalResultType(tpe: Type): Type = {
    val global = c.universe.asInstanceOf[scala.tools.nsc.Global]
    val gTpe = tpe.asInstanceOf[global.Type]
    gTpe.finalResultType.asInstanceOf[Type]
  }

  def typeArgs(tpe: Type) = tpe match {
    case TypeRef(_, _, args) => args
    case _ => Nil
  }

  def materializeAux[F[_], D, T](proxied: Boolean)
    (implicit fTag: WeakTypeTag[F[_]], dTag: WeakTypeTag[D], tTag: WeakTypeTag[T]): Tree = {
    val fTcTpe = fTag.tpe.typeConstructor
    val dTpe = dTag.tpe
    val tTpe = tTag.tpe
    val appTpe = appliedType(fTcTpe, List(tTpe))

    val open = openImplicits
    val materializerIdx = if(proxied) 1 else 0
    val materializer = open(materializerIdx)
    val checkIdx = (materializerIdx*2)+1
    if(open.size > checkIdx) {
      val check = open(checkIdx)
      if(materializer.sym == check.sym && materializer.pre =:= check.pre && materializer.pt =:= check.pt)
        c.abort(c.enclosingPosition, "Backtrack")
    }

    val deriver =
      dTpe match {
        case SingleType(pre, sym) => mkAttributedRef(pre, sym)
        case other =>
          c.abort(c.enclosingPosition, "Deriver $dTpe not found")
      }

    val inst = c.inferImplicitValue(appTpe, silent = true)

    val masks =
      if(!proxied) List(q"def materialize = ???")
      else {
        val proxyOwner = materializer.sym.owner
        val proxyTpe = proxyOwner.typeSignature
        val proxyNames = proxyTpe.members.filter(_.isImplicit).map(_.name)

        proxyNames.map { name => q"def ${name.toTermName} = ???" }
      }

    val probe =
      q"""
        ..$masks
        import $deriver._
        _root_.shapeless.the[$appTpe]
       """

    val checkedProbe = c.typeCheck(probe, pt = appTpe, silent = true)
    if(checkedProbe == EmptyTree) {
      if(inst == EmptyTree) {
        c.abort(c.enclosingPosition, s"No derived instance $appTpe")
      } else {
        inst
      }
    } else {
      val derived = checkedProbe match {
        case b: Block => b.expr
      }

      if(derived.equalsStructure(inst)) inst
      else if(inst == EmptyTree) derived
      else {
        val resTpeD = finalResultType(derived.symbol.asMethod.typeSignature)
        val resTpeI = finalResultType(inst.symbol.asMethod.typeSignature)

        val useDerived =
          typeArgs(resTpeD).zip(typeArgs(resTpeI)).forall { case (ad, ai) =>
            ai.typeSymbol.isParameter ||
            (!ad.typeSymbol.isParameter && !(ad <:< ai))
          }

        if(useDerived) derived else inst
      }
    }
  }

  def materializeWrapped[T](implicit tTag: WeakTypeTag[T]): Tree = {
    val open = c.openImplicits
    val masks =
      if(open.size < 2) Nil
      else {
        val sym = open(1).sym
        List(q"def ${sym.name.toTermName} = ???")
      }

    val tpe = tTag.tpe
    q"""
    {
      ..$masks
      _root_.shapeless.WrappedOrphan[$tpe](_root_.shapeless.lazily[$tpe])
    }
    """
  }
}

object OrphanMacros {
  def inst(c: Context) = new OrphanMacros[c.type](c)

  def materializeImpl[F[_], D, T](c: Context)
    (implicit fTag: c.WeakTypeTag[F[_]], dTag: c.WeakTypeTag[D], tTag: c.WeakTypeTag[T]): c.Expr[F[T]] =
      c.Expr[F[T]](inst(c).materializeImpl[F, D, T])

  def materializeOrphanImpl[F[_], D, T](c: Context)
    (implicit fTag: c.WeakTypeTag[F[_]], dTag: c.WeakTypeTag[D], tTag: c.WeakTypeTag[T]): c.Expr[Orphan[F, D, T]] =
      c.Expr[Orphan[F, D, T]](inst(c).materializeOrphanImpl[F, D, T])
}
