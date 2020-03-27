/*
 * Copyright (c) 2015-16 Miles Sabin
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

import scala.reflect.macros.whitebox

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

class OrphanMacros(val c: whitebox.Context) extends CaseClassMacros {
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

  def materializeAux[F[_], D, T](proxied: Boolean)
    (implicit fTag: WeakTypeTag[F[_]], dTag: WeakTypeTag[D], tTag: WeakTypeTag[T]): Tree = {
    val fTcTpe = fTag.tpe.typeConstructor
    val dTpe = dTag.tpe
    val tTpe = tTag.tpe
    val appTpe = appliedType(fTcTpe, List(tTpe))

    val open = c.openImplicits
    val materializerIdx = if(proxied) 1 else 0
    val materializer = open(materializerIdx)
    val checkIdx = (materializerIdx*2)+1
    if(open.size > checkIdx) {
      val check = open(checkIdx)
      if(materializer.sym == check.sym && materializer.pre =:= check.pre && materializer.pt =:= check.pt)
        c.abort(c.enclosingPosition, "Backtrack")
    }

    val deriver = dTpe match {
      case singleton: SingleType => mkAttributedRef(singleton)
      case _ => c.abort(c.enclosingPosition, s"Deriver $dTpe not found")
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

    val checkedProbe = c.typecheck(probe, pt = appTpe, silent = true)
    if(checkedProbe == EmptyTree) {
      if(inst == EmptyTree) {
        c.abort(c.enclosingPosition, s"No derived instance $appTpe")
      } else {
        inst
      }
    } else {
      val derived = (checkedProbe: @unchecked) match {
        case b: Block => b.expr
      }

      if(derived.equalsStructure(inst)) inst
      else if(inst == EmptyTree) derived
      else {
        val resTpeD = derived.symbol.asMethod.info.finalResultType
        val resTpeI = inst.symbol.asMethod.info.finalResultType

        val useDerived =
          resTpeD.typeArgs.zip(resTpeI.typeArgs).forall { case (ad, ai) =>
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
