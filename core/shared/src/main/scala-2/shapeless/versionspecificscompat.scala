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
import scala.reflect.macros.whitebox

trait ScalaVersionSpecifics {
  private[shapeless] type IterableTypeableResult[CC[X] <: Iterable[X], T] = CC[T] with Iterable[T]

  // Quantifiers
  type ∃[P[_]] = P[T] forSome { type T }
  type ∀[P[_]] = ¬[∃[({ type λ[X] = ¬[P[X]]})#λ]]

  def cachedImplicit[T]: T = macro CachedImplicitMacros.cachedImplicitImpl[T]

  private[shapeless] def implicitNotFoundMessage(c: whitebox.Context)(tpe: c.Type): String = {
    val global = c.universe.asInstanceOf[scala.tools.nsc.Global]
    val gTpe = tpe.asInstanceOf[global.Type]
    gTpe.typeSymbolDirect match {
      case global.analyzer.ImplicitNotFoundMsg(msg) =>
        msg.formatDefSiteMessage(gTpe)
      case _ =>
        s"Implicit value of type $tpe not found"
    }
  }
}

trait NeqInstances {
  implicit def neq[A, B] : A =:!= B = new =:!=[A, B] {}
  implicit def neqAmbig1[A] : A =:!= A = unexpected
  implicit def neqAmbig2[A] : A =:!= A = unexpected
}

trait NsubInstances {
  implicit def nsub[A, B] : A <:!< B = new <:!<[A, B] {}
  implicit def nsubAmbig1[A, B >: A] : A <:!< B = unexpected
  implicit def nsubAmbig2[A, B >: A] : A <:!< B = unexpected
}

trait CaseClassMacrosVersionSpecifics { self: CaseClassMacros =>
  import c.universe._

  val varargTpt = tq"_root_.scala.collection.immutable.Seq"
  val varargTC = typeOf[scala.collection.immutable.Seq[_]].typeConstructor
}

class CachedImplicitMacros(val c: whitebox.Context) {
  import c.universe._

  def cachedImplicitImpl[T](implicit tTag: WeakTypeTag[T]): Tree = {
    val casted = c.asInstanceOf[reflect.macros.runtime.Context]
    val typer = casted.callsiteTyper
    val global: casted.universe.type = casted.universe
    val analyzer: global.analyzer.type = global.analyzer
    val tCtx = typer.context
    val owner = tCtx.owner
    if(!owner.isVal && !owner.isLazy)
      c.abort(c.enclosingPosition, "cachedImplicit should only be used to initialize vals and lazy vals")
    val tTpe = weakTypeOf[T]
    val application = casted.macroApplication
    val tpe = {
      val tpe0 =
        if (tTpe.typeSymbol.isParameter) owner.tpe.asInstanceOf[Type]
        else tTpe
      tpe0.finalResultType
    }
    val gTpe = tpe.asInstanceOf[global.Type]

    // Run our own custom implicit search that isn't allowed to find
    // the thing we are enclosed in
    val sCtx = tCtx.makeImplicit(false)
    val is = new analyzer.ImplicitSearch(
      tree = application,
      pt = gTpe,
      isView = false,
      context0 = sCtx,
      pos0 = c.enclosingPosition.asInstanceOf[global.Position]
    ) {
      override def searchImplicit(
        implicitInfoss: List[List[analyzer.ImplicitInfo]],
        isLocalToCallsite: Boolean
      ): analyzer.SearchResult = {
        val filteredInput = implicitInfoss.map { infos =>
          infos.filter { info =>
            val sym = if(info.sym.isLazy) info.sym else info.sym.accessedOrSelf
            sym.owner != owner.owner || (!sym.isVal && !sym.isLazy)
          }
        }
        super.searchImplicit(filteredInput, isLocalToCallsite)
      }
    }
    val best = is.bestImplicit
    if (best.isFailure) {
      val errorMsg = implicitNotFoundMessage(c)(tpe)
      c.abort(c.enclosingPosition, errorMsg)
    } else {
      best.tree.asInstanceOf[Tree]
    }
  }
}
