/*
 * Copyright (c) 2013-18 Miles Sabin
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

import scala.language.experimental.macros

import scala.reflect.macros.whitebox

package object shapeless extends ScalaVersionSpecifics {
  def unexpected : Nothing = sys.error("Unexpected invocation")

  // Basic definitions
  type Id[+T] = T
  type Const[C] = {
    type λ[T] = C
  }

  type ¬[T] = T => Nothing
  type ¬¬[T] = ¬[¬[T]]
  type ∧[T, U] = T with U
  type ∨[T, U] = ¬[¬[T] ∧ ¬[U]]

  // Type-lambda for context bound
  type |∨|[T, U] = {
    type λ[X] = ¬¬[X] <:< (T ∨ U)
  }

  // Type inequalities
  trait =:!=[A, B] extends Serializable

  implicit def neq[A, B] : A =:!= B = new =:!=[A, B] {}
  implicit def neqAmbig1[A] : A =:!= A = unexpected
  implicit def neqAmbig2[A] : A =:!= A = unexpected

  @scala.annotation.implicitNotFound("${A} must not be a subtype of ${B}")
  trait <:!<[A, B] extends Serializable

  implicit def nsub[A, B] : A <:!< B = new <:!<[A, B] {}
  implicit def nsubAmbig1[A, B >: A] : A <:!< B = unexpected
  implicit def nsubAmbig2[A, B >: A] : A <:!< B = unexpected

  // Type-lambda for context bound
  type |¬|[T] = {
    type λ[U] = U <:!< T
  }

  // Quantifiers
  type ∃[P[_]] = P[T] forSome { type T }
  type ∀[P[_]] = ¬[∃[({ type λ[X] = ¬[P[X]]})#λ]]

  /** `Optic` definitions */
  val optic = OpticDefns
  val lens = OpticDefns
  val prism = OpticDefns
  val ^ = Path

  /** `Nat` literals */
  val nat = Nat

  /** 'Fin' */
  val fin = Fin

  /** `Poly` definitions */
  val poly = PolyDefns
  import poly._

  /** Dependent nullary function type. */
  trait DepFn0 {
    type Out
    def apply(): Out
  }

  /** Dependent unary function type. */
  trait DepFn1[T] {
    type Out
    def apply(t: T): Out
  }

  /** Dependent binary function type. */
  trait DepFn2[T, U] {
    type Out
    def apply(t: T, u: U): Out
  }

  /** The SYB everything combinator */
  type Everything[F <: Poly, K <: Poly, T] = Case1[EverythingAux[F, K], T]

  class ApplyEverything[F <: Poly] {
    def apply(k : Poly): EverythingAux[F, k.type] {} = new EverythingAux[F, k.type]
  }

  def everything(f: Poly): ApplyEverything[f.type] {} = new ApplyEverything[f.type]

  /** The SYB everywhere combinator */
  type Everywhere[F <: Poly, T] = Case1[EverywhereAux[F], T]

  def everywhere(f: Poly): EverywhereAux[f.type] {} = new EverywhereAux[f.type]

  def cachedImplicit[T]: T = macro CachedImplicitMacros.cachedImplicitImpl[T]
}

package shapeless {
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
}
