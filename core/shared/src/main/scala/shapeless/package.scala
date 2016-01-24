/*
 * Copyright (c) 2013-16 Miles Sabin
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

package object shapeless {
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
  trait =:!=[A, B]

  implicit def neq[A, B] : A =:!= B = new =:!=[A, B] {}
  implicit def neqAmbig1[A] : A =:!= A = unexpected
  implicit def neqAmbig2[A] : A =:!= A = unexpected

  trait <:!<[A, B]

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
  @macrocompat.bundle
  class CachedImplicitMacros(val c: whitebox.Context) {
    import c.universe._
    import scala.reflect.macros.TypecheckException
    import scala.util.matching.Regex

    def cachedImplicitImpl[T](implicit tTag: WeakTypeTag[T]): Tree = {
      val casted = c.asInstanceOf[reflect.macros.runtime.Context]
      val typer = casted.callsiteTyper
      val global: casted.universe.type = casted.universe
      val analyzer: global.analyzer.type = global.analyzer
      val tCtx = typer.context
      val owner = tCtx.owner
      val tTpe = weakTypeOf[T]
      val application = casted.macroApplication
      val tpe = {
        if (tTpe.typeSymbol.isParameter) owner.tpe
        else tTpe
      }.asInstanceOf[global.Type]

      // Run our own custom implicit search that isn't allowed to find
      // the thing we are enclosed in
      val sCtx = tCtx.makeImplicit(true)

      // 2.12 changed the signature on a method we need to override. In order to allow us to define
      // both versions below with the "override" modifier, we need an interface that contains both.
      trait CompatImplicitSearch extends analyzer.ImplicitsContextErrors { self: analyzer.ImplicitSearch =>
        import analyzer.global.{Type, Tree}
        import analyzer.{ImplicitInfo, Context}
        def AmbiguousImplicitError(
          info1: ImplicitInfo,
          info2: ImplicitInfo,
          pre1: String, pre2: String,
          trailer: String
        )(isView: Boolean, pt: Type, tree: Tree)(implicit context0: Context): Unit

        def AmbiguousImplicitError(
            info1: ImplicitInfo, tree1: Tree,
            info2: ImplicitInfo, tree2: Tree,
            pre1: String, pre2: String,
            trailer: String
        )(isView: Boolean, pt: Type, tree: Tree)(implicit context0: Context): Unit
      }

      val is = new analyzer.ImplicitSearch(
        tree=application,
        pt=tpe,
        isView=false,
        context0=sCtx,
        pos0=c.enclosingPosition.asInstanceOf[global.Position]
      ) with CompatImplicitSearch {

        import analyzer.global.{Type, Tree}
        import analyzer.{ImplicitInfo, Context}

        override def searchImplicit(
          implicitInfoss: List[List[analyzer.ImplicitInfo]],
          isLocalToCallsite: Boolean
        ): analyzer.SearchResult = {
          val filteredInput = implicitInfoss.map { infos =>
            infos.filter(_.sym.accessedOrSelf != owner)
          }
          super.searchImplicit(filteredInput, isLocalToCallsite)
        }

        // Lift a little bit of compiler code. This could be avoided
        // by using variant sources
        object ImplicitAmbiguousMsg {
          import global._
          val ImplicitAmbiguousClass = global.rootMirror.getClassIfDefined("scala.annotation.implicitAmbiguous")
          def unapply(sym: global.Symbol): Option[Message] = {
            val msg = sym.getAnnotation(ImplicitAmbiguousClass).flatMap { _.stringArg(0) }
            msg.map(new Message(sym, _))
          }
          class Message(sym: Symbol, msg: String) {
            private val Intersobralator = """\$\{\s*([^}\s]+)\s*\}""".r
            private def interpolate(text: String, vars: Map[String, String]) =
              Intersobralator.replaceAllIn(text, (_: Regex.Match) match {
                  case Regex.Groups(v) => Regex quoteReplacement vars.getOrElse(v, "")
              })
            private lazy val typeParamNames: List[String] = sym.typeParams.map(_.decodedName)
            def format(typeArgs: List[String]): String =
              interpolate(msg, Map((typeParamNames zip typeArgs): _*))
          }
        }

        override def AmbiguousImplicitError(
          info1: ImplicitInfo, tree1: Tree,
          info2: ImplicitInfo, tree2: Tree,
          pre1: String, pre2: String,
          trailer: String
        )(isView: Boolean, pt: Type, tree: Tree)(implicit context0: Context): Unit = {

          import global._

          def treeTypeArgs(annotatedTree: Tree): List[String] = annotatedTree match {
            case TypeApply(_, args) => args.map(_.toString)
            case Block(_, Function(_, treeInfo.Applied(_, targs, _))) => targs.map(_.toString)
            case _ => Nil
          }

          (info1.sym, info2.sym) match {
            case (ImplicitAmbiguousMsg(msg), _) =>
              c.abort(c.enclosingPosition, msg.format(treeTypeArgs(tree1)))
            case (_, ImplicitAmbiguousMsg(msg)) =>
              c.abort(c.enclosingPosition, msg.format(treeTypeArgs(tree2)))
            case (_, _) =>
              AmbiguousImplicitError(info1, info2, pre1, pre2, trailer)(isView, pt, tree)
          }
        }

        override def AmbiguousImplicitError(
          info1: ImplicitInfo,
          info2: ImplicitInfo,
          pre1: String, pre2: String,
          trailer: String
        )(isView: Boolean, pt: Type, tree: Tree)(implicit context0: Context): Unit = {
          if (!info1.tpe.isErroneous && !info2.tpe.isErroneous) {
            val coreMsg =
              s"""| $pre1 ${info1.sym.fullLocationString} of type ${info1.tpe}
                  | $pre2 ${info2.sym.fullLocationString} of type ${info2.tpe}
                  | $trailer""".stripMargin.trim
            val msg = s"ambiguous implicit values:\n${coreMsg}\nmatch expected type $pt"
            c.abort(c.enclosingPosition, msg)
          }
        }
      }

      val best = is.bestImplicit

      sCtx.reporter.errors.foreach { err =>
        c.abort(err.errPos.asInstanceOf[c.Position], err.errMsg)
      }

      if (best.isFailure) {

        val errorMsg = tpe.typeSymbolDirect match {
          case analyzer.ImplicitNotFoundMsg(msg) =>
            msg.format(TermName("evidence").asInstanceOf[global.TermName], tpe)
          case _ =>
            s"Could not find an implicit $tpe to cache"
        }
        c.abort(c.enclosingPosition, errorMsg)
      } else {
        best.tree.asInstanceOf[Tree]
      }
    }
  }
}
