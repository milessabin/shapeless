/*
 * Copyright (c) 2013-15 Miles Sabin
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

import scala.collection.immutable.ListMap
import scala.reflect.macros.whitebox

/**
 * Wraps a lazily computed value. Also circumvents cycles during implicit search, or wrong implicit divergences
 * as illustrated below, and holds the corresponding implicit value lazily.
 *
 * The following implicit search sometimes fails to compile, because of a wrongly reported implicit divergence,
 * {{{
 *   case class ListCC(list: List[CC])
 *   case class CC(i: Int, s: String)
 *
 *   trait TC[T]
 *
 *   object TC {
 *     implicit def intTC: TC[Int] = ???
 *     implicit def stringTC: TC[String] = ???
 *     implicit def listTC[T](implicit underlying: TC[T]): TC[List[T]] = ???
 *
 *     implicit def genericTC[F, G](implicit
 *       gen: Generic.Aux[F, G],
 *       underlying: TC[G]
 *     ): TC[F] = ???
 *
 *     implicit def hnilTC: TC[HNil] = ???
 *
 *     implicit def hconsTC[H, T <: HList](implicit
 *       headTC: TC[H],
 *       tailTC: TC[T]
 *     ): TC[H :: T] = ???
 *   }
 *
 *   implicitly[TC[CC]] // fails with: diverging implicit expansion for type TC[CC]
 * }}}
 *
 * This wrongly reported implicit divergence can be circumvented by wrapping some of the implicit values in
 * `Lazy`,
 * {{{
 *   case class ListCC(list: List[CC])
 *   case class CC(i: Int, s: String)
 *
 *   trait TC[T]
 *
 *   object TC {
 *     implicit def listTC[T](implicit underlying: TC[T]): TC[List[T]] = ???
 *
 *     implicit def genericTC[F, G](implicit
 *       gen: Generic.Aux[F, G],
 *       underlying: Lazy[TC[G]] // wrapped in Lazy
 *     ): TC[F] = ???
 *
 *     implicit def hnilTC: TC[HNil] = ???
 *
 *     implicit def hconsTC[H, T <: HList](implicit
 *       headTC: Lazy[TC[H]], // wrapped in Lazy
 *       tailTC: TC[T]
 *     ): TC[H :: T] = ???
 *   }
 *
 *   implicitly[TC[CC]]
 * }}}
 *
 * When looking for an implicit `Lazy[TC[T]]`, the `Lazy.mkLazy` macro will itself trigger the implicit search
 * for a `TC[T]`. If this search itself triggers searches for types wrapped in `Lazy`, these will be done
 * only once, their result put in a `lazy val`, and a reference to this `lazy val` will be returned as the corresponding
 * value. It will then wrap all the resulting values together, and return a reference to the first one.
 *
 * E.g. with the above example definitions, when looking up for an implicit `TC[CC]`, the returned tree roughly looks
 * like
 * {{{
 *   TC.genericTC(
 *     Generic[CC], // actually, the tree returned by Generic.materialize, not written here for the sake of brevity
 *     Lazy {
 *       lazy val impl1: TC[List[CC] :: HNil] = TC.hconsTC(
 *         Lazy(impl2),
 *         TC.hnilTC
 *       )
 *       lazy val impl2: TC[List[CC]] = TC.listTC(TC.genericTC(
 *         Generic[CC], // actually, the tree returned by Generic.materialize
 *         Lazy(impl1)  // cycles to the initial TC[List[CC] :: HNil]
 *       ))
 *
 *       impl1
 *     }
 *   )
 * }}}
 *
 */
trait Lazy[+T] extends Serializable {
  val value: T

  def map[U](f: T => U): Lazy[U] = Lazy { f(value) }
  def flatMap[U](f: T => Lazy[U]): Lazy[U] = Lazy { f(value).value }
}

object Lazy {
  implicit def apply[T](t: => T): Lazy[T] =
    new Lazy[T] {
      lazy val value = t
    }

  def unapply[T](lt: Lazy[T]): Option[T] = Some(lt.value)

  class Values[T <: HList](val values: T) extends Serializable
  object Values {
    implicit val hnilValues: Values[HNil] = new Values(HNil)
    implicit def hconsValues[H, T <: HList](implicit lh: Lazy[H], t: Values[T]): Values[H :: T] =
      new Values(lh.value :: t.values)
  }

  def values[T <: HList](implicit lv: Lazy[Values[T]]): T = lv.value.values

  implicit def mkLazy[I]: Lazy[I] = macro LazyMacros.mkLazyImpl[I]


  /**
   * Wraps a lazily computed value, and circumvents implicit cycles / wrong implicit divergences, like `Lazy` does.
   * Assumes that all the contexts it is called in are equivalent, and caches its calculations.
   *
   * Unlike `Lazy`, `Lazy.Global` assumes that all the contexts in which values wrapped in it are looked up, are all
   * equivalent. This allows to cache the generated trees upon first call, and simply return the cached trees
   * during subsequent calls.
   *
   * Shares its cache with `Strict.Cache`.
   */
  trait Global[+T] extends Lazy[T] {
    override def map[U](f: T => U): Global[U] = Global { f(value) }
    def flatMap[U](f: T => Global[U]): Global[U] = Global { f(value).value }
  }

  object Global {
    implicit def apply[T](t: => T): Global[T] =
      new Global[T] {
        val value = t
      }

    def unapply[T](lt: Global[T]): Option[T] = Some(lt.value)

    implicit def mkGlobal[I]: Global[I] = macro LazyMacros.mkGlobalLazyImpl[I]
  }
}

object lazily {
  def apply[T](implicit lv: Lazy[T]): T = lv.value
}

/**
 * Wraps an eagerly computed value. Prevents wrongly reported implicit divergence, like `Lazy` does, but,
 * unlike it, does not circumvent implicit cycles.
 *
 * Creation of `Lazy` instances usually triggers the creation of an anonymous class, to compute the wrapped
 * value (e.g. with the by-name argument of `Lazy.apply`). `Strict` avoids that, which can lead to less
 * overhead during compilation.
 */
trait Strict[+T] extends Serializable {
  val value: T

  def map[U](f: T => U): Strict[U] = Strict { f(value) }
  def flatMap[U](f: T => Strict[U]): Strict[U] = Strict { f(value).value }
}

object Strict {
  implicit def apply[T](t: T): Strict[T] =
    new Strict[T] {
      val value = t
    }

  def unapply[T](lt: Strict[T]): Option[T] = Some(lt.value)

  implicit def mkStrict[I]: Strict[I] = macro LazyMacros.mkStrictImpl[I]


  /**
   * Wraps an eagerly computed value, and circumvents wrong implicit divergences, like `Strict`.
   * Assumes all the contexts it is called are equivalent, and caches its calculations.
   *
   * Unlike `Strict`, `Strict.Global` assumes that all the contexts in which values wrapped in it are looked up, are all
   * equivalent. This allows to cache the generated trees upon first call, and simply return the cached trees
   * during subsequent calls.
   *
   * Shares its cache with `Lazy.Cache`.
   */
  trait Global[+T] extends Strict[T] {
    override def map[U](f: T => U): Global[U] = Global { f(value) }
    def flatMap[U](f: T => Global[U]): Global[U] = Global { f(value).value }
  }

  object Global {
    implicit def apply[T](t: T): Global[T] =
      new Global[T] {
        val value = t
      }

    def unapply[T](lt: Global[T]): Option[T] = Some(lt.value)

    implicit def mkGlobal[I]: Global[I] = macro LazyMacros.mkGlobalStrictImpl[I]
  }
}

class LazyMacros(val c: whitebox.Context) {
  import c.universe._
  import c.ImplicitCandidate

  def mkLazyImpl[I](implicit iTag: WeakTypeTag[I]): Tree =
    mkImpl[I](
      (tree, actualType) => q"_root_.shapeless.Lazy.apply[$actualType]($tree)",
      q"null.asInstanceOf[_root_.shapeless.Lazy[Nothing]]",
      None
    )

  def mkStrictImpl[I](implicit iTag: WeakTypeTag[I]): Tree =
    mkImpl[I](
      (tree, actualType) => q"_root_.shapeless.Strict.apply[$actualType]($tree)",
      q"null.asInstanceOf[_root_.shapeless.Strict[Nothing]]",
      None
    )

  def mkGlobalLazyImpl[I](implicit iTag: WeakTypeTag[I]): Tree =
    mkImpl[I](
      (tree, actualType) => q"_root_.shapeless.Lazy.Global.apply[$actualType]($tree)",
      q"null.asInstanceOf[_root_.shapeless.Lazy.Global[Nothing]]",
      Some("")
    )

  def mkGlobalStrictImpl[I](implicit iTag: WeakTypeTag[I]): Tree =
    mkImpl[I](
      (tree, actualType) => q"_root_.shapeless.Strict.Global.apply[$actualType]($tree)",
      q"null.asInstanceOf[_root_.shapeless.Strict.Global[Nothing]]",
      Some("")
    )

  def mkImpl[I](mkInst: (c.Tree, c.Type) => c.Tree, nullInst: => c.Tree, cacheOpt: Option[String])(implicit iTag: WeakTypeTag[I]): Tree = {
    (c.openImplicits.headOption, iTag.tpe.dealias) match {
      case (Some(ImplicitCandidate(_, _, TypeRef(_, _, List(tpe)), _)), _) =>
        LazyMacros.deriveInstance(c)(tpe.map(_.dealias), mkInst, cacheOpt)
      case (None, tpe) if tpe.typeSymbol.isParameter =>       // Workaround for presentation compiler
        nullInst
      case (None, tpe) =>                                     // Non-implicit invocation
        LazyMacros.deriveInstance(c)(tpe, mkInst, cacheOpt)
      case _ =>
        c.abort(c.enclosingPosition, s"Bad Lazy materialization ${c.openImplicits.head}")
    }
  }
}

object LazyMacros {
  var caches = Map.empty[String, List[(Any, Any)]]

  var dcRef: Option[DerivationContext] = None

  def deriveInstance(c: whitebox.Context)(tpe: c.Type, mkInst: (c.Tree, c.Type) => c.Tree, cacheOpt: Option[String]): c.Tree = {
    val cached =
      if (dcRef.isEmpty)
        cacheOpt.flatMap(caches.get).flatMap { cache =>
          cache.collectFirst {
            case (tpe0, v) if tpe0.asInstanceOf[c.Type] =:= tpe =>
              v.asInstanceOf[c.Tree]
          }
        }
      else
        None

    def derive: c.Tree = {
      val (dc, root) =
        dcRef match {
          case None =>
            val dc = DerivationContext(c)
            dcRef = Some(dc)
            (dc, true)
          case Some(dc) =>
            (DerivationContext.establish(dc, c), false)
        }

      if (root)
        // Sometimes corrupted, and slows things too
        c.universe.asInstanceOf[scala.tools.nsc.Global].analyzer.resetImplicits()

      try {
        dc.State.deriveInstance(tpe, root, mkInst)
      } finally {
        if(root) dcRef = None
      }
    }

    cached.getOrElse{
      val res = derive
      if (dcRef.isEmpty)
        for (cache <- cacheOpt)
          LazyMacros.caches += cache -> ((tpe, res) :: LazyMacros.caches.getOrElse(cache, Nil))
      res
    }
  }
}

object DerivationContext {
  type Aux[C0] = DerivationContext { type C = C0 }

  def apply(c0: whitebox.Context): Aux[c0.type] =
    new DerivationContext {
      type C = c0.type
      val c: C = c0
    }

  def establish(dc: DerivationContext, c0: whitebox.Context): Aux[c0.type] =
    dc.asInstanceOf[DerivationContext { type C = c0.type }]
}

trait LazyExtension {
  type Ctx <: DerivationContext
  val ctx: Ctx

  import ctx._
  import c.universe._

  /** Uniquely identifies a `LazyExtension`. Only one extension with a given id is taken into account, during a
    * Lazy / Strict implicit search. */
  def id: String

  /** State of this extension, kept and provided back during Lazy / Strict implicit search. */
  type ThisState

  /** Initial state of this extension, upon initialization. */
  def initialState: ThisState

  /**
   * Called during a `Lazy` or `Strict` implicit materialization.
   *
   * If this extension handles @tpe, it should return either `Some(Right(...))` upon success, or
   * `Some(Left(...))` upon failure. The latter will make the current implicit search fail.
   *
   * If it does not handle this type, it should return `None`. Materialization will then go on with other
   * extensions, or standard implicit search.
   */
  def derive(
    state: State,
    extState: ThisState,
    update: (State, ThisState) => State )(
    tpe: Type
  ): Option[Either[String, (State, Instance)]]
}

/**
 * Lazy extension companions should extend this trait, and return a new `LazyExtension` instance via
 * `instantiate`.
 *
 * These companions typically provide a materializer method like
 * {{{
 *   implicit def init[H]: Wrapper[T] = macro initImpl
 * }}},
 * where `Wrapper` is the wrapper type that this extension handles, and `initImpl` is provided by
 * the `LazyExtensionCompanion` trait. This initializes the extension upon first use during
 * a `Lazy` / `Strict` implicit search.
 */
trait LazyExtensionCompanion {
  def instantiate(ctx0: DerivationContext): LazyExtension { type Ctx = ctx0.type }

  def initImpl(c: whitebox.Context): Nothing = {
    val ctx = LazyMacros.dcRef.getOrElse(
      c.abort(c.enclosingPosition, "")
    )

    val extension = instantiate(ctx)
    ctx.State.addExtension(extension)

    c.abort(c.enclosingPosition, s"Added extension ${extension.id}")
  }
}

trait LazyDefinitions {
  type C <: whitebox.Context
  val c: C

  import c.universe._

  case class Instance(
    instTpe: Type,
    name: TermName,
    symbol: Symbol,
    inst: Option[Tree],
    actualTpe: Type,
    dependsOn: List[Type]
  ) {
    def ident = Ident(symbol)
  }

  object Instance {
    def apply(instTpe: Type) = {
      val nme = TermName(c.freshName("inst"))
      val sym = c.internal.setInfo(c.internal.newTermSymbol(NoSymbol, nme), instTpe)

      new Instance(instTpe, nme, sym, None, instTpe, Nil)
    }
  }

  class TypeWrapper(val tpe: Type) {
    override def equals(other: Any): Boolean =
      other match {
        case TypeWrapper(tpe0) => tpe =:= tpe0
        case _ => false
      }
    override def toString = tpe.toString
  }

  object TypeWrapper {
    def apply(tpe: Type) = new TypeWrapper(tpe)
    def unapply(tw: TypeWrapper): Option[Type] = Some(tw.tpe)
  }


  case class ExtensionWithState[S <: DerivationContext, T](
    extension: LazyExtension { type Ctx = S; type ThisState = T },
    state: T
  ) {
    import extension.ctx

    def derive(
      state0: ctx.State,
      update: (ctx.State, ExtensionWithState[S, T]) => ctx.State )(
      tpe: ctx.c.Type
    ): Option[Either[String, (ctx.State, ctx.Instance)]] =
      extension.derive(state0, state, (ctx, t) => update(ctx, copy(state = t)))(tpe)
  }

  object ExtensionWithState {
    def apply(extension: LazyExtension): ExtensionWithState[extension.Ctx, extension.ThisState] =
      ExtensionWithState(extension, extension.initialState)
  }

}

trait DerivationContext extends shapeless.CaseClassMacros with LazyDefinitions { ctx =>
  type C <: whitebox.Context
  val c: C

  import c.universe._

  object State {
    final val ctx0: ctx.type = ctx
    val empty = State("", ListMap.empty, Nil, Nil)

    private var current = Option.empty[State]
    private var addExtensions = List.empty[ExtensionWithState[ctx.type, _]]

    def addExtension(extension: LazyExtension { type Ctx = ctx0.type }): Unit = {
      addExtensions = ExtensionWithState(extension) :: addExtensions
    }

    def takeNewExtensions(): List[ExtensionWithState[ctx.type, _]] = {
      val addExtensions0 = addExtensions
      addExtensions = Nil
      addExtensions0
    }

    def resolveInstance(state: State)(tpe: Type): Option[(State, Tree)] = {
      val former = State.current
      State.current = Some(state)
      val (state0, tree) =
        try {
          val tree = c.inferImplicitValue(tpe, silent = true)
          (State.current.get, tree)
        } finally {
          State.current = former
        }

        if (tree == EmptyTree || addExtensions.nonEmpty) None
        else Some((state0, tree))
      }

    def deriveInstance(instTpe0: Type, root: Boolean, mkInst: (Tree, Type) => Tree): Tree = {
      if (root) {
        assert(current.isEmpty)
        val open = c.openImplicits
        val name = if (open.length > 1) open(1).sym.name.toTermName.toString else "lazy"
        current = Some(empty.copy(name = "anon$"+name))
      }

      ctx.derive(current.get)(instTpe0) match {
        case Right((state, inst)) =>
          val (tree, actualType) = if (root) mkInstances(state)(instTpe0) else (inst.ident, inst.actualTpe)
          current = if (root) None else Some(state)
          if (root) {
            val valNme = TermName(c.freshName)
            q"""
            val $valNme: $actualType = $tree
            ${mkInst(q"$valNme", actualType)}
            """
          } else
            mkInst(tree, actualType)
        case Left(err) =>
          abort(err)
      }
    }
  }

  case class State(
    name: String,
    dict: ListMap[TypeWrapper, Instance],
    open: List[Instance],
    extensions: List[ExtensionWithState[ctx.type, _]]
  ) {
    def addDependency(tpe: Type): State = {
      import scala.::
      val open0 = open match {
        case Nil => Nil
        case h :: t => h.copy(dependsOn = if (h.instTpe =:= tpe || h.dependsOn.exists(_ =:= tpe)) h.dependsOn else tpe :: h.dependsOn) :: t
      }
      copy(open = open0)
    }

    private def update(inst: Instance): State =
      copy(dict = dict.updated(TypeWrapper(inst.instTpe), inst))

    def openInst(tpe: Type): (State, Instance) = {
      val inst = Instance(tpe)
      val state0 = addDependency(tpe)
      (state0.copy(open = inst :: state0.open).update(inst), inst)
    }

    def closeInst(tpe: Type, tree: Tree, actualTpe: Type): (State, Instance) = {
      assert(open.nonEmpty)
      assert(open.head.instTpe =:= tpe)
      val instance = open.head
      val sym = c.internal.setInfo(instance.symbol, actualTpe)
      val instance0 = instance.copy(inst = Some(tree), actualTpe = actualTpe, symbol = sym)
      (copy(open = open.tail).update(instance0), instance0)
    }

    def lookup(instTpe: Type): Either[State, (State, Instance)] =
      dict.get(TypeWrapper(instTpe)) match {
        case Some(i) => Right((addDependency(instTpe), i))
        case None => Left(openInst(instTpe)._1)
      }


    def dependsOn(tpe: Type): List[Instance] = {
      import scala.::
      def helper(tpes: List[List[Type]], acc: List[Instance]): List[Instance] =
        tpes match {
          case Nil => acc
          case Nil :: t =>
            helper(t, acc)
          case (h :: t0) :: t =>
            if (acc.exists(_.instTpe =:= h))
              helper(t0 :: t, acc)
            else {
              val inst = dict(TypeWrapper(h))
              helper(inst.dependsOn :: t0 :: t, inst :: acc)
            }
        }

      helper(List(List(tpe)), Nil)
    }
  }

  def stripRefinements(tpe: Type): Option[Type] =
    tpe match {
      case RefinedType(parents, decls) => Some(parents.head)
      case _ => None
    }

  def resolve(state: State)(inst: Instance): Option[(State, Instance)] =
    resolve0(state)(inst.instTpe)
      .filter{case (_, tree, _) => !tree.equalsStructure(inst.ident) }
      .map {case (state0, extInst, actualTpe) =>
        state0.closeInst(inst.instTpe, extInst, actualTpe)
      }

  def resolve0(state: State)(tpe: Type): Option[(State, Tree, Type)] = {
    val extInstOpt =
      State.resolveInstance(state)(tpe)
        .orElse(
          stripRefinements(tpe).flatMap(State.resolveInstance(state))
        )

    extInstOpt.map {case (state0, extInst) =>
      (state0, extInst, extInst.tpe.finalResultType)
    }
  }

  def derive(state: State)(tpe: Type): Either[String, (State, Instance)] = {
    val fromExtensions: Option[Either[String, (State, Instance)]] =
      state.extensions.zipWithIndex.foldRight(Option.empty[Either[String, (State, Instance)]]) {
        case (_, acc @ Some(_)) => acc
        case ((ext, idx), None) =>
          def update(state: State, withState: ExtensionWithState[ctx.type, _]) =
            state.copy(extensions = state.extensions.updated(idx, withState))

          ext.derive(state, update)(tpe)
      }

    val result: Either[String, (State, Instance)] =
      fromExtensions.getOrElse {
        state.lookup(tpe).left.flatMap { state0 =>
          val inst = state0.dict(TypeWrapper(tpe))
          resolve(state0)(inst)
            .toRight(s"Unable to derive $tpe")
        }
      }

    // Check for newly added extensions, and re-derive with them.
    lazy val current = state.extensions.map(_.extension.id).toSet
    val newExtensions0 = State.takeNewExtensions().filter(ext => !current(ext.extension.id))
    if (newExtensions0.nonEmpty)
      derive(state.copy(extensions = newExtensions0 ::: state.extensions))(tpe)
    else
      result
  }

  // Workaround for https://issues.scala-lang.org/browse/SI-5465
  class StripUnApplyNodes extends Transformer {
    val global = c.universe.asInstanceOf[scala.tools.nsc.Global]
    import global.nme

    override def transform(tree: Tree): Tree = {
      super.transform {
        tree match {
          case UnApply(Apply(Select(qual, nme.unapply | nme.unapplySeq), List(Ident(nme.SELECTOR_DUMMY))), args) =>
            Apply(transform(qual), transformTrees(args))
          case t => t
        }
      }
    }
  }

  def mkInstances(state: State)(primaryTpe: Type): (Tree, Type) = {
    val instances = state.dict.values.toList
    val (from, to) = instances.map { d => (d.symbol, NoSymbol) }.unzip

    def clean(inst: Tree) = {
      val cleanInst = c.untypecheck(c.internal.substituteSymbols(inst, from, to))
      new StripUnApplyNodes().transform(cleanInst)
    }

    if (instances.length == 1) {
      val instance = instances.head
      import instance._
      inst match {
        case Some(inst) =>
          val cleanInst = clean(inst)
          (q"$cleanInst.asInstanceOf[$actualTpe]", actualTpe)
        case None =>
          abort(s"Uninitialized $instTpe lazy implicit")
      }
    } else {
      val instTrees =
        instances.map { instance =>
          import instance._
          inst match {
            case Some(inst) =>
              val cleanInst = clean(inst)
              q"""lazy val $name: $actualTpe = $cleanInst.asInstanceOf[$actualTpe]"""
            case None =>
              abort(s"Uninitialized $instTpe lazy implicit")
          }
        }

      val primaryInstance = state.lookup(primaryTpe).right.get._2
      val primaryNme = primaryInstance.name
      val clsName = TypeName(c.freshName(state.name))

      val tree =
        q"""
          final class $clsName extends _root_.scala.Serializable {
            ..$instTrees
          }
          (new $clsName).$primaryNme
         """
      val actualType = primaryInstance.actualTpe

      (tree, actualType)
    }
  }
}
