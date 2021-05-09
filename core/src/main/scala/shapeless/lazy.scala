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

package shapeless

import scala.language.experimental.macros

import scala.annotation.implicitNotFound
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
 *   implicitly[TC[ListCC]] // fails with: diverging implicit expansion for type TC[ListCC]
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
 *     implicit def intTC: TC[Int] = ???
 *     implicit def stringTC: TC[String] = ???
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
 *   implicitly[TC[ListCC]]
 * }}}
 *
 * When looking for an implicit `Lazy[TC[T]]`, the `Lazy.mkLazy` macro will itself trigger the implicit search
 * for a `TC[T]`. If this search itself triggers searches for types wrapped in `Lazy`, these will be done
 * only once, their result put in a `lazy val`, and a reference to this `lazy val` will be returned as the corresponding
 * value. It will then wrap all the resulting values together, and return a reference to the first one.
 *
 * E.g. with the above example definitions, when looking up for an implicit `TC[ListCC]`, the returned tree roughly looks
 * like
 * {{{
 *   TC.genericTC(
 *     Generic[ListCC], // actually, the tree returned by Generic.materialize, not written here for the sake of brevity
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
@implicitNotFound("could not find Lazy implicit value of type ${T}")
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

  @implicitNotFound("could not find Lazy implicit values for all of the types enumerated in ${T}")
  class Values[T <: HList](val values: T) extends Serializable
  object Values {
    implicit val hnilValues: Values[HNil] = new Values(HNil)
    implicit def hconsValues[H, T <: HList](implicit lh: Lazy[H], t: Values[T]): Values[H :: T] =
      new Values(lh.value :: t.values)
  }

  def values[T <: HList](implicit lv: Lazy[Values[T]]): T = lv.value.values

  implicit def mkLazy[I]: Lazy[I] = macro LazyMacros.mkLazyImpl[I]
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
}

trait OpenImplicitMacros {
  val c: whitebox.Context

  import c.universe._

  def openImplicitTpe: Option[Type] =
    c.openImplicits.headOption.map(_.pt)

  def openImplicitTpeParam: Option[Type] =
    openImplicitTpe.map {
      case TypeRef(_, _, List(tpe)) =>
        tpe.dealias
      case other =>
        c.abort(c.enclosingPosition, s"Bad materialization: $other")
    }

  def secondOpenImplicitTpe: Option[Type] =
    c.openImplicits match {
      case (List(_, second, _ @ _*)) =>
        Some(second.pt)
      case _ => None
    }
}

class LazyMacros(val c: whitebox.Context) extends CaseClassMacros with OpenImplicitMacros with LowPriorityTypes {
  import c.universe._
  import c.internal._
  import decorators._

  def mkLazyImpl[I](implicit iTag: WeakTypeTag[I]): Tree =
    mkImpl[I](
      (tree, actualType) => q"_root_.shapeless.Lazy.apply[$actualType]($tree)",
      q"null.asInstanceOf[_root_.shapeless.Lazy[_root_.scala.Nothing]]"
    )

  def mkStrictImpl[I](implicit iTag: WeakTypeTag[I]): Tree =
    mkImpl[I](
      (tree, actualType) => q"_root_.shapeless.Strict.apply[$actualType]($tree)",
      q"null.asInstanceOf[_root_.shapeless.Strict[_root_.scala.Nothing]]"
    )

  def mkImpl[I](mkInst: (Tree, Type) => Tree, nullInst: => Tree)(implicit iTag: WeakTypeTag[I]): Tree = {
    openImplicitTpeParam match {
      case Some(tpe) => LazyMacros.deriveInstance(this)(tpe, mkInst)
      case None =>
        val tpe = iTag.tpe.dealias
        if (tpe.typeSymbol.isParameter)
          nullInst
        else
          LazyMacros.deriveInstance(this)(tpe, mkInst)
    }
  }

  def setAnnotation(msg: String): Unit = {
    val tree0 =
      c.typecheck(
        q"""
          new _root_.scala.annotation.implicitNotFound("dummy")
        """,
        silent = false
      )

    class SubstMessage extends Transformer {
      val global = c.universe.asInstanceOf[scala.tools.nsc.Global]

      override def transform(tree: Tree): Tree = {
        super.transform {
          tree match {
            case Literal(Constant("dummy")) => Literal(Constant(msg))
            case t => t
          }
        }
      }
    }

    val tree = new SubstMessage().transform(tree0)

    symbolOf[Lazy[Any]].setAnnotations(Annotation(tree))
  }

  def resetAnnotation: Unit =
    setAnnotation("could not find Lazy implicit value of type ${T}")

  trait LazyDefinitions {
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
  }

  class DerivationContext extends LazyDefinitions {
    object State {
      val empty = State("", ListMap.empty, Nil, Nil)

      private var current = Option.empty[State]

      def resolveInstance(state: State)(tpe: Type): Option[(State, Tree)] = {
        val former = State.current
        State.current = Some(state)
        val (state0, tree) =
          try {
            val tree = c.inferImplicitValue(tpe, silent = true)
            if(tree.isEmpty) {
              tpe.typeSymbol.annotations.
                find(_.tree.tpe =:= typeOf[_root_.scala.annotation.implicitNotFound]).foreach { _ =>
                  setAnnotation(implicitNotFoundMessage(c)(tpe))
                }
            }
            (State.current.get, tree)
          } finally {
            State.current = former
          }

          if (tree == EmptyTree) None
          else Some((state0, tree))
        }

      def deriveInstance(instTpe0: Type, root: Boolean, mkInst: (Tree, Type) => Tree): Tree = {
        if (root) {
          assert(current.isEmpty)
          val open = c.openImplicits
          val name = if (open.length > 1) open(1).sym.name.toTermName.toString else "lazy"
          current = Some(empty.copy(name = "anon$"+name))
        }

        derive(current.get)(instTpe0) match {
          case Right((state, inst)) =>
            val (tree, actualType) = if (root) mkInstances(state)(instTpe0) else (inst.ident, inst.actualTpe)
            current = if (root) None else Some(state)
            if (root) {
              val valNme = TermName(c.freshName("inst"))
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
      // Types whose derivation must fail no matter what
      prevent: List[TypeWrapper]
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


    def deriveLowPriority(
      state0: State,
      instTpe: Type
    ): Option[Either[String, (State, Instance)]] = {

      def helper(
        state: State,
        wrappedTpe: Type,
        innerTpe: Type,
        ignoring: String
      ): (State, Instance) = {

        val tmpState = state.copy(prevent = state.prevent :+ TypeWrapper(wrappedTpe))

        val existingInstOpt = derive(tmpState)(innerTpe).toOption.flatMap {
          case (state2, inst) =>
            if (inst.inst.isEmpty)
              resolve0(state2)(innerTpe).map { case (_, tree, _) => tree }
            else
              Some(inst.inst.get)
        }

        val existingInstAvailable = existingInstOpt.exists { actualTree =>
          def ignored = actualTree match {
            case TypeApply(method, other) => method.toString().endsWith(ignoring)
            case _ => false
          }

          ignoring.isEmpty || !ignored
        }

        if (existingInstAvailable)
          c.abort(c.enclosingPosition, s"$innerTpe available elsewhere")

        val lowTpe =
          if (ignoring.isEmpty)
            appliedType(lowPriorityForTpe, List(innerTpe))
          else
            appliedType(lowPriorityForIgnoringTpe, List(internal.constantType(Constant(ignoring)), innerTpe))

        val low = q"null: $lowTpe"

        state.closeInst(wrappedTpe, low, lowTpe)
      }

      if (state0.prevent.contains(TypeWrapper(instTpe)))
        Some(Left(s"Not deriving $instTpe"))
      else
        instTpe match {
          case LowPriorityFor(ignored, tpe) =>
            val res = state0.lookup(instTpe) match {
              case Left(state) => helper(state, instTpe, tpe, ignored)
              case Right(res) => res
            }

            Some(Right(res))

          case _ => None
        }
    }

    def derive(state: State)(tpe: Type): Either[String, (State, Instance)] = {
      deriveLowPriority(state, tpe).getOrElse {
        state.lookup(tpe).swap.flatMap { state0 =>
          val inst = state0.dict(TypeWrapper(tpe))
          resolve(state0)(inst).toLeft(s"Unable to derive $tpe")
        }.swap
      }
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
            case UnApply(Apply(TypeApply(Select(qual, nme.unapply | nme.unapplySeq), _), List(Ident(nme.SELECTOR_DUMMY))), args) =>
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

        val primaryInstance = (state.lookup(primaryTpe): @unchecked) match {
          case Right((_, pi)) => pi
        }
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
}

object LazyMacros {
  def dcRef(lm: LazyMacros): Option[LazyMacros#DerivationContext] = {
    // N.B. openMacros/enclosingMacros annoyingly include macros which are not enclosing this macro at all,
    // but simply happen to be expanding further up on the same compiler stack (and the compiler stack doesn't
    // necessarily correspond to a single path through the AST - it can jump to other trees during typing), so
    // we need to stop once the position of the open macros no longer matches ours
    lm.c.openMacros.takeWhile(_.enclosingPosition == lm.c.enclosingPosition)
      // use the first enclosing DerivationContext we find (if any)
      .find(c => c.internal.attachments(c.macroApplication).contains[lm.DerivationContext])
      .flatMap(c => c.internal.attachments(c.macroApplication).get[lm.DerivationContext])
  }

  def deriveInstance(lm: LazyMacros)(tpe: lm.c.Type, mkInst: (lm.c.Tree, lm.c.Type) => lm.c.Tree): lm.c.Tree = {
    val (dc, root) =
      dcRef(lm) match {
        case None =>
          lm.resetAnnotation
          val dc = new lm.DerivationContext
          lm.c.internal.updateAttachment(lm.c.macroApplication, dc)
          (dc, true)
        case Some(dc) =>
          (dc.asInstanceOf[lm.DerivationContext], false)
      }

    if (root)
      // Sometimes corrupted, and slows things too
      lm.c.universe.asInstanceOf[scala.tools.nsc.Global].analyzer.resetImplicits()

    try {
      dc.State.deriveInstance(tpe, root, mkInst)
    } finally {
      if(root) {
        lm.c.internal.removeAttachment[lm.DerivationContext](lm.c.macroApplication)
      }
    }
  }
}
