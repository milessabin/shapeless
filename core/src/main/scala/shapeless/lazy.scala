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
}

object lazily {
  def apply[T](implicit lv: Lazy[T]): T = lv.value
}

class LazyMacros(val c: whitebox.Context) {
  import c.universe._
  import c.ImplicitCandidate

  def mkLazyImpl[I](implicit iTag: WeakTypeTag[I]): Tree = {
    (c.openImplicits.headOption, iTag.tpe.dealias) match {
      case (Some(ImplicitCandidate(_, _, TypeRef(_, _, List(tpe)), _)), _) =>
        LazyMacros.deriveInstance(c)(tpe.map(_.dealias))
      case (None, tpe) if tpe.typeSymbol.isParameter =>       // Workaround for presentation compiler
        q"null.asInstanceOf[_root_.shapeless.Lazy[Nothing]]"
      case (None, tpe) =>                                     // Non-implicit invocation
        LazyMacros.deriveInstance(c)(tpe)
      case _ =>
        c.abort(c.enclosingPosition, s"Bad Lazy materialization $c.openImplicits.head")
    }
  }
}

object LazyMacros {
  var dcRef: Option[DerivationContext] = None

  def deriveInstance(c: whitebox.Context)(tpe: c.Type): c.Tree = {
    val (dc, root) =
      dcRef match {
        case None =>
          val dc = DerivationContext(c)
          dcRef = Some(dc)
          (dc, true)
        case Some(dc) =>
          (DerivationContext.establish(dc, c), false)
      }

    try {
      dc.deriveInstance(tpe, root)
    } finally {
      if(root) dcRef = None
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

trait DerivationContext extends CaseClassMacros {
  type C <: whitebox.Context
  val c: C

  import c.universe._

  case class Instance(
    instTpe: Type,
    name: TermName,
    symbol: Symbol,
    inst: Tree,
    actualTpe: Type
  ) {
    def ident = Ident(symbol)
  }

  object Instance {
    def apply(instTpe: Type) = {
      val nme = TermName(c.freshName("inst"))
      val sym = c.internal.setInfo(c.internal.newTermSymbol(NoSymbol, nme), instTpe)
      val tree = Ident(sym)

      new Instance(instTpe, nme, sym, tree, instTpe)
    }
  }

  class TypeWrapper(val tpe: Type) {
    override def equals(other: Any): Boolean =
      other match {
        case TypeWrapper(tpe0) => tpe =:= tpe0
        case _ => false
      }
  }

  object TypeWrapper {
    def apply(tpe: Type) = new TypeWrapper(tpe)
    def unapply(tw: TypeWrapper): Option[Type] = Some(tw.tpe)
  }

  var dict: Map[TypeWrapper, Instance] = ListMap.empty

  def add(d: Instance): Instance = {
    dict = dict.updated(TypeWrapper(d.instTpe), d)
    d
  }

  def remove(d: Instance): Unit = {
    dict = dict-TypeWrapper(d.instTpe)
  }

  def deriveInstance(instTpe: Type, root: Boolean): Tree = {
    val instTree =
      dict.get(TypeWrapper(instTpe)) match {
        case Some(d) => (d.ident, d.actualTpe)
        case _ =>
          val instance0 = add(Instance(instTpe))

          def resolveInstance(tpe: Type): Option[Tree] = {
            val tree = c.inferImplicitValue(tpe, silent = true)
            if(tree == EmptyTree || tree.equalsStructure(instance0.ident)) None
            else Some(tree)
          }

          def stripRefinements(tpe: Type): Option[Type] =
            tpe match {
              case RefinedType(parents, decls) => Some(parents.head)
              case _ => None
            }

          val extInst =
            resolveInstance(instTpe).orElse(
              stripRefinements(instTpe).flatMap(resolveInstance)
            ).getOrElse {
              remove(instance0)
              abort(s"Unable to derive $instTpe")
            }

          val instance1 = dict(TypeWrapper(instTpe))
          val actualTpe = extInst.tpe.finalResultType

          val sym = c.internal.setInfo(instance1.symbol, actualTpe)
          val tree = add(instance1.copy(inst = extInst, actualTpe = actualTpe, symbol = sym)).ident
          (tree, actualTpe)
      }

    if(root) {
      val (tree, actualType) = mkInstances(instTpe)
      val valNme = TermName(c.freshName)
      q"""
      val $valNme: $actualType = $tree
      _root_.shapeless.Lazy[$actualType]($valNme)
      """
    } else {
      val (tree, actualType) = instTree
      q"""
      _root_.shapeless.Lazy[$actualType]($tree)
      """
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
          case t => t
        }
      }
    }
  }

  def mkInstances(primaryTpe: Type): (Tree, Type) = {
    val instances = dict.values.toList

    val (from, to) = instances.map { d => (d.symbol, NoSymbol) }.unzip

    val instTrees: List[Tree] =
      instances.map { instance =>
        import instance._
        val cleanInst0 = c.untypecheck(c.internal.substituteSymbols(inst, from, to))
        val cleanInst = new StripUnApplyNodes().transform(cleanInst0)
        q"""lazy val $name: $actualTpe = $cleanInst.asInstanceOf[$actualTpe]"""
      }

    val primaryInstance = dict(TypeWrapper(primaryTpe))
    val primaryNme = primaryInstance.name
    val clsName = TypeName(c.freshName())

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
