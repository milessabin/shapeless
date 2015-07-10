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

trait Lazy[+T] extends Serializable {
  val value: T

  def map[U](f: T => U): Lazy[U] = Lazy { f(value) }
  def flatMap[U](f: T => Lazy[U]): Lazy[U] = Lazy { f(value).value }
}

/**
 * Allow lower priority implicits to be defined without interfering with implicit scopes.
 *
 * When looking for a `Lazy[Priority[T, F]]`,
 * - first try to find an implicit `T`. In case of success, an implicit `Priority.High[T]` is found
 *   as a `Priority[T, F]`.
 * - else try to find an implicit `F` (fallback). In case of success, an implicit `Priority.Low[F]`
 *   is found as a `Priority[T, F]`.
 *
 * Unlike standard implicit prioritization, implicit lookups like
 *     implicit def lookupTc[T](implicit priority: Lazy[Priority[TC[T], Fallback[T]]]): TC[T] = ???
 * are allowed, without this definition hindering the lookup for an implicit `TC[T]` elsewhere.
 *
 * If the lookup of `TC[T]` or `Fallback[T]` in the example requires other implicit instances like
 * `TC[U]`, these can be looked up the same way too (first `TC[U]`, else `Fallback[U]` - no matter
 * whether a `TC[T]` or `Fallback[T]` was found in the first place).
 *
 * The method body in the example above will typically be like
 *     priority.value.fold(identity)(_.makeTC)
 * where `makeTC` will be a method of `Fallback[T]` returning a `TC[T]`.
 */
trait Priority[+T, +F] {
  def fold[U](high: T => U)(low: F => U): U =
    this match {
      case Priority.High(t) => high(t)
      case Priority.Low(d) => low(d)
    }
}

object Priority {
  implicit def apply[T, F](implicit lz: Lazy[Priority[T, F]]): Priority[T, F] = lz.value

  case class High[+T](t: T) extends Priority[T, Nothing]
  case class Low[+A](a: A) extends Priority[Nothing, A]
}

object Lazy {
  implicit def apply[T](t: => T): Lazy[T] = new Lazy[T] {
    lazy val value = t
  }

  def unapply[T](lt: Lazy[T]): Option[T] = Some(lt.value)

  class Values[T <: HList](val values: T)
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
    actualTpe: Type,
    initialized: Boolean
  ) {
    def ident = Ident(symbol)
  }

  object Instance {
    def apply(instTpe: Type) = {
      val nme = TermName(c.freshName("inst"))
      val sym = c.internal.setInfo(c.internal.newTermSymbol(NoSymbol, nme), instTpe)
      val tree = Ident(sym)

      new Instance(instTpe, nme, sym, tree, instTpe, initialized = false)
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

  var dict: Map[TypeWrapper, Instance] = ListMap.empty

  def add(d: Instance): Instance = {
    dict = dict.updated(TypeWrapper(d.instTpe), d)
    d
  }

  def remove(d: Instance): Unit = {
    dict = dict-TypeWrapper(d.instTpe)
  }

  var priorityLookups: Set[TypeWrapper] = Set.empty

  var noUninitialized: Map[TypeWrapper, Int] = Map.empty

  def addNoUninitialized(tpe: Type): Unit =
    noUninitialized = noUninitialized.updated(
      TypeWrapper(tpe),
      noUninitialized.getOrElse(TypeWrapper(tpe), 0) + 1
    )

  def removeNoUninitialized(tpe: Type): Unit = {
    val count = noUninitialized.getOrElse(TypeWrapper(tpe), 0) - 1
    noUninitialized =
      if (count <= 0)
        noUninitialized-TypeWrapper(tpe)
      else
        noUninitialized.updated(TypeWrapper(tpe), count)
  }

  def highPriorityTpe: Type = typeOf[Priority.High[_]].typeConstructor
  def lowPriorityTpe: Type = typeOf[Priority.Low[_]].typeConstructor
  def priorityTpe: Type = typeOf[Priority[_, _]].typeConstructor

  object PriorityTpe {
    def unapply(tpe: Type): Option[(Type, Type)] =
    tpe.dealias match {
      case TypeRef(_, cpdTpe, List(highTpe, lowTpe))
         if cpdTpe.asType.toType.typeConstructor =:= priorityTpe =>
        Some(highTpe, lowTpe)
      case _ =>
        None
    }
  }

  def deriveInstance(instTpe0: Type, root: Boolean): Tree = {
    def lookup(instTpe: Type): Either[Boolean, Instance] =
      dict
        .get(TypeWrapper(instTpe))
        .map{ instance =>
          if (instance.initialized || !noUninitialized.contains(TypeWrapper(instTpe)))
            Right(instance)
          else
            Left(false)
        }
        .getOrElse(Left(true))

    def resolveInstance(tpe: Type): Option[Tree] = {
      val tree = c.inferImplicitValue(tpe, silent = true)
      if(tree == EmptyTree) None
      else Some(tree)
    }

    def stripRefinements(tpe: Type): Option[Type] =
      tpe match {
        case RefinedType(parents, decls) => Some(parents.head)
        case _ => None
      }

    def derive(instTpe: Type): Option[(Tree, Type)] =
      lookup(instTpe) match {
        case Right(d) =>
          Some((d.ident, d.actualTpe))
        case Left(createInstance) =>
          val newInstanceOpt =
            if (createInstance) Some(add(Instance(instTpe)))
            else None

          val extInstOpt =
            resolveInstance(instTpe)
              .filterNot(tree =>
                newInstanceOpt.exists(tree equalsStructure _.ident)
              )
              .orElse(
                stripRefinements(instTpe).flatMap(resolveInstance)
              )

          (newInstanceOpt, extInstOpt) match {
            case (Some(newInstance), None) =>
              remove(newInstanceOpt.get)
              None
            case (Some(newInstance), Some(extInst)) =>
              val instance1 = dict(TypeWrapper(instTpe))
              val actualTpe = extInst.tpe.finalResultType

              val sym = c.internal.setInfo(instance1.symbol, actualTpe)
              val tree = add(instance1.copy(inst = extInst, actualTpe = actualTpe, symbol = sym, initialized = true)).ident
              Some((tree, actualTpe))
            case (None, _) =>
              extInstOpt.map(t => (t, t.tpe.finalResultType))
          }
      }

    def derivePriority(priorityTpe: Type, highInstTpe: Type, lowInstTpe: Type) = {
      val instance = add(Instance(priorityTpe))

      val high = {
        priorityLookups = priorityLookups + TypeWrapper(priorityTpe)
        addNoUninitialized(highInstTpe)

        val high0 =
          derive(highInstTpe)
            .map{case (tree, actualType) =>
              (q"_root_.shapeless.Priority.High[$actualType]($tree)", appliedType(highPriorityTpe, List(actualType)))
            }

        removeNoUninitialized(highInstTpe)
        priorityLookups = priorityLookups - TypeWrapper(priorityTpe)

        high0
      }

      def low =
        derive(lowInstTpe)
          .map{case (tree, actualType) =>
            (q"_root_.shapeless.Priority.Low[$actualType]($tree)", appliedType(lowPriorityTpe, List(actualType)))
          }

      high.orElse(low) match {
        case None =>
          remove(instance)
          None
        case Some((extInst, actualTpe)) =>
          val instance1 = dict(TypeWrapper(priorityTpe))
          val sym = c.internal.setInfo(instance1.symbol, actualTpe)
          val tree = add(instance1.copy(inst = extInst, actualTpe = actualTpe, symbol = sym)).ident
          Some((tree, actualTpe))
      }
    }

    val instTreeOpt =
      instTpe0 match {
        case PriorityTpe(highTpe, lowTpe) =>
          if (priorityLookups(TypeWrapper(instTpe0)))
            abort(s"Not deriving $instTpe0")
          else
            derivePriority(instTpe0, highTpe, lowTpe)

        case _ =>
          derive(instTpe0)
      }

    instTreeOpt match {
      case Some((tree0, actualType0)) =>
        val (tree, actualType) = if (root) mkInstances(instTpe0) else (tree0, actualType0)
        q"_root_.shapeless.Lazy[$actualType]($tree)"
      case None =>
        abort(s"Unable to derive $instTpe0")
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
