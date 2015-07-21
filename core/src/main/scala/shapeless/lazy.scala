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
import scala.language.reflectiveCalls

import scala.collection.immutable.ListMap
import scala.reflect.macros.Context

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

class LazyMacros[C <: Context](val c: C) {
  import c.universe._

  def mkLazyImpl[I](implicit iTag: WeakTypeTag[I]): Tree = {
    (c.openImplicits.headOption, iTag.tpe.normalize) match {
      case (Some((TypeRef(_, _, List(tpe)), _)), _) =>
        LazyMacros.deriveInstance(c)(tpe.map(_.normalize))
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
  def inst(c: Context) = new LazyMacros[c.type](c)

  def mkLazyImpl[I: c.WeakTypeTag](c: Context): c.Expr[Lazy[I]] = {
    import c.universe._

    val lmSym = typeOf[LazyMacros.type].typeSymbol
    lmSym.attachments.all.headOption match {
      case Some(lm) =>
        if(lm == LazyMacros)
          c.Expr[Lazy[I]](inst(c).mkLazyImpl[I])
        else {
          lm.asInstanceOf[
            { def mkLazyImpl(c: Context)(i: c.WeakTypeTag[I]): c.Expr[Lazy[I]] }
          ].mkLazyImpl(c)(weakTypeTag[I])
        }
      case None =>
        lmSym.updateAttachment[LazyMacros.type](this)
        try {
          c.Expr[Lazy[I]](inst(c).mkLazyImpl[I])
        } finally {
            lmSym.removeAttachment[LazyMacros.type]
        }
    }
  }

  var dcRef: Option[DerivationContext] = None

  def deriveInstance(c: Context)(tpe: c.Type): c.Tree = {
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

  def apply(c0: Context): Aux[c0.type] =
    new DerivationContext {
      type C = c0.type
      val c: C = c0
    }

  def establish(dc: DerivationContext, c0: Context): Aux[c0.type] =
    dc.asInstanceOf[DerivationContext { type C = c0.type }]
}

trait DerivationContext extends CaseClassMacros {
  type C <: Context
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
      val nme = newTermName(c.fresh("inst"))
      val sym = NoSymbol.newTermSymbol(nme).setTypeSignature(instTpe)
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
          val actualTpe = extInst.tpe match {
            case NullaryMethodType(restpe) => restpe
            case other => other
          }

          val sym = instance1.symbol.setTypeSignature(actualTpe)
          val tree = add(instance1.copy(inst = extInst, actualTpe = actualTpe, symbol = sym)).ident
          (tree, actualTpe)
      }

    val (tree, actualType) = if(root) mkInstances(instTpe) else instTree
    q"""
    _root_.shapeless.Lazy[$actualType]($tree)
    """
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
        val cleanInst0 = c.resetLocalAttrs(inst.substituteSymbols(from, to))
        val cleanInst = new StripUnApplyNodes().transform(cleanInst0)
        q"""lazy val $name: $actualTpe = $cleanInst.asInstanceOf[$actualTpe]"""
      }

    val primaryInstance = dict(TypeWrapper(primaryTpe))
    val primaryNme = primaryInstance.name
    val clsName = newTypeName(c.fresh())

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
