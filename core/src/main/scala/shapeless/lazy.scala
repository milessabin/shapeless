/*
 * Copyright (c) 2013-4 Miles Sabin
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

trait Lazy[+T] {
  val value: T

  def map[U](f: T => U): Lazy[U] = Lazy { f(value) }
  def flatMap[U](f: T => Lazy[U]): Lazy[U] = Lazy { f(value).value }
}

object Lazy {
  implicit def apply[T](t: => T): Lazy[T] = new Lazy[T] {
    lazy val value = t
  }

  def unapply[T](lt: Lazy[T]): Option[T] = Some(lt.value)

  implicit def mkLazy[I]: Lazy[I] = macro LazyMacros.mkLazyImpl
}

class LazyMacros(val c: whitebox.Context) {
  import c.universe._
  import c.ImplicitCandidate

  def mkLazyImpl: Tree = {
    val tpe = c.openImplicits.head match {
      case ImplicitCandidate(_, _, TypeRef(_, _, List(tpe)), _) => tpe
      case _ =>
        c.abort(c.enclosingPosition, s"Bad Lazy materialization $c.openImplicits.head")
    }
    val (tree, actualType) = LazyMacros.deriveInstance(c)(tpe)
    q"_root_.shapeless.Lazy[$actualType]($tree)"
  }
}

object LazyMacros {
  var dcRef: Option[DerivationContext] = None

  def deriveInstance(c: whitebox.Context)(tpe: c.Type): (c.Tree, c.Type) = {
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

  def lowPriority(inst: Tree, tc: Type): Boolean = {
    val ownerSym = inst.symbol.owner
    ownerSym.isType && {
      val ownerTpe = ownerSym.asType.toType
      val companionTpe = companionRef(tc).tpe
      companionTpe <:< ownerTpe && !(ownerTpe <:< companionTpe)
    }
  }

  def isLoop(instTree: Tree): Boolean = {
    val sym = instTree.symbol.asTerm
    sym.isVal || sym.isGetter
  }

  def deriveInstance(instTpe: Type, root: Boolean): (Tree, Type) = {
    val instTree =
      dict.get(TypeWrapper(instTpe)) match {
        case Some(d) => (d.ident, d.actualTpe)
        case _ =>
          val instance0 = add(Instance(instTpe))
          val extInst = c.inferImplicitValue(instTpe, silent = true)
          if(extInst == EmptyTree || extInst.equalsStructure(instance0.ident)) {
            remove(instance0)
            abort(s"Unable to derive $instTpe")
          }

          val instance1 = dict(TypeWrapper(instTpe))
          val actualTpe = extInst.tpe.finalResultType

          val sym = c.internal.setInfo(instance1.symbol, actualTpe)
          val tree = add(instance1.copy(inst = extInst, actualTpe = actualTpe, symbol = sym)).ident
          (tree, actualTpe)
      }

    if(root) mkInstances(instTpe) else instTree
  }

  def mkInstances(primaryTpe: Type): (Tree, Type) = {
    val instances = dict.values.toList

    val instTrees: List[Tree] =
      instances.map { instance =>
        import instance._
        q"""lazy val $name: $actualTpe = $inst.asInstanceOf[$actualTpe]"""
      }

    val objName = TermName(c.freshName())
    val obj =
      q"""
        object $objName {
          ..$instTrees
        }
      """

    val instance = dict(TypeWrapper(primaryTpe))
    val nme = instance.name
    val actualType = instance.actualTpe

    val (from, to) = instances.map { d => (d.symbol, NoSymbol) }.unzip
    val cleanObj = c.untypecheck(c.internal.substituteSymbols(obj, from, to))

    val tree =
      q"""
        $cleanObj
        $objName.$nme
       """

    (tree, actualType)
  }
}
