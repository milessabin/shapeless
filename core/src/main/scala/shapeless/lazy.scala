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

trait Lazy[T] {
  val value: T

  def map[U](f: T => U): Lazy[U] = Lazy { f(value) }
  def flatMap[U](f: T => Lazy[U]): Lazy[U] = Lazy { f(value).value }
}

object Lazy {
  implicit def apply[T](t: => T): Lazy[T] = new Lazy[T] {
    lazy val value = t
  }

  def unapply[T](lt: Lazy[T]): Option[T] = Some(lt.value)

  implicit def mkLazy[I]: Lazy[I] = macro LazyMacros.mkLazyImpl[I]
}

class LazyMacros(val c: whitebox.Context) {
  import c.universe._

  def mkLazyImpl[I](implicit iTag: WeakTypeTag[I]): Tree = {
    val iTpe = weakTypeOf[I].dealias
    val i = LazyMacros.deriveInstance(c)(iTpe)
    q"_root_.shapeless.Lazy[$iTpe]($i)"
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
    inst: Tree
  ) {
    def ident = Ident(symbol)
  }

  object Instance {
    def apply(instTpe: Type) = {
      val nme = TermName(c.freshName("inst"))
      val sym = c.internal.setInfo(c.internal.newTermSymbol(NoSymbol, nme), instTpe)
      val tree = Ident(sym)

      new Instance(instTpe, nme, sym, tree)
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

  def deriveInstance(instTpe: Type, root: Boolean): Tree = {
    val instTree =
      dict.get(TypeWrapper(instTpe)) match {
        case Some(d) => d.ident
        case _ =>
          val instance = add(Instance(instTpe))
          val extInst = c.inferImplicitValue(instTpe, silent = true)
          if(extInst == EmptyTree || extInst.equalsStructure(instance.ident)) {
            remove(instance)
            abort(s"Unable to derive $instTpe")
          }

          add(instance.copy(inst = extInst)).ident
      }

    if(root) mkInstances(instTpe) else instTree
  }

  def mkInstances(primaryTpe: Type): Tree = {
    val instances = dict.values.toList

    val instTrees: List[Tree] =
      instances.map { instance =>
        import instance._
        q"""implicit lazy val $name: $instTpe = $inst"""
      }

    val objName = TermName(c.freshName())
    val obj =
      q"""
        object $objName {
          ..$instTrees
        }
      """

    val (from, to) = instances.map { d => (d.symbol, NoSymbol) }.unzip
    val cleanObj = c.untypecheck(c.internal.substituteSymbols(obj, from, to))

    val nme = dict(TypeWrapper(primaryTpe)).name
    q"""
      $cleanObj
      $objName.$nme
     """
  }
}
