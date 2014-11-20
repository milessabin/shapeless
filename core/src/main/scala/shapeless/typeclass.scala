/*
 * Copyright (c) 2013-14 Lars Hupel, Miles Sabin
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

/**
 * A type class abstracting over the `product` operation of type classes over
 * types of kind `*`, as well as deriving instances using an isomorphism.
 */
trait ProductTypeClass {
  type C[T]
  type Elem[T]

  /**
   * Given a type class instance for `H`, and a type class instance for a
   * product, produce a type class instance for the product prepended with `H`.
   */
  def product[H, T <: HList](h: Elem[H], t: C[T]): C[H :: T]

  /**
   * The empty product.
   */
  def emptyProduct: C[HNil]

  /**
   * Given an isomorphism between `F` and `G`, and a type class instance for `G`,
   * produce a type class instance for `F`.
   */
  def project[F, G](instance: => C[G], to: F => G, from: G => F): C[F]
}

/**
 * A type class abstracting over the `product` operation of type classes over
 * types of kind `*`, as well as deriving instances using an isomorphism.
 * Refines ProductTypeClass with the addition of runtime `String` labels
 * corresponding to the names of the product elements.
 */
trait LabelledProductTypeClass {
  type C[T]
  type Elem[T]

  /**
   * Given a type class instance for `H`, and a type class instance for a
   * product, produce a type class instance for the product prepended with `H`.
   */
  def product[H, T <: HList](name: String, h: Elem[H], t: C[T]): C[H :: T]

  /**
   * The empty product.
   */
  def emptyProduct: C[HNil]

  /**
   * Given an isomorphism between `F` and `G`, and a type class instance for `G`,
   * produce a type class instance for `F`.
   */
  def project[F, G](instance: => C[G], to: F => G, from: G => F): C[F]
}

/**
 * A type class additionally abstracting over the `coproduct` operation of type
 * classes over types of kind `*`.
 */
trait TypeClass extends ProductTypeClass {
  /**
   * Given two type class instances for `L` and `R`, produce a type class
   * instance for the coproduct `L :+: R`.
   */
  def coproduct[L, R <: Coproduct](l: => Elem[L], r: => C[R]): C[L :+: R]

  /**
   * The empty coproduct
   */
  def emptyCoproduct: C[CNil]
}

/**
 * A type class additionally abstracting over the `coproduct` operation of type
 * classes over types of kind `*`.
 *
 * Name hints can be safely ignored.
 */
trait LabelledTypeClass extends LabelledProductTypeClass {
  /**
   * Given two type class instances for `L` and `R`, produce a type class
   * instance for the coproduct `L :+: R`.
   */
  def coproduct[L, R <: Coproduct](name: String, l: => Elem[L], r: => C[R]): C[L :+: R]

  /**
   * The empty coproduct
   */
  def emptyCoproduct: C[CNil]
}


trait TypeClassCompanion {
  trait MkTypeClass {
    class Unpack[I, T, TC](val typeClass: TC)

    def unpack[I, T] = new MkUnpack[I, T]

    class MkUnpack[I, T] {
      def apply[TC](tc: TC): Unpack[I, T, TC] = new Unpack[I, T, TC](tc)
    }
  }

  case class Derive[I](instance: I)
  implicit def mkDerive[I]: Derive[I] = macro TypeClassMacros.mkDeriveImpl[I]
}

trait SimpleTypeClassCompanion[C0[_]] extends TypeClassCompanion {
  trait SimpleTypeClass {
    type C[T] = C0[T]
    type Elem[T] = C0[T]
  }

  val typeClass: SimpleTypeClass

  implicit def apply[T](implicit wct: Derive[C0[T]]): C0[T] = wct.instance
}

class TypeClassMacros(val c: whitebox.Context) {
  import c.universe._

  val typeClassName = TermName("typeClass")
  val mkTypeClassName = TypeName("MkTypeClass")
  val unpackName = TypeName("Unpack")

  def mkDeriveImpl[I](implicit iTag: WeakTypeTag[I]): Tree = {
    val iTpe = weakTypeOf[I]
    val prefix = c.prefix.tree
    val prefixTpe = prefix.tpe
    val mkTypeClassTpe = prefixTpe.member(mkTypeClassName).asType.toTypeIn(prefixTpe)
    val typeClass = prefixTpe.member(typeClassName).asTerm
    val typeClassTpe = typeClass.typeSignatureIn(prefixTpe)

    val (tcInst, tTpe) =
      if(typeClassTpe <:< mkTypeClassTpe) {
        val unpackTcTpe = typeClassTpe.member(unpackName).asType.toTypeIn(typeClassTpe).typeConstructor
        val unpackTpe = appliedType(unpackTcTpe, List(iTpe, WildcardType, WildcardType))
        val unpack = c.inferImplicitValue(unpackTpe)

        if(unpack == EmptyTree)
          c.abort(c.enclosingPosition, s"Unable to resolve typeClass for $iTpe")

        val (tTpe, tcTpe) = unpack.tpe match {
          case TypeRef(_, _, List(_, tTpe, tcTpe)) => (tTpe, tcTpe)
          case NullaryMethodType(TypeRef(_, _, List(_, tTpe, tcTpe))) => (tTpe, tcTpe)
          case _ =>
            c.abort(c.enclosingPosition, s"Unable to unpack typeClass information from ${unpack.tpe}")
        }

        val tcInst = c.internal.setType(q"$unpack.typeClass", tcTpe)
        (tcInst, tTpe)
      } else {
        val tcInst = c.internal.setType(q"$typeClass", typeClassTpe)
        val tTpe = iTpe match {
          case TypeRef(_, _, List(tTpe)) => tTpe
          case NullaryMethodType(TypeRef(_, _, List(tTpe))) => tTpe
          case _ =>
            c.abort(c.enclosingPosition, s"Unable to unpack typeClass information from ${iTpe}")
        }

        (tcInst, tTpe)
      }

    val i = TypeClassMacros.deriveInstance(c)(tcInst, tTpe)
    if(i == EmptyTree) i
    else q"$prefix.Derive[$iTpe]($i)"
  }
}

object TypeClassMacros {
  var dcRef: Option[DerivationContext] = None

  def deriveInstance(c: whitebox.Context)(typeClass: c.Tree, tpe: c.Type): c.Tree = {
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
      dc.deriveInstance(typeClass, tpe, root)
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

  sealed trait DerivationState
  case object New extends DerivationState
  case object Probing extends DerivationState
  case object PropProductElement extends DerivationState
  case object PropCoproductElement extends DerivationState
  case object ResolvedInternal extends DerivationState
  case object ResolvedExternal extends DerivationState

  case class Instance(
    instTpe: Type,
    name: TermName,
    symbol: Symbol,
    typeClass: Tree,
    tc: Type,
    tpe: Type,
    inst: Tree,
    state: DerivationState,
    standalone: Boolean
  ) {
    def probing = state == Probing
    def external = state == ResolvedExternal
    def resolved = state == ResolvedInternal || state == ResolvedExternal
    def propagating = state == PropProductElement || state == PropCoproductElement
    def typeClassTpe = typeClass.tpe
    def elem = typeClassTpe.member(TypeName("Elem")).infoIn(typeClassTpe)
    def product =
      (typeClassTpe <:< typeOf[ProductTypeClass] && !(typeClassTpe <:< typeOf[TypeClass])) ||
      (typeClassTpe <:< typeOf[LabelledProductTypeClass] && !(typeClassTpe <:< typeOf[LabelledTypeClass]))

    def labelled = typeClassTpe <:< typeOf[LabelledTypeClass] || typeClassTpe <:< typeOf[LabelledProductTypeClass]
    def ident = Ident(symbol)
  }

  object Instance {
    def apply(
      instTpe: Type,
      typeClass: Tree,
      tc: Type,
      tpe: Type,
      state: DerivationState,
      standalone: Boolean
    ) = {
      val nme = TermName(c.freshName("inst"))
      val sym = c.internal.setInfo(c.internal.newTermSymbol(NoSymbol, nme), instTpe)
      val tree = Ident(sym)

      new Instance(instTpe, nme, sym, typeClass, tc, tpe, tree, state, standalone)
    }
  }

  var dict: Map[Type, Instance] = Map.empty

  def add(d: Instance): Unit = {
    dict = dict.updated(d.instTpe, d)
  }

  def lowPriority(inst: Tree, tc: Type): Boolean = {
    val ownerSym = inst.symbol.owner
    ownerSym.isType && {
      val ownerTpe = ownerSym.asType.toType
      val companionTpe = companionRef(tc).tpe
      companionTpe <:< ownerTpe && !(ownerTpe <:< companionTpe)
    }
  }

  def extendedFieldsOf(tpe: Type): List[(TermName, Type)] =
    if(tpe <:< typeOf[HList]) hlistElements(tpe).map((termNames.EMPTY, _))
    else fieldsOf(tpe)

  def extendedCtorsOf(tpe: Type): List[Type] =
    if(tpe <:< typeOf[Coproduct]) coproductElements(tpe)
    else ctorsOf(tpe)

  def isExtendedProduct(tpe: Type): Boolean =
    isProduct(tpe) || tpe <:< typeOf[HList]

  def isExtendedCoproduct(tpe: Type): Boolean =
    isCoproduct(tpe) || tpe <:< typeOf[Coproduct]

  def isDerivable(tpe: Type): Boolean =
    isExtendedProduct(tpe) || isExtendedCoproduct(tpe)

  def resolve(instance: Instance): Tree = {
    import instance.{ typeClass, tc, elem, tpe }

    val fromProduct = isExtendedProduct(tpe)

    if(!fromProduct && instance.product)
      abort(s"Cannot derive a ProductTypeClass for non-product $tpe")

    add(instance.copy(state = ResolvedInternal))

    val elements: List[Type] =
      if(fromProduct) extendedFieldsOf(tpe).map(_._2)
      else extendedCtorsOf(tpe)

    if(tc =:= elem) {
      elements.foreach { tpe =>
        val instTpe = appliedType(tc, tpe)
        if(fromProduct && dict.contains(instTpe))
          add(dict(instTpe).copy(standalone = true))
        else {
          val propState = if(fromProduct) PropProductElement else PropCoproductElement
          val instance = Instance(instTpe, typeClass, tc, tpe, propState, fromProduct)
          add(instance)

          val inst = c.inferImplicitValue(instTpe, silent = true)
          if(dict(instTpe).propagating) {
            if(inst != instance.ident && inst != EmptyTree && (!lowPriority(inst, tc) || !isDerivable(tpe)))
              add(instance.copy(inst = inst, state = ResolvedExternal, standalone = true))
            else {
              if(isDerivable(tpe))
                deriveInstance(typeClass, tpe, false)
              if(dict(instTpe).propagating)
                c.abort(c.enclosingPosition, s"No implicit value for $instTpe during derivation.")
            }
          }
        }
      }
    } else {
      elements.foreach { tpe =>
        val instTpe = appliedType(elem, tpe)
        if(dict.contains(instTpe))
          add(dict(instTpe).copy(standalone = true))
        else {
          val inst = c.inferImplicitValue(instTpe, silent = true)
          if(inst == EmptyTree) {
            c.abort(c.enclosingPosition, s"No implicit value for $instTpe during derivation.")
          } else if(!dict.contains(instTpe))
            add(Instance(instTpe, EmptyTree, elem, instTpe, ResolvedExternal, true).copy(inst = inst))
        }
      }
    }

    instance.ident
  }

  def isLoop(instTree: Tree): Boolean = {
    val sym = instTree.symbol.asTerm
    sym.isVal || sym.isGetter
  }

  def deriveInstance(typeClass: Tree, tpe: Type, root: Boolean): Tree = {
    val tc = typeClass.tpe.member(TypeName("C")).infoIn(typeClass.tpe)
    val instTpe = appliedType(tc, tpe)
    val instTree =
      dict.get(instTpe) match {
        case Some(d) if d.probing => EmptyTree
        case Some(d) if d.resolved => d.inst
        case other =>
          val instance =
            other.map(_.copy(state = Probing)).getOrElse(Instance(instTpe, typeClass, tc, tpe, Probing, true))
          add(instance)

          val extInst = c.inferImplicitValue(instTpe, silent = true)
          if(extInst == EmptyTree && isDerivable(tpe))
            resolve(instance)
          else if((lowPriority(extInst, tc) || (root && isLoop(extInst))) && isDerivable(tpe))
            resolve(instance)
          else {
            add(instance.copy(inst = extInst, state = ResolvedExternal, standalone = true))
            instance.ident
          }
      }

    if(root && instTree != EmptyTree) mkInstances(instTpe) else instTree
  }

  def mkInstances(primaryTpe: Type): Tree = {
    val instances = dict.values.toList
    val derived = instances.filter(!_.external)
    val primary = instances.filter(_.standalone)

    val typeClasses: Map[Type, (TermName, Tree)] =
      instances.filter(_.typeClass != EmptyTree).groupBy(_.typeClassTpe).map(_._2.head).map { instance =>
        val typeClassTpe = instance.typeClassTpe
        val typeClassName = TermName(c.freshName("typeClass"))
        val typeClassTree = q"val $typeClassName: $typeClassTpe = ${instance.typeClass}"
        (typeClassTpe, (typeClassName, typeClassTree))
      }.toMap

    val generics: Map[Type, (TermName, Tree)] =
      derived.map(_.tpe).distinct.map { tpe =>
        val genName = TermName(c.freshName("gen"))
        (tpe, (genName, q"val $genName = _root_.shapeless.Generic[$tpe]"))
      }.toMap

    val reprInstances: Map[Type, (TermName, Tree)] = {
      val reprInstanceNames: Map[Type, TermName] =
        derived.map { instance => (instance.instTpe, TermName(c.freshName("repr"))) }.toMap

      def mkReprInstance(instance: Instance): (TermName, Tree) = {
        import instance._

        val reprInstanceName = reprInstanceNames(instTpe)
        val typeClass = typeClasses(instance.typeClassTpe)._1

        val reprTree =
          if(isExtendedProduct(tpe))
            extendedFieldsOf(tpe).
              foldRight(q"$typeClass.emptyProduct": Tree) { case ((nme, tpe), acc) =>
                val elemTpe = appliedType(elem, tpe)
                val inst = dict(elemTpe).symbol

                if(labelled)
                  q"""$typeClass.product(${nme.toString}, $inst, $acc)"""
                else
                  q"""$typeClass.product($inst, $acc)"""
              }
          else
            extendedCtorsOf(tpe).
              foldRight(q"$typeClass.emptyCoproduct": Tree) { case (tpe, acc) =>
                val elemTpe = appliedType(elem, tpe)
                val productInstance = dict(elemTpe)
                val inst =
                  if(productInstance.external)
                    productInstance.ident
                  else {
                    val reprInstanceName = reprInstanceNames(elemTpe)
                    val genName = generics(tpe)._1
                    q"""
                      $typeClass.project(
                        $reprInstanceName,
                        (t: $tpe) => $genName.to(t),
                        (r: $genName.Repr) => $genName.from(r)
                      )
                     """
                  }

                if(labelled)
                  q"""$typeClass.coproduct(${nameOf(tpe).toString}, $inst, $acc)"""
                else
                  q"""$typeClass.coproduct($inst, $acc)"""
              }

        (reprInstanceName, q"""lazy val $reprInstanceName = $reprTree""")
      }

      derived.map { instance => (instance.instTpe, mkReprInstance(instance)) }.toMap
    }

    val primaryInstances: List[Tree] = primary.map { instance =>
      import instance._

      if(external)
        q"""implicit lazy val $name: $instTpe = $inst"""
      else {
        val typeClass = typeClasses(instance.typeClassTpe)._1
        val reprInstanceName = reprInstances(instTpe)._1
        val genName = generics(tpe)._1

        if(product || isExtendedCoproduct(tpe) || !(tc =:= elem))
          q"""implicit lazy val $name: $instTpe =
                $typeClass.project(
                  $reprInstanceName,
                  (t: $tpe) => $genName.to(t),
                  (r: $genName.Repr) => $genName.from(r)
                )
           """
        else {
          val productTree =
            q"""
              $typeClass.project(
                $reprInstanceName,
                (t: $tpe) => $genName.to(t),
                (r: $genName.Repr) => $genName.from(r)
              )
             """

          val coproductTree =
            if(labelled)
              q"""$typeClass.coproduct(${nameOf(tpe).toString}, $productTree, $typeClass.emptyCoproduct)"""
            else
              q"""$typeClass.coproduct($productTree, $typeClass.emptyCoproduct)"""

          q"""implicit lazy val $name: $instTpe =
                $typeClass.project(
                  $coproductTree,
                  (t: $tpe) => Inl(t),
                  (r: $tpe :+: CNil) =>
                    r match {
                      case Inl(p) => p
                      case _ => ???
                    }
                )
           """
        }
      }
    }

    val objName = TermName(c.freshName())
    val obj =
      q"""
        object $objName {
          ..${typeClasses.values.map(_._2)}

          ..${generics.values.map(_._2)}

          ..${reprInstances.values.map(_._2)}

          ..$primaryInstances
        }
      """

    val (from, to) = instances.map { d => (d.symbol, NoSymbol) }.unzip
    val cleanObj = c.untypecheck(c.internal.substituteSymbols(obj, from, to))

    val nme = dict(primaryTpe).name

    q"""
      $cleanObj
      $objName.$nme
     """
  }
}
