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
trait ProductTypeClass[C[_]] {
  /**
   * Given a type class instance for `H`, and a type class instance for a
   * product, produce a type class instance for the product prepended with `H`.
   */
  def product[H, T <: HList](CHead: C[H], CTail: C[T]): C[H :: T]

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

trait ProductTypeClassCompanion[C[_]] {
  object auto {
    implicit def derive[T]: C[T] =
      macro TypeClassMacros.deriveProductInstance[C, T]
  }

  def apply[T]: C[T] =
    macro TypeClassMacros.deriveProductInstance[C, T]
}

/**
 * A type class abstracting over the `product` operation of type classes over
 * types of kind `*`, as well as deriving instances using an isomorphism.
 * Refines ProductTypeClass with the addition of runtime `String` labels
 * corresponding to the names of the product elements.
 */
trait LabelledProductTypeClass[C[_]] {
  /**
   * Given a type class instance for `H`, and a type class instance for a
   * product, produce a type class instance for the product prepended with `H`.
   */
  def product[H, T <: HList](name: String, CHead: C[H], CTail: C[T]): C[H :: T]

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

trait LabelledProductTypeClassCompanion[C[_]] {
  object auto {
    implicit def derive[T]: C[T] =
      macro TypeClassMacros.deriveLabelledProductInstance[C, T]
  }

  def apply[T]: C[T] =
    macro TypeClassMacros.deriveLabelledProductInstance[C, T]
}

/**
 * A type class additionally abstracting over the `coproduct` operation of type
 * classes over types of kind `*`.
 */
trait TypeClass[C[_]] extends ProductTypeClass[C] {
  /**
   * Given two type class instances for `L` and `R`, produce a type class
   * instance for the coproduct `L :+: R`.
   */
  def coproduct[L, R <: Coproduct](CL: => C[L], CR: => C[R]): C[L :+: R]

  /**
   * The empty coproduct
   */
  def emptyCoproduct: C[CNil]
}

trait TypeClassCompanion[C[_]] {
  object auto {
    implicit def derive[T]: C[T] =
      macro TypeClassMacros.deriveInstance[C, T]
  }

  def apply[T]: C[T] =
    macro TypeClassMacros.deriveInstance[C, T]
}

/**
 * A type class additionally abstracting over the `coproduct` operation of type
 * classes over types of kind `*`.
 *
 * Name hints can be safely ignored.
 */
trait LabelledTypeClass[C[_]] extends LabelledProductTypeClass[C] {
  /**
   * Given two type class instances for `L` and `R`, produce a type class
   * instance for the coproduct `L :+: R`.
   */
  def coproduct[L, R <: Coproduct](name: String, CL: => C[L], CR: => C[R]): C[L :+: R]

  /**
   * The empty coproduct
   */
  def emptyCoproduct: C[CNil]
}

trait LabelledTypeClassCompanion[C[_]] {
  object auto {
    implicit def derive[T]: C[T] =
      macro TypeClassMacros.deriveLabelledInstance[C, T]
  }

  def apply[T]: C[T] =
    macro TypeClassMacros.deriveLabelledInstance[C, T]
}

final class DeriveConstructors

object TypeClass {
  implicit val deriveConstructors: DeriveConstructors = new DeriveConstructors()
}

trait TypeClassTCMacros {
  val c: whitebox.Context
  import c.universe._

  def typeClassTpe = typeOf[TypeClass[Option]].typeConstructor
  def labelledTypeClassTpe = typeOf[LabelledTypeClass[Option]].typeConstructor
  def productTypeClassTpe = typeOf[ProductTypeClass[Option]].typeConstructor
  def labelledProductTypeClassTpe = typeOf[LabelledProductTypeClass[Option]].typeConstructor
}

class TypeClassMacros(val c: whitebox.Context) extends TypeClassTCMacros {
  import c.universe._

  def deriveProductInstance[C[_], T](implicit tTag: WeakTypeTag[T], cTag: WeakTypeTag[C[Any]]) =
    deriveInstanceAux(productTypeClassTpe, cTag.tpe.typeConstructor, tTag.tpe)

  def deriveLabelledProductInstance[C[_], T](implicit tTag: WeakTypeTag[T], cTag: WeakTypeTag[C[Any]]) =
    deriveInstanceAux(labelledProductTypeClassTpe, cTag.tpe.typeConstructor, tTag.tpe)

  def deriveInstance[C[_], T](implicit tTag: WeakTypeTag[T], cTag: WeakTypeTag[C[Any]]) =
    deriveInstanceAux(typeClassTpe, cTag.tpe.typeConstructor, tTag.tpe)

  def deriveLabelledInstance[C[_], T](implicit tTag: WeakTypeTag[T], cTag: WeakTypeTag[C[Any]]) =
    deriveInstanceAux(labelledTypeClassTpe, cTag.tpe.typeConstructor, tTag.tpe)

  def deriveInstanceAux(typeClassTpe: Type, tc: Type, tpe: Type): Tree = {
    TypeClassMacros.deriveInstance(c)(typeClassTpe, tc, tpe)
  }
}

object TypeClassMacros {
  var dcRef: Option[DerivationContext] = None

  def deriveInstance(c: whitebox.Context)(typeClassTpe: c.Type, tc: c.Type, tpe: c.Type): c.Tree = {
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
      dc.deriveInstance(typeClassTpe, tc, tpe, root)
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

trait DerivationContext extends CaseClassMacros with TypeClassTCMacros {
  type C <: whitebox.Context
  val c: C

  import c.universe._

  def deriveCtorsTpe = typeOf[DeriveConstructors]

  sealed trait DerivationState
  case object New extends DerivationState
  case object Probing extends DerivationState
  case object PropProductElement extends DerivationState
  case object PropCoproductElement extends DerivationState
  case object ResolvedInternal extends DerivationState
  case object ResolvedExternal extends DerivationState

  case class Instance(
    name: TermName,
    symbol: Symbol,
    typeClassTcTpe: Type,
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
    def product = typeClassTcTpe =:= productTypeClassTpe || typeClassTcTpe =:= labelledProductTypeClassTpe
    def labelled = typeClassTcTpe =:= labelledTypeClassTpe || typeClassTcTpe =:= labelledProductTypeClassTpe
    def typeClassTpe = appliedType(typeClassTcTpe, tc)
    def instTpe = appliedType(tc, tpe)
    def ident = Ident(symbol)
  }

  object Instance {
    def apply(
      typeClassTcTpe: Type,
      tc: Type,
      tpe: Type,
      state: DerivationState,
      standalone: Boolean
    ) = {
      val nme = TermName(c.freshName("inst"))
      val sym = c.internal.setInfo(c.internal.newTermSymbol(NoSymbol, nme), appliedType(tc, tpe))
      val tree = Ident(sym)

      new Instance(nme, sym, typeClassTcTpe, tc, tpe, tree, state, standalone)
    }
  }

  var dict: Map[Type, Instance] = Map.empty

  def add(d: Instance): Unit = {
    dict = dict.updated(d.instTpe, d)
  }

  def resolve(instance: Instance): Tree = {
    import instance.{ typeClassTcTpe, tc, tpe }

    val fromProduct = isProduct(tpe)

    if(!fromProduct && instance.product)
      abort(s"Cannot derive a ProductTypeClass for non-product $tpe")

    add(instance.copy(state = ResolvedInternal))

    val elements: List[Type] =
      if(fromProduct) fieldsOf(tpe).map(_._2)
      else ctorsOf(tpe)

    elements.foreach { tpe =>
      val instTpe = appliedType(tc, tpe)
      if(fromProduct && dict.contains(instTpe))
        add(dict(instTpe).copy(standalone = true))
      else {
        val propState = if(fromProduct) PropProductElement else PropCoproductElement
        val instance = Instance(typeClassTcTpe, tc, tpe, propState, fromProduct)
        add(instance)

        val inst = c.inferImplicitValue(instTpe, silent = true)
        if(dict(instTpe).propagating) {
          if(inst != instance.ident && inst != EmptyTree)
            add(instance.copy(inst = inst, state = ResolvedExternal, standalone = true))
          else {
            if(isProduct(tpe) || isCoproduct(tpe))
              deriveInstance(typeClassTcTpe, tc, tpe, false)
            if(dict(instTpe).propagating)
              c.abort(c.enclosingPosition, s"No implicit value for $instTpe during derivation.")
          }
        }
      }
    }

    instance.ident
  }

  def deriveInstance(typeClassTcTpe: Type, tc: Type, tpe: Type, root: Boolean): Tree = {
    val instTpe = appliedType(tc, tpe)
    val instTree =
      dict.get(instTpe) match {
        case Some(d) if d.probing => EmptyTree
        case Some(d) if d.resolved => d.inst
        case other =>
          val instance =
            other.map(_.copy(state = Probing)).getOrElse(Instance(typeClassTcTpe, tc, tpe, Probing, true))
          add(instance)

          if(root)
            resolve(instance)
          else {
            val extInst = c.inferImplicitValue(instTpe, silent = true)
            if(extInst == EmptyTree)
              resolve(instance)
            else {
              add(instance.copy(inst = extInst, state = ResolvedExternal, standalone = true))
              instance.ident
            }
          }
      }

    if(root) mkDerivations(instTpe) else instTree
  }

  def mkDerivations(primaryTpe: Type): Tree = {
    val instances = dict.values.toList
    val derived = instances.filter(!_.external)
    val primary = instances.filter(_.standalone)

    val typeClasses: Map[Type, (TermName, Tree)] =
      instances.map(_.typeClassTpe).distinct.map { typeClassTpe =>
        val typeClassName = TermName(c.freshName("typeClass"))

        val typeClassInst = c.inferImplicitValue(typeClassTpe, silent = true)
        if(typeClassInst == EmptyTree)
          abort(s"Unable to resolve implicit TypeClass instance $typeClassTpe")

        val typeClassTree = q"val $typeClassName: $typeClassTpe = $typeClassInst"
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

        val instTpe = appliedType(tc, tpe)
        val reprInstanceName = reprInstanceNames(instTpe)
        val typeClass = typeClasses(instance.typeClassTpe)._1

        val reprTree =
          if(isProduct(tpe))
            fieldsOf(tpe).
              foldRight(q"$typeClass.emptyProduct": Tree) { case ((nme, tpe), acc) =>
                val instTpe = appliedType(tc, tpe)
                val inst = dict(instTpe).symbol

                if(labelled)
                  q"""$typeClass.product(${nme.toString}, $inst, $acc)"""
                else
                  q"""$typeClass.product($inst, $acc)"""
              }
          else
            ctorsOf(tpe).
              foldRight(q"$typeClass.emptyCoproduct": Tree) { case (tpe, acc) =>
                val instTpe = appliedType(tc, tpe)
                val productInstance = dict(instTpe)
                val inst =
                  if(productInstance.external)
                    productInstance.ident
                  else {
                    val reprInstanceName = reprInstanceNames(instTpe)
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

        (reprInstanceName, q"""def $reprInstanceName = $reprTree""")
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

        if(product || isCoproduct(tpe))
          q"""implicit lazy val $name: $instTpe =
                $typeClass.project(
                  $reprInstanceName,
                  (t: $tpe) => $genName.to(t),
                  (r: $genName.Repr) => $genName.from(r)
                )
           """
        else {
          val sym = classSym(tpe)
          sym.baseClasses.find(baseSym => baseSym != sym && baseSym.isClass && baseSym.asClass.isSealed) match {
            case Some(baseSym) if c.inferImplicitValue(deriveCtorsTpe) == EmptyTree =>
              val msg =
                s"Attempting to derive a type class instance for class `${sym.name.decodedName.toString}` with "+
                s"sealed superclass `${baseSym.name.decodedName.toString}`; this is most likely unintended. To "+
                s"silence this warning, import `TypeClass.deriveConstructors`"

              if (c.compilerSettings contains "-Xfatal-warnings")
                c.error(c.enclosingPosition, msg)
              else
                c.warning(c.enclosingPosition, msg)
            case _ =>
          }

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
