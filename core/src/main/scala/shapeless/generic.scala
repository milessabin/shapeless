/*
 * Copyright (c) 2012-15 Lars Hupel, Miles Sabin
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

import scala.annotation.{ StaticAnnotation, tailrec }
import scala.reflect.macros.Context

import ops.{ hlist, coproduct }

 /** Represents the ability to convert from a concrete type (e.g. a case class)
  * to a generic ([[HList]] / [[Coproduct]]} based) representation of the type.
  *
  * For example:
  * {{{
  * scala> sealed trait Animal

  * defined trait Animal
  * scala> case class Cat(name: String, livesLeft: Int) extends Animal
  * defined class Cat
  *
  * scala> case class Dog(name: String, bonesHidden: Int) extends Animal
  * defined class Dog
  *
  * scala> val genCat = Generic[Cat]
  * genCat: shapeless.Generic[Cat]{ type Repr = String :: Int :: HNil } = ...
  *
  * scala> val genDog = Generic[Dog]
  * genDog: shapeless.Generic[Dog]{ type Repr = String :: Int :: HNil } = ...
  *
  * scala> val garfield = Cat("Garfield", 9)
  * garfield: Cat = Cat(Garfield,9)
  *
  * scala> val genGarfield = genCat.to(garfield)
  * genGarfield: genCat.Repr = Garfield :: 9 :: HNil
  *
  * scala> val reconstructed = genCat.from(genGarfield)
  * reconstructed: Cat = Cat(Garfield,9)
  *
  * scala> reconstructed == garfield
  * res0: Boolean = true
  *
  * }}}
  *
  * Note that constituents of Cat and Dog are exactly the same - a String and an Int. So we could do:
  *
  * {{{
  *
  * scala> val odieAsCat = genCat.from(genDog.to(odie))
  * odieAsCat: Cat = Cat(odie,3)
  *
  * }}}
  *
  * This is quite useful in certain cases, such as copying from one object type to another, as in schema evolution.
  *
  * Note that the generic representation depends on the type at which we instantiate Generic. In the
  * example above we instantiated it at Cat and at Dog, and so the generic representation gave the minimal constituents
  * of each of those.
  *
  * However, if we instantiate Generic[Animal] instead the generic representation would encode
  * the Cat-ness or Dog-ness of the instance as well (see [[Coproduct]] for details of the encoding):
  *
  * {{{
  *
  * scala> genDog.to(odie)
  * res9: genDog.Repr = odie :: 3 :: HNil
  *
  * scala> val genAnimal = Generic[Animal]
  * genAnimal: shapeless.Generic[Animal]{ type Repr = Cat :+: Dog :+: CNil } = ...
  *
  * scala> genAnimal.to(odie)
  * res8: genAnimal.Repr = Dog(odie,3)
  *
  * scala> genAnimal.to(odie) match { case Inr(Inl(dog)) => dog; case _ => null }
  * res9: Dog = Dog(odie,3)
  *
  * }}}
  *
  * Inr and Inl are [[shapeless.Coproduct]] constructors.
  * Shapeless constructs each class representation as a sort of
  * "nested Either" using Coproduct. So in our example, genAnimal would essentially encode garfield as Inl(garfield)
  * and odie as Inr(Inl(odie)). Please see [[shapeless.Coproduct]] for more details.
  * }}}
  *
  * @tparam T  An immutable data type that has a canonical way of constructing and deconstructing
  *            instances (e.g. via apply / unapply). Sealed families of case classes work best.
  */
trait Generic[T] extends Serializable {
  /** The generic representation type for {T}, which will be composed of {Coproduct} and {HList} types  */
  type Repr

  /** Convert an instance of the concrete type to the generic value representation */
  def to(t : T) : Repr

  /** Convert an instance of the generic representation to an instance of the concrete type */
  def from(r : Repr) : T
}

/** The companion object for the [[Generic]] trait provides a way of obtaining a Generic[T] instance
 * for some T. In addition, it defines [[Generic.Aux]], which is an important implementation technique
 * that can be generally useful.
 */
object Generic {

  /** Provides a representation of Generic[T], which has a nested Repr type, as a type with two type
   * parameters instead.
   *
   * This is useful for two reasons. First, it's surprisingly easy to wind up with a Generic type that
   * has lost the refinement that carries the crucial Generic.Repr type, a problem which Generic.Aux prevents.
   *
   * More importantly, Aux allows us to write code like this:
   *
   * {{{
   *   def myMethod[T]()(implicit eqGen: Generic.Aux[T,R], repEq: Eq[R]) = ???
   * }}}
   *
   * Here, we specify T, and we find a Generic.Aux[T,R] by implicit search. We then use R in the second argument.
   * Generic.Aux[T, R] is exactly equivalent to Generic[T] { type Repr = R }, but Scala doesn't allow us to write
   * it this way:
   *
   * {{{
   *   def myMethod[T]()(eqGen: Generic[T] { Repr = R }, reqEq: Eq[egGen.Repr) = ???
   * }}}
   *
   * The reason is that we are not allowed to have dependencies between arguments in the same parameter group. So
   * Aux neatly sidesteps this problem.
   *
   * The "Aux pattern" is now in use in several other libraries as well, and is a useful general technique.
   *
   * @tparam T the type for which we want to find a Generic
   * @tparam Repr0 the generic representation type equivalent to T.
   */
  type Aux[T, Repr0] = Generic[T] { type Repr = Repr0 }

  /** Provides an instance of Generic. Prefer this over finding one with `implicitly`, or else use `the`.
    *
    * Either of these approaches preserves the Repr type refinement, which `implicitly` will lose.
    *
    */
  def apply[T](implicit gen: Generic[T]): Aux[T, gen.Repr] = gen

  implicit def materialize[T, R]: Aux[T, R] = macro GenericMacros.materialize[T, R]
}

/**
  * LabelledGeneric is similar to Generic, but includes information about field
  * names or class names in addition to the raw structure.
  *
  * Continuing the example from [[shapeless.Generic]], we use LabelledGeneric to convert an object to an [[shapeless.HList]]:
  *
  * {{{
  * scala> val lgenDog = LabelledGeneric[Dog]
  * lgenDog: shapeless.LabelledGeneric[Dog]{ type Repr = Record.`'name -> String, 'bonesHidden -> Int`.T } = ...
  *
  * scala> lgenDog.to(odie)
  * res15: lgenDog.Repr = odie :: 3 :: HNil
  * }}}
  *
  * Note that the representation does not include the labels! The labels are actually encoded in the generic type representation
  * using [[shapeless.Witness]] types.
  *
  * As with [[shapeless.Generic]], the representation for Animal captures the subclass embedding rather than the fields in the class,
  * using [[shapeless.Coproduct]]:
  *
  * {{{
  * scala> val lgenAnimal = LabelledGeneric[Animal]
  * lgenAnimal: shapeless.LabelledGeneric[Animal]{ type Repr = Union.`'Cat -> Cat, 'Dog -> Dog`.T } = ...
  *
  * scala> lgenAnimal.to(odie)
  * res16: lgenAnimal.Repr = Dog(odie,3)
  *
  * scala> genAnimal.to(odie) match { case Inr(Inl(dog)) => dog ; case _ => ???}
  * res19: Dog = Dog(odie,3)
  *
  * }}}
  *
  * @tparam T the type which this instance can convert to and from a labelled generic representation
  */
trait LabelledGeneric[T] extends Generic[T]

object LabelledGeneric {

  /** Like [[shapeless.Generic.Aux]], this is an implementation of the Aux pattern, please
    * see comments there.
    * @tparam T the type
    * @tparam Repr0 the labelled generic representation of the type
    */
  type Aux[T, Repr0] = LabelledGeneric[T]{ type Repr = Repr0 }

  /** Provides an instance of LabelledGeneric for the given T. As with [[shapeless.Generic]],
    * use this method or {{{the[LabelledGeneric[T]]}}} to obtain an instance for suitable given T. */
  def apply[T](implicit lgen: LabelledGeneric[T]): Aux[T, lgen.Repr] = lgen

  /** Handles the Product case (fields in a case class, for example) */
  implicit def materializeProduct[T, K <: HList, V <: HList, R <: HList]
    (implicit
      lab: DefaultSymbolicLabelling.Aux[T, K],
      gen: Generic.Aux[T, V],
      zip: hlist.ZipWithKeys.Aux[K, V, R],
      ev: R <:< V
    ): Aux[T, R] =
    new LabelledGeneric[T] {
      type Repr = R
      def to(t: T): Repr = zip(gen.to(t))
      def from(r: Repr): T = gen.from(r)
    }

  /** Handles the Coproduct case (specifying subclasses derive from a sealed trait) */
  implicit def materializeCoproduct[T, K <: HList, V <: Coproduct, R <: Coproduct]
    (implicit
      lab: DefaultSymbolicLabelling.Aux[T, K],
      gen: Generic.Aux[T, V],
      zip: coproduct.ZipWithKeys.Aux[K, V, R],
      ev: R <:< V
    ): Aux[T, R] =
    new LabelledGeneric[T] {
      type Repr = R
      def to(t: T): Repr = zip(gen.to(t))
      def from(r: Repr): T = gen.from(r)
    }
}

class nonGeneric extends StaticAnnotation

class IsTuple[T] extends Serializable

object IsTuple {
  implicit def apply[T]: IsTuple[T] = macro GenericMacros.mkIsTuple[T]
}

class HasProductGeneric[T] extends Serializable

object HasProductGeneric {
  implicit def apply[T]: HasProductGeneric[T] = macro GenericMacros.mkHasProductGeneric[T]
}

class HasCoproductGeneric[T] extends Serializable

object HasCoproductGeneric {
  implicit def apply[T]: HasCoproductGeneric[T] = macro GenericMacros.mkHasCoproductGeneric[T]
}

trait ReprTypes {
  val c: Context
  import c.universe.{ Symbol => _, _ }

  def hlistTpe = typeOf[HList]
  def hnilTpe = typeOf[HNil]
  def hconsTpe = typeOf[::[_, _]].typeConstructor
  def coproductTpe = typeOf[Coproduct]
  def cnilTpe = typeOf[CNil]
  def cconsTpe = typeOf[:+:[_, _]].typeConstructor

  def atatTpe = typeOf[tag.@@[_,_]].typeConstructor
  def fieldTypeTpe = typeOf[shapeless.labelled.FieldType[_, _]].typeConstructor
  def keyTagTpe = typeOf[shapeless.labelled.KeyTag[_, _]].typeConstructor
  def symbolTpe = typeOf[Symbol]
}

trait CaseClassMacros extends ReprTypes {
  import c.universe._
  import Flag._

  def abort(msg: String) =
    c.abort(c.enclosingPosition, msg)

  def isReprType(tpe: Type): Boolean =
    tpe <:< hlistTpe || tpe <:< coproductTpe

  def isReprType1(tpe: Type): Boolean = {
    val normalized = appliedType(tpe, List(WildcardType)).normalize
    normalized <:< hlistTpe || normalized <:< coproductTpe
  }

  def isProduct(tpe: Type): Boolean =
    tpe =:= typeOf[Unit] || (tpe.typeSymbol.isClass && isCaseClassLike(classSym(tpe)))

  def isProduct1(tpe: Type): Boolean =
    appliedType(tpe, List(typeOf[Any])).normalize =:= typeOf[Unit] || (tpe.typeSymbol.isClass && isCaseClassLike(classSym(tpe)))

  def isCoproduct(tpe: Type): Boolean = {
    val sym = tpe.typeSymbol
    if(!sym.isClass) false
    else {
      val sym = classSym(tpe)
      (sym.isTrait || sym.isAbstractClass) && sym.isSealed
    }
  }

  def ownerChain(sym: Symbol): List[Symbol] = {
    @tailrec
    def loop(sym: Symbol, acc: List[Symbol]): List[Symbol] =
      if(sym.owner == NoSymbol) acc
      else loop(sym.owner, sym :: acc)

    loop(sym, Nil)
  }

  def mkDependentRef(prefix: Type, path: List[Name]): (Type, Symbol) = {
    val (_, pre, sym) =
      path.foldLeft((prefix, NoType, NoSymbol)) {
        case ((pre, _, sym), nme) =>
          val sym0 = pre.member(nme)
          val pre0 = sym0.typeSignature
          (pre0, pre, sym0)
      }
    (pre, sym)
  }

  def isAnonOrRefinement(sym: Symbol): Boolean = {
    val nameStr = sym.name.toString
    nameStr.contains("$anon") || nameStr == "<refinement>"
  }

  def fieldsOf(tpe: Type): List[(TermName, Type)] = {
    val tSym = tpe.typeSymbol
    if(tSym.isClass && isAnonOrRefinement(tSym)) Nil
    else
      tpe.declarations.toList collect {
        case sym: TermSymbol if isCaseAccessorLike(sym) =>
          val NullaryMethodType(restpe) = sym.typeSignatureIn(tpe)
          (sym.name.toTermName, restpe)
      }
  }

  def productCtorsOf(tpe: Type): List[Symbol] =
    tpe.declarations.toList.filter { x => x.isMethod && x.asMethod.isConstructor }

  def ctorsOf(tpe: Type): List[Type] = distinctCtorsOfAux(tpe, false)
  def ctorsOf1(tpe: Type): List[Type] = distinctCtorsOfAux(tpe, true)

  def distinctCtorsOfAux(tpe: Type, hk: Boolean): List[Type] = {
    def distinct[A](list: List[A])(eq: (A, A) => Boolean): List[A] = list.foldLeft(List.empty[A]) { (acc, x) =>
        if (!acc.exists(eq(x, _))) x :: acc
        else acc
    }.reverse
    distinct(ctorsOfAux(tpe, hk))(_ =:= _)
  }

  def ctorsOfAux(tpe: Type, hk: Boolean): List[Type] = {
    def collectCtors(classSym: ClassSymbol): List[ClassSymbol] = {
      classSym.knownDirectSubclasses.toList flatMap { child0 =>
        val child = child0.asClass
        child.typeSignature // Workaround for <https://issues.scala-lang.org/browse/SI-7755>
        if (isCaseClassLike(child) || isCaseObjectLike(child))
          List(child)
        else if (child.isSealed)
          collectCtors(child)
        else
          abort(s"$child is not case class like or a sealed trait")
      }
    }

    if(isProduct(tpe))
      List(tpe)
    else if(isCoproduct(tpe)) {
      def typeArgs(tpe: Type) = tpe match {
        case TypeRef(_, _, args) => args
        case _ => Nil
      }

      val basePre = prefix(tpe)
      val baseSym = classSym(tpe)
      val baseTpe =
        if(!hk) tpe
        else {
          val tc = tpe.typeConstructor
          val TypeRef(_, sym, _) = tc
          val paramSym = sym.asType.typeParams.head
          val paramTpe = paramSym.asType.toType
          appliedType(tc, List(paramTpe))
        }
      val baseArgs = typeArgs(baseTpe.normalize)

      val ctorSyms = collectCtors(baseSym).sortBy(_.fullName)
      val ctors =
        ctorSyms flatMap { sym =>
          def substituteArgs: List[Type] = {
            val subst = typeArgs(ThisType(sym).baseType(baseSym))
            sym.typeParams.map { param =>
              val paramTpe = param.asType.toType
              baseArgs(subst.indexWhere(_ =:= paramTpe))
            }
          }

          val suffix = ownerChain(sym).dropWhile(_ != basePre.typeSymbol)
          val ctor =
            if(suffix.isEmpty) {
              if(sym.isModuleClass) {
                val moduleSym = sym.asClass.module
                val modulePre = prefix(moduleSym.typeSignature)
                singleType(modulePre, moduleSym)
              } else
                appliedType(sym.toTypeIn(basePre), substituteArgs)
            } else {
              if(sym.isModuleClass) {
                val path = suffix.tail.map(_.name.toTermName)
                val (modulePre, moduleSym) = mkDependentRef(basePre, path)
                singleType(modulePre, moduleSym)
              } else if(isAnonOrRefinement(sym)) {
                val path = suffix.tail.init.map(_.name.toTermName)
                val (valPre, valSym) = mkDependentRef(basePre, path)
                singleType(valPre, valSym)
              } else {
                val path = suffix.tail.init.map(_.name.toTermName) :+ suffix.last.name.toTypeName
                val (subTpePre, subTpeSym) = mkDependentRef(basePre, path)
                typeRef(subTpePre, subTpeSym, substituteArgs)
              }
            }
          if(!isAccessible(ctor))
            abort(s"$tpe has an inaccessible subtype $ctor")
          if(ctor <:< baseTpe) Some(ctor) else None
        }
      if (ctors.isEmpty)
        abort(s"Sealed trait $tpe has no case class subtypes")
      ctors
    }
    else
      abort(s"$tpe is not a case class, case class-like, a sealed trait or Unit")
  }

  def nameAsString(name: Name): String = name.decodedName.toString.trim

  def nameAsValue(name: Name): Constant = Constant(nameAsString(name))

  def nameOf(tpe: Type) = tpe.typeSymbol.name

  def mkCompoundTpe(nil: Type, cons: Type, items: List[Type]): Type =
    items.foldRight(nil) {
      case (tpe, acc) => appliedType(cons, List(devarargify(tpe), acc))
    }

  def mkLabelTpe(name: Name): Type =
    appliedType(atatTpe, List(typeOf[scala.Symbol], ConstantType(nameAsValue(name))))

  def mkFieldTpe(name: Name, valueTpe: Type): Type = {
    appliedType(fieldTypeTpe, List(mkLabelTpe(name), valueTpe))
  }

  def mkHListTpe(items: List[Type]): Type =
    mkCompoundTpe(hnilTpe, hconsTpe, items)

  def mkCoproductTpe(items: List[Type]): Type =
    mkCompoundTpe(cnilTpe, cconsTpe, items)

  def unpackHListTpe(tpe: Type): List[Type] = {
    @tailrec
    def unfold(u: Type, acc: List[Type]): List[Type] = {
      val HNilTpe = hnilTpe
      val HConsPre = prefix(hconsTpe)
      val HConsSym = hconsTpe.typeSymbol
      if(u <:< HNilTpe) acc
      else (u baseType HConsSym) match {
        case TypeRef(pre, _, List(hd, tl)) if pre =:= HConsPre => unfold(tl, hd :: acc)
        case _ => abort(s"$tpe is not an HList type")
      }
    }

    unfold(tpe, List()).reverse
  }

  def unpackFieldType(tpe: Type): (Type, Type) = {
    val KeyTagPre = prefix(keyTagTpe)
    val KeyTagSym = keyTagTpe.typeSymbol
    tpe.normalize match {
      case RefinedType(List(v0, TypeRef(pre, KeyTagSym, List(k, v1))), _) if pre =:= KeyTagPre && v0 =:= v1 => (k, v0)
      case _ => abort(s"$tpe is not a field type")
    }
  }

  def mkTypTree(tpe: Type): Tree = {
    tpe match {
      case SingleType(pre @ SingleType(_, _), sym) =>
        SingletonTypeTree(mkAttributedRef(pre, sym))

      case TypeRef(pre, _, args) if isVararg(tpe) =>
        val argTrees = args.map(mkTypTree)
        AppliedTypeTree(tq"_root_.scala.collection.Seq", argTrees)

      case t => tq"$t"
    }
  }

  def appliedTypTree1(tpe: Type, param: Type, arg: TypeName): Tree = {
    tpe match {
      case t if t =:= param =>
        Ident(arg)
      case PolyType(params, body) if params.head.asType.toType =:= param =>
        appliedTypTree1(body, param, arg)
      case t @ TypeRef(pre, sym, List()) if t.takesTypeArgs =>
        val argTrees = sym.asType.typeParams.map(sym => appliedTypTree1(sym.asType.toType, param, arg))
        AppliedTypeTree(mkAttributedRef(pre, sym), argTrees)
      case TypeRef(pre, sym, List()) =>
        mkAttributedRef(pre, sym)
      case TypeRef(pre, sym, args) =>
        val argTrees = args.map(appliedTypTree1(_, param, arg))
        AppliedTypeTree(mkAttributedRef(pre, sym), argTrees)
      case t if t.takesTypeArgs =>
        val argTrees = t.typeSymbol.asType.typeParams.map(sym => appliedTypTree1(sym.asType.toType, param, arg))
        AppliedTypeTree(mkAttributedRef(tpe.typeConstructor), argTrees)
      case t =>
        tq"$tpe"
    }
  }

  def mkCompoundTypTree(nil: Type, cons: Type, items: List[Type]): Tree =
    items.foldRight(mkAttributedRef(nil): Tree) { case (tpe, acc) =>
      AppliedTypeTree(mkAttributedRef(cons), List(mkTypTree(tpe), acc))
    }

  def mkCompoundTypTree1(nil: Type, cons: Type, items: List[Type], param: Type, arg: TypeName): Tree =
    items.foldRight(mkAttributedRef(nil): Tree) { case (tpe, acc) =>
      AppliedTypeTree(mkAttributedRef(cons), List(appliedTypTree1(tpe, param, arg), acc))
    }

  def mkHListTypTree(items: List[Type]): Tree =
    mkCompoundTypTree(hnilTpe, hconsTpe, items)

  def mkHListTypTree1(items: List[Type], param: Type, arg: TypeName): Tree =
    mkCompoundTypTree1(hnilTpe, hconsTpe, items, param, arg)

  def mkCoproductTypTree(items: List[Type]): Tree =
    mkCompoundTypTree(cnilTpe, cconsTpe, items)

  def mkCoproductTypTree1(items: List[Type], param: Type, arg: TypeName): Tree =
    mkCompoundTypTree1(cnilTpe, cconsTpe, items, param, arg)

  def unfoldCompoundTpe(compoundTpe: Type, nil: Type, cons: Type): List[Type] = {
    @tailrec
    def loop(tpe: Type, acc: List[Type]): List[Type] =
      tpe.normalize match {
        case TypeRef(_, consSym, List(hd, tl))
          if consSym.asType.toType.typeConstructor =:= cons => loop(tl, hd :: acc)
        case `nil` => acc
        case other => abort(s"Bad compound type $compoundTpe")
      }
    loop(compoundTpe, Nil).reverse
  }

  def hlistElements(tpe: Type): List[Type] =
    unfoldCompoundTpe(tpe, hnilTpe, hconsTpe)

  def coproductElements(tpe: Type): List[Type] =
    unfoldCompoundTpe(tpe, cnilTpe, cconsTpe)

  def reprTpe(tpe: Type): Type = {
    if(isProduct(tpe)) mkHListTpe(fieldsOf(tpe).map(_._2))
    else mkCoproductTpe(ctorsOf(tpe))
  }

  def param1(tpe: Type): Type =
    tpe match {
      case t @ TypeRef(_, sym, args) if(t.takesTypeArgs) => sym.asType.typeParams.head.asType.toType
      case TypeRef(_, _, List(arg)) => arg
      case _ => NoType
    }

  def reprTypTree(tpe: Type): Tree = {
    if(isProduct(tpe)) mkHListTypTree(fieldsOf(tpe).map(_._2))
    else mkCoproductTypTree(ctorsOf(tpe))
  }

  def reprTypTree1(tpe: Type, arg: TypeName): Tree = {
    val param = param1(tpe)
    if(isProduct1(tpe)) mkHListTypTree1(fieldsOf(tpe).map(_._2), param, arg)
    else mkCoproductTypTree1(ctorsOf1(tpe), param, arg)
  }

  def isCaseClassLike(sym: ClassSymbol): Boolean = {
    def checkCtor: Boolean = {
      def unique[T](s: Seq[T]): Option[T] =
        s.headOption.find(_ => s.tail.isEmpty)

      val tpe = sym.typeSignature
      (for {
        ctor <- unique(productCtorsOf(tpe))
        params <- unique(ctor.asMethod.paramss)
      } yield params.size == fieldsOf(tpe).size).getOrElse(false)
    }

    sym.isCaseClass ||
    (!sym.isAbstractClass && !sym.isTrait &&
     sym.knownDirectSubclasses.isEmpty && checkCtor)
  }

  def isCaseObjectLike(sym: ClassSymbol): Boolean = sym.isModuleClass

  def isCaseAccessorLike(sym: TermSymbol): Boolean =
    !isNonGeneric(sym) && sym.isPublic && (if(sym.owner.asClass.isCaseClass) sym.isCaseAccessor else sym.isAccessor)

  def isSealedHierarchyClassSymbol(symbol: ClassSymbol): Boolean = {
    def helper(classSym: ClassSymbol): Boolean = {
      classSym.knownDirectSubclasses.toList forall { child0 =>
        val child = child0.asClass
        child.typeSignature // Workaround for <https://issues.scala-lang.org/browse/SI-7755>

        isCaseClassLike(child) || (child.isSealed && helper(child))
      }
    }

    symbol.isSealed && helper(symbol)
  }

  def classSym(tpe: Type): ClassSymbol = {
    val sym = tpe.typeSymbol
    if (!sym.isClass)
      abort(s"$sym is not a class or trait")

    val classSym = sym.asClass
    classSym.typeSignature // Workaround for <https://issues.scala-lang.org/browse/SI-7755>

    classSym
  }

  // See https://github.com/milessabin/shapeless/issues/212
  def companionRef(tpe: Type): Tree = {
    val global = c.universe.asInstanceOf[scala.tools.nsc.Global]
    val gTpe = tpe.asInstanceOf[global.Type]
    val pre = gTpe.prefix
    val cSym = patchedCompanionSymbolOf(tpe.typeSymbol).asInstanceOf[global.Symbol]
    if(cSym != NoSymbol)
      global.gen.mkAttributedRef(pre, cSym).asInstanceOf[Tree]
    else
      Ident(tpe.typeSymbol.name.toTermName) // Attempt to refer to local companion
  }

  def isAccessible(tpe: Type): Boolean = {
    val global = c.universe.asInstanceOf[scala.tools.nsc.Global]
    val typer = c.asInstanceOf[reflect.macros.runtime.Context].callsiteTyper.asInstanceOf[global.analyzer.Typer]
    val typerContext = typer.context
    typerContext.isAccessible(
      tpe.typeSymbol.asInstanceOf[global.Symbol],
      prefix(tpe).asInstanceOf[global.Type]
    )
  }

  // Cut-n-pasted (with most original comments) and slightly adapted from
  // https://github.com/scalamacros/paradise/blob/c14c634923313dd03f4f483be3d7782a9b56de0e/plugin/src/main/scala/org/scalamacros/paradise/typechecker/Namers.scala#L568-L613
  def patchedCompanionSymbolOf(original: Symbol): Symbol = {
    // see https://github.com/scalamacros/paradise/issues/7
    // also see https://github.com/scalamacros/paradise/issues/64

    val global = c.universe.asInstanceOf[scala.tools.nsc.Global]
    val typer = c.asInstanceOf[reflect.macros.runtime.Context].callsiteTyper.asInstanceOf[global.analyzer.Typer]
    val ctx = typer.context
    val owner = original.owner

    import global.analyzer.Context

    original.companionSymbol.orElse {
      import global.{ abort => aabort, _ }
      implicit class PatchedContext(ctx: Context) {
        trait PatchedLookupResult { def suchThat(criterion: Symbol => Boolean): Symbol }
        def patchedLookup(name: Name, expectedOwner: Symbol) = new PatchedLookupResult {
          override def suchThat(criterion: Symbol => Boolean): Symbol = {
            var res: Symbol = NoSymbol
            var ctx = PatchedContext.this.ctx
            while (res == NoSymbol && ctx.outer != ctx) {
              // NOTE: original implementation says `val s = ctx.scope lookup name`
              // but we can't use it, because Scope.lookup returns wrong results when the lookup is ambiguous
              // and that triggers https://github.com/scalamacros/paradise/issues/64
              val s = {
                val lookupResult = ctx.scope.lookupAll(name).filter(criterion).toList
                lookupResult match {
                  case Nil => NoSymbol
                  case List(unique) => unique
                  case _ => aabort(s"unexpected multiple results for a companion symbol lookup for $original#{$original.id}")
                }
              }
              if (s != NoSymbol && s.owner == expectedOwner)
                res = s
              else
                ctx = ctx.outer
            }
            res
          }
        }
      }
      ctx.patchedLookup(original.asInstanceOf[global.Symbol].name.companionName, owner.asInstanceOf[global.Symbol]).suchThat(sym =>
        (original.isTerm || sym.hasModuleFlag) &&
          (sym isCoDefinedWith original.asInstanceOf[global.Symbol])
      ).asInstanceOf[c.universe.Symbol]
    }
  }

  def prefix(tpe: Type): Type = {
    val global = c.universe.asInstanceOf[scala.tools.nsc.Global]
    val gTpe = tpe.asInstanceOf[global.Type]
    gTpe.prefix.asInstanceOf[Type]
  }

  def mkAttributedRef(tpe: Type): Tree = {
    val global = c.universe.asInstanceOf[scala.tools.nsc.Global]
    val gTpe = tpe.asInstanceOf[global.Type]
    val pre = gTpe.prefix
    val sym = gTpe.typeSymbol
    global.gen.mkAttributedRef(pre, sym).asInstanceOf[Tree]
  }

  def mkAttributedRef(pre: Type, sym: Symbol): Tree = {
    val global = c.universe.asInstanceOf[scala.tools.nsc.Global]
    val gPre = pre.asInstanceOf[global.Type]
    val gSym = sym.asInstanceOf[global.Symbol]
    global.gen.mkAttributedRef(gPre, gSym).asInstanceOf[Tree]
  }

  def isNonGeneric(sym: Symbol): Boolean = {
    def check(sym: Symbol): Boolean = {
      // See https://issues.scala-lang.org/browse/SI-7424
      sym.typeSignature                   // force loading method's signature
      sym.annotations.foreach(_.tpe) // force loading all the annotations

      sym.annotations.exists(_.tpe =:= typeOf[nonGeneric])
    }

    // See https://issues.scala-lang.org/browse/SI-7561
    check(sym) ||
    (sym.isTerm && sym.asTerm.isAccessor && check(sym.asTerm.accessed)) ||
    sym.allOverriddenSymbols.exists(isNonGeneric)
  }

  def isTuple(tpe: Type): Boolean =
    tpe <:< typeOf[Unit] || definitions.TupleClass.seq.contains(tpe.typeSymbol)

  def isVararg(tpe: Type): Boolean =
    tpe.typeSymbol == c.universe.definitions.RepeatedParamClass

  def devarargify(tpe: Type): Type =
    tpe match {
      case TypeRef(pre, _, args) if isVararg(tpe) =>
        appliedType(typeOf[scala.collection.Seq[_]].typeConstructor, args)
      case _ => tpe
    }
}

class GenericMacros[C <: Context](val c: C) extends CaseClassMacros {
  import c.universe._
  import Flag._

  def materialize[T: WeakTypeTag, R: WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[T]
    if(isReprType(tpe))
      abort("No Generic instance available for HList or Coproduct")

    if(isProduct(tpe))
      mkProductGeneric(tpe)
    else
      mkCoproductGeneric(tpe)
  }

  def mkProductGeneric(tpe: Type): Tree = {
    def mkProductCases: (CaseDef, CaseDef) = {
      if(tpe =:= typeOf[Unit])
        (
          cq"() => _root_.shapeless.HNil",
          cq"_root_.shapeless.HNil => ()"
        )
      else if(isCaseObjectLike(tpe.typeSymbol.asClass)) {
        val singleton =
          tpe match {
            case SingleType(pre, sym) =>
              mkAttributedRef(pre, sym)
            case TypeRef(pre, sym, List()) if sym.isModule =>
              mkAttributedRef(pre, sym.asModule)
            case TypeRef(pre, sym, List()) if sym.isModuleClass =>
              mkAttributedRef(pre, sym.asClass.module)
            case other =>
              abort(s"Bad case object-like type $tpe")
          }

        (
          cq"_: $tpe => _root_.shapeless.HNil",
          cq"_root_.shapeless.HNil => $singleton: $tpe"
        )
      } else {
        val sym = tpe.typeSymbol
        val isCaseClass = sym.asClass.isCaseClass
        def hasNonGenericCompanionMember(name: String): Boolean = {
          val mSym = sym.companionSymbol.typeSignature.member(newTermName(name))
          mSym != NoSymbol && !isNonGeneric(mSym)
        }

        val binders = fieldsOf(tpe).map { case (name, tpe) => (newTermName(c.fresh("pat")), name, tpe, isVararg(tpe)) }

        val to =
          if(isCaseClass || hasNonGenericCompanionMember("unapply")) {
            val wcard = Star(Ident(nme.WILDCARD))  // like pq"_*" except that it does work
            val lhs = pq"${companionRef(tpe)}(..${binders.map(x => if (x._4) pq"${x._1} @ $wcard" else pq"${x._1}")})"
            val rhs =
              binders.foldRight(q"_root_.shapeless.HNil": Tree) {
                case ((bound, name, tpe, _), acc) =>
                  tpe match {
                    case ConstantType(c) =>
                      q"_root_.shapeless.::($c, $acc)"
                    case _ =>
                      q"_root_.shapeless.::($bound, $acc)"
                  }
              }
            cq"$lhs => $rhs"
          } else {
            val lhs = newTermName(c.fresh("pat"))
            val rhs =
              fieldsOf(tpe).foldRight(q"_root_.shapeless.HNil": Tree) {
                case ((name, tpe), acc) => q"_root_.shapeless.::($lhs.$name, $acc)"
              }
            cq"$lhs => $rhs"
          }

        val from = {
          val lhs =
            binders.foldRight(q"_root_.shapeless.HNil": Tree) {
              case ((bound, _, _, _), acc) => pq"_root_.shapeless.::($bound, $acc)"
            }

          val rhs = {
            val ctorArgs = binders.map { case (bound, name, tpe, vararg) =>
              if (vararg) q"$bound: _*"
              else
                tpe match {
                  case ConstantType(c) =>
                    q"$c.asInstanceOf[$tpe]"
                  case _ =>
                    Ident(bound)
                }
            }
            if(isCaseClass || hasNonGenericCompanionMember("apply"))
              q"${companionRef(tpe)}(..$ctorArgs)"
            else
              q"new $tpe(..$ctorArgs)"
          }

          cq"$lhs => $rhs"
        }

        (to, from)
      }
    }

    val (toCases, fromCases) = {
      val (to, from) = mkProductCases
      (List(to), List(from))
    }

    val clsName = newTypeName(c.fresh("anon$"))
    q"""
      final class $clsName extends _root_.shapeless.Generic[$tpe] {
        type Repr = ${reprTypTree(tpe)}
        def to(p: $tpe): Repr = (p match { case ..$toCases }).asInstanceOf[Repr]
        def from(p: Repr): $tpe = p match { case ..$fromCases }
      }
      new $clsName(): _root_.shapeless.Generic.Aux[$tpe, ${reprTypTree(tpe)}]
    """
  }

  def mkCoproductGeneric(tpe: Type): Tree = {
    def mkCoproductCases(tpe0: Type, index: Int): CaseDef = {
      tpe0 match {
        case SingleType(pre, sym) =>
          val singleton = mkAttributedRef(pre, sym)
          cq"p if p eq $singleton => $index"
        case _ =>
          cq"_: $tpe0 => $index"
      }
    }

    val to = {
      val toCases = ctorsOf(tpe) zip (Stream from 0) map (mkCoproductCases _).tupled
      q"""_root_.shapeless.Coproduct.unsafeMkCoproduct((p: @_root_.scala.unchecked) match { case ..$toCases }, p).asInstanceOf[Repr]"""
    }

    val clsName = newTypeName(c.fresh("anon$"))
    q"""
      final class $clsName extends _root_.shapeless.Generic[$tpe] {
        type Repr = ${reprTypTree(tpe)}
        def to(p: $tpe): Repr = $to
        def from(p: Repr): $tpe = _root_.shapeless.Coproduct.unsafeGet(p).asInstanceOf[$tpe]
      }
      new $clsName(): _root_.shapeless.Generic.Aux[$tpe, ${reprTypTree(tpe)}]
    """
  }

  def mkIsTuple[T: WeakTypeTag]: Tree = {
    val tTpe = weakTypeOf[T]
    if(!isTuple(tTpe))
      abort(s"Unable to materialize IsTuple for non-tuple type $tTpe")

    q"""new _root_.shapeless.IsTuple[$tTpe]: _root_.shapeless.IsTuple[$tTpe]"""
  }

  def mkHasProductGeneric[T: WeakTypeTag]: Tree = {
    val tTpe = weakTypeOf[T]
    if(isReprType(tTpe) || !isProduct(tTpe))
      abort(s"Unable to materialize HasProductGeneric for $tTpe")

    q"""new _root_.shapeless.HasProductGeneric[$tTpe]: _root_.shapeless.HasProductGeneric[$tTpe]"""
  }

  def mkHasCoproductGeneric[T: WeakTypeTag]: Tree = {
    val tTpe = weakTypeOf[T]
    if(isReprType(tTpe) || !isCoproduct(tTpe))
      abort(s"Unable to materialize HasCoproductGeneric for $tTpe")

    q"""new _root_.shapeless.HasCoproductGeneric[$tTpe]: _root_.shapeless.HasCoproductGeneric[$tTpe]"""
  }
}

object GenericMacros {
  def inst(c: Context) = new GenericMacros[c.type](c)

  def materialize[T: c.WeakTypeTag, R: c.WeakTypeTag](c: Context): c.Expr[Generic.Aux[T, R]] =
    c.Expr[Generic.Aux[T, R]](inst(c).materialize[T, R])

  def mkIsTuple[T: c.WeakTypeTag](c: Context): c.Expr[IsTuple[T]] =
    c.Expr[IsTuple[T]](inst(c).mkIsTuple[T])

  def mkHasProductGeneric[T: c.WeakTypeTag](c: Context): c.Expr[HasProductGeneric[T]] = 
    c.Expr[HasProductGeneric[T]](inst(c).mkHasProductGeneric[T])

  def mkHasCoproductGeneric[T: c.WeakTypeTag](c: Context): c.Expr[HasCoproductGeneric[T]] =
    c.Expr[HasCoproductGeneric[T]](inst(c).mkHasCoproductGeneric[T])
}
