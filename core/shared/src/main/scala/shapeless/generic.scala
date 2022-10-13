/*
 * Copyright (c) 2012-18 Lars Hupel, Miles Sabin
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

import shapeless.ops.{coproduct, hlist}

import scala.annotation.{StaticAnnotation, tailrec}
import scala.language.experimental.macros
import scala.reflect.macros.{blackbox, whitebox}

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
   *   def myMethod[T, R]()(implicit eqGen: Generic.Aux[T,R], repEq: Eq[R]) = ???
   * }}}
   *
   * Here, we specify T, and we find a Generic.Aux[T,R] by implicit search. We then use R in the second argument.
   * Generic.Aux[T, R] is exactly equivalent to Generic[T] { type Repr = R }, but Scala doesn't allow us to write
   * it this way:
   *
   * {{{
   *   def myMethod[T, R]()(eqGen: Generic[T] { Repr = R }, reqEq: Eq[egGen.Repr]) = ???
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
    */
  def apply[T](implicit gen: Generic[T]): Aux[T, gen.Repr] = gen

  /** Creates a new Generic instance from a pair of functions.
    *
    * The functions `f` and `g` should be the inverse of each other, i.e.
    *   - `f(g(x)) == x`
    *   - `g(f(y)) == y`
    */
  def instance[T, R](f: T => R, g: R => T): Aux[T, R] = new Generic[T] {
    type Repr = R
    def to(t: T): R = f(t)
    def from(r: R): T = g(r)
  }

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
trait LabelledGeneric[T] extends Serializable {
  /** The generic representation type for {T}, which will be composed of {Coproduct} and {HList} types  */
  type Repr

  /** Convert an instance of the concrete type to the generic value representation */
  def to(t : T) : Repr

  /** Convert an instance of the generic representation to an instance of the concrete type */
  def from(r : Repr) : T
}

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
  val c: blackbox.Context
  import c.universe.{Symbol => _, _}

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

  def objectRef[O: TypeTag]: Tree = Ident(typeOf[O].termSymbol)
}

trait CaseClassMacros extends ReprTypes with CaseClassMacrosVersionSpecifics {
  val c: whitebox.Context

  import c.universe._
  import internal.constantType

  def abort(msg: String) =
    c.abort(c.enclosingPosition, msg)

  def isReprType(tpe: Type): Boolean =
    tpe <:< hlistTpe || tpe <:< coproductTpe

  def isReprType1(tpe: Type): Boolean =
    isReprType(lowerKind(tpe))

  /**
   * Lower the order of `tpe`'s kind by applying `Any` in place of all type parameters (`Any` is poly-kinded).
   * Note that the resulting type is dealiased before being returned.
   *
   * {{{
   *   lowerKind(typeOf[List[_]].typeConstructor) -> List[Any]
   * }}}
   */
  def lowerKind(tpe: Type): Type =
    appliedType(tpe, tpe.typeParams.map(_ => definitions.AnyTpe)).dealias

  def isProductAux(tpe: Type): Boolean =
    tpe.typeSymbol.isClass && {
      val cls = tpe.typeSymbol.asClass
      isCaseObjectLike(cls) || isCaseClassLike(cls) || HasApplyUnapply(tpe) || HasCtorUnapply(tpe)
    }

  def isProduct(tpe: Type): Boolean =
    tpe =:= definitions.UnitTpe || (!(tpe =:= definitions.AnyRefTpe) && isProductAux(tpe))

  def isProduct1(tpe: Type): Boolean =
    isProduct(lowerKind(tpe))

  def isCoproduct(tpe: Type): Boolean =
    tpe.typeSymbol.isClass && {
      val cls = tpe.typeSymbol.asClass
      (cls.isTrait || cls.isAbstract) && cls.isSealed
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

  /**
   * @return a List of name and type pairs for the fields of type `tpe`.
   * @see [[isCaseAccessorLike]] for the definition of what is considered a field.
   * */
  def fieldsOf(tpe: Type): List[(TermName, Type)] = {
    val clazz = tpe.typeSymbol.asClass
    // Case class field names have an extra space at the end.
    val nameOf: TermSymbol => TermName =
      if (!clazz.isCaseClass) _.name
      else field => TermName(field.name.toString.dropRight(1))
    if (isCaseObjectLike(clazz) || isAnonOrRefinement(clazz)) Nil
    else tpe.decls.sorted.collect {
      case field: TermSymbol if isCaseAccessorLike(field) =>
        nameOf(field) -> field.typeSignatureIn(tpe).finalResultType
    }
  }

  def productCtorsOf(tpe: Type): List[Symbol] = tpe.decls.toList.filter(_.isConstructor)

  def accessiblePrimaryCtorOf(tpe: Type): Option[Symbol] = {
    for {
      ctor <- tpe.decls.find { sym => sym.isMethod && sym.asMethod.isPrimaryConstructor && isAccessible(tpe, sym) }
      if !ctor.isJava || productCtorsOf(tpe).size == 1
    } yield ctor
  }

  def ctorsOf(tpe: Type): List[Type] = distinctCtorsOfAux(tpe, hk = false)
  def ctorsOf1(tpe: Type): List[Type] = distinctCtorsOfAux(tpe, hk = true)

  def distinctCtorsOfAux(tpe: Type, hk: Boolean): List[Type] = {
    def distinct[A](list: List[A])(eq: (A, A) => Boolean): List[A] = list.foldLeft(List.empty[A]) { (acc, x) =>
        if (!acc.exists(eq(x, _))) x :: acc
        else acc
    }.reverse
    distinct(ctorsOfAux(tpe, hk))(_ =:= _)
  }

  def ctorsOfAux(tpe: Type, hk: Boolean): List[Type] = {
    def collectCtors(classSym: ClassSymbol): List[ClassSymbol] =
      classSym.knownDirectSubclasses.toList.flatMap { child =>
        val cls = child.asClass
        if (isProductAux(cls.typeSignature)) List(cls)
        else if (cls.isSealed) collectCtors(cls)
        else abort(s"$cls is not case class like or a sealed trait")
      }

    if(isProduct(tpe))
      List(tpe)
    else if(isCoproduct(tpe)) {
      val basePre = prefix(tpe)
      val baseSym = classSym(tpe)
      val baseTpe =
        if(!hk) tpe
        else {
          val tc = tpe.typeConstructor
          val paramSym = tc.typeParams.head
          val paramTpe = paramSym.asType.toType
          appliedType(tc, paramTpe)
        }
      val baseArgs = baseTpe.dealias.typeArgs

      def isLess(sym1: Symbol, sym2: Symbol): Boolean = {
        val global = c.universe.asInstanceOf[scala.tools.nsc.Global]
        val gSym1 = sym1.asInstanceOf[global.Symbol]
        val gSym2 = sym2.asInstanceOf[global.Symbol]
        gSym1.isLess(gSym2)
      }

      def orderSyms(s1: Symbol, s2: Symbol): Boolean = {
        val fn1 = s1.fullName
        val fn2 = s2.fullName
        fn1 < fn2 || (fn1 == fn2 && isLess(s1, s2))
      }

      val ctors = collectCtors(baseSym).sortWith(orderSyms).flatMap { sym =>
        import c.internal._

        val owner = sym.owner
        val isNamed = !isAnonOrRefinement(sym)

        // Construct a stable prefix from the path.
        val pre = if (sym.isStatic) prefix(sym.toType) else {
          // Look for a path from the macro call site to the subtype.
          val owners = ownerChain(if (isNamed) owner else owner.owner)
          val prePaths = for (pre <- Iterator.iterate(basePre)(prefix).takeWhile(_ != NoPrefix))
            yield (pre, owners.iterator.dropWhile(pre.baseType(_) == NoType))

          // Find a path from a (sub-)prefix or the enclosing owner.
          val (pre0, path) = prePaths.find(_._2.nonEmpty).getOrElse {
            val enclosing = ownerChain(enclosingOwner)
            val common = owners zip enclosing indexWhere { case (o1, o2) => o1 != o2 }
            (NoPrefix, if (common < 0) Iterator.empty else owners.iterator drop common - 1)
          }

          path.drop(1).foldLeft(pre0) { (pre1, part) =>
            if (part.isType) part.asType.toTypeIn(pre1)
            else abort(s"$tpe has a subtype $sym with unstable prefix")
          }
        }

        val ctor = if (isNamed) {
          if (sym.isModuleClass) {
            singleType(pre, sym.module)
          } else {
            val subst = thisType(sym).baseType(baseSym).typeArgs.map(_.typeSymbol)
            val params = sym.typeParams
            val free = params.exists(!subst.contains(_))
            val args = for (param <- params) yield {
              val i = subst.indexOf(param)
              if (i >= 0) baseArgs(i) else param.asType.toType
            }

            val ref = typeRef(pre, sym, args)
            if (free) existentialAbstraction(params, ref) else ref
          }
        } else {
          def ownerIsSubType = owner.typeSignatureIn(pre) <:< baseTpe
          if (owner.isTerm && owner.asTerm.isVal && ownerIsSubType) singleType(pre, owner)
          else abort(s"$tpe has a subtype $sym with unstable prefix")
        }

        if (!isAccessible(ctor)) abort(s"$tpe has an inaccessible subtype $ctor")
        else if (ctor <:< baseTpe) Some(ctor)
        else None
      }

      if (ctors.isEmpty) abort(s"Sealed trait $tpe has no case class subtypes")
      else ctors
    } else {
      abort(s"$tpe is not a case class, case class-like, a sealed trait or Unit")
    }
  }

  def nameAsString(name: Name): String =
    name.decodedName.toString

  def nameAsValue(name: Name): Constant =
    Constant(nameAsString(name))

  def nameOf(tpe: Type): Name =
    tpe.typeSymbol.name

  def mkHListValue(elems: List[Tree]): Tree = {
    val cons = objectRef[::.type]
    elems.foldRight(objectRef[HNil.type]) {
      case (elem, acc) => q"$cons($elem, $acc)"
    }
  }

  /**
   * Fold `items` into a type using `cons` as a type constructor.
   *
   * {{{
   *   mkCompoundTpe(hnilTpe, hconsTpe, Seq(typeOf[String], typeOf[Int])) -> String :: Int :: HNil
   * }}}
   */
  def mkCompoundTpe(nil: Type, cons: Type, items: List[Type]): Type =
    items.foldRight(nil) { (tpe, acc) =>
      appliedType(cons, List(devarargify(tpe), acc))
    }

  def mkLabelTpe(name: Name): Type =
    appliedType(atatTpe, List(typeOf[scala.Symbol], constantType(nameAsValue(name))))

  def mkFieldTpe(name: Name, valueTpe: Type): Type =
    appliedType(fieldTypeTpe, List(mkLabelTpe(name), valueTpe))

  /**
   * Convert `items` to corresponding HList type.
   */
  def mkHListTpe(items: List[Type]): Type =
    mkCompoundTpe(hnilTpe, hconsTpe, items)

  /**
   * Convert `items` to corresponding Coproduct type.
   */
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

  object FieldType {
    import internal._

    def apply(kTpe: Type, vTpe: Type): Type =
      refinedType(List(vTpe, typeRef(prefix(keyTagTpe), keyTagTpe.typeSymbol, List(kTpe, vTpe))), NoSymbol)

    def unapply(fTpe: Type): Option[(Type, Type)] = {
      val KeyTagPre = prefix(keyTagTpe)
      val KeyTagSym = keyTagTpe.typeSymbol
      fTpe.dealias match {
        case RefinedType(v0 :+ TypeRef(pre, KeyTagSym, List(k, v1)), scope)
          if pre =:= KeyTagPre && refinedType(v0, NoSymbol, scope, NoPosition) =:= v1 =>
            Some((k, v1))
        case _ => None
      }
    }
  }

  def unpackFieldType(tpe: Type): (Type, Type) =
    FieldType.unapply(tpe).getOrElse(abort(s"$tpe is not a field type"))

  def findField(lTpe: Type, kTpe: Type): Option[(Type, Int)] =
    unpackHListTpe(lTpe).zipWithIndex.collectFirst {
      case (FieldType(k, v), i) if k =:= kTpe => (v, i)
    }

  def mkTypTree(tpe: Type): Tree =
    tpe match {
      case singleton: SingleType =>
        SingletonTypeTree(mkAttributedRef(singleton))
      case TypeRef(_, _, args) if isVararg(tpe) =>
        val argTrees = args.map(mkTypTree)
        AppliedTypeTree(varargTpt, argTrees)
      case other =>
        tq"$other"
    }

  def appliedTypTree1(tpe: Type, param: Type, arg: TypeName): Tree = {
    tpe match {
      case t if t =:= param =>
        Ident(arg)
      case PolyType(params, body) if params.head.asType.toType =:= param =>
        appliedTypTree1(body, param, arg)
      case TypeRef(pre, sym, Nil) =>
        mkAttributedRef(pre, sym)
      case TypeRef(pre, sym, args) =>
        val argTrees = args.map(appliedTypTree1(_, param, arg))
        AppliedTypeTree(mkAttributedRef(pre, sym), argTrees)
      case other =>
        tq"$other"
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
      tpe.dealias match {
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
      case t if tpe.takesTypeArgs => t.typeParams.head.asType.toType
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

  /** Returns the parameter lists of `tpe`, removing any implicit parameters. */
  private def nonImplicitParamLists(tpe: Type): List[List[Symbol]] =
    tpe.paramLists.takeWhile(params => params.isEmpty || !params.head.isImplicit)

  def isCaseClassLike(sym: ClassSymbol): Boolean = {
    def isConcrete = !(sym.isAbstract || sym.isTrait || sym == symbolOf[Object])
    def isFinalLike = sym.isFinal || sym.knownDirectSubclasses.isEmpty
    def constructor = for {
      constructor <- accessiblePrimaryCtorOf(sym.typeSignature)
      Seq(params) <- Option(nonImplicitParamLists(constructor.typeSignature))
      if params.length == fieldsOf(sym.typeSignature).length
    } yield constructor
    sym.isCaseClass || (isConcrete && isFinalLike && constructor.isDefined)
  }

  def isCaseObjectLike(sym: ClassSymbol): Boolean =
    sym.isModuleClass

  def isCaseAccessorLike(sym: TermSymbol): Boolean = {
    val isGetter =
      if (sym.owner.asClass.isCaseClass) sym.isCaseAccessor && !sym.isMethod
      else sym.isGetter && sym.isPublic && (sym.isParamAccessor || sym.isLazy)
    isGetter && !isNonGeneric(sym)
  }

  def isSealedHierarchyClassSymbol(symbol: ClassSymbol): Boolean = {
    def helper(classSym: ClassSymbol): Boolean =
      classSym.knownDirectSubclasses.toList.forall { child =>
        val cls = child.asClass
        isCaseClassLike(cls) || (cls.isSealed && helper(cls))
      }

    symbol.isSealed && helper(symbol)
  }

  def classSym(tpe: Type): ClassSymbol = {
    val sym = tpe.typeSymbol
    if (!sym.isClass) abort(s"$sym is not a class or trait")
    sym.asClass
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

  def isAccessible(pre: Type, sym: Symbol): Boolean = {
    val global = c.universe.asInstanceOf[scala.tools.nsc.Global]
    val typer = c.asInstanceOf[scala.reflect.macros.runtime.Context].callsiteTyper.asInstanceOf[global.analyzer.Typer]
    val typerContext = typer.context
    typerContext.isAccessible(
      sym.asInstanceOf[global.Symbol],
      pre.asInstanceOf[global.Type]
    )
  }
  def isAccessible(tpe: Type): Boolean =
    isAccessible(prefix(tpe), tpe.typeSymbol)

  // Cut-n-pasted (with most original comments) and slightly adapted from
  // https://github.com/scalamacros/paradise/blob/c14c634923313dd03f4f483be3d7782a9b56de0e/plugin/src/main/scala/org/scalamacros/paradise/typechecker/Namers.scala#L568-L613
  def patchedCompanionSymbolOf(original: Symbol): Symbol = {
    // see https://github.com/scalamacros/paradise/issues/7
    // also see https://github.com/scalamacros/paradise/issues/64

    val global = c.universe.asInstanceOf[scala.tools.nsc.Global]
    val typer = c.asInstanceOf[scala.reflect.macros.runtime.Context].callsiteTyper.asInstanceOf[global.analyzer.Typer]
    val ctx = typer.context
    val owner = original.owner

    import global.analyzer.Context

    original.companion.orElse {
      import global.{abort => aabort, _}
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

  def mkAttributedRef(singleton: SingleType): Tree = {
    val sym = singleton.sym
    val getter = sym.asTerm.getter.orElse(sym)
    mkAttributedRef(singleton.pre, getter)
  }

  /**
   * Check if `sym` or any of its overrides are annotated by [[nonGeneric]].
   */
  def isNonGeneric(sym: Symbol): Boolean = {
    def check(sym: Symbol): Boolean = {
      // See https://issues.scala-lang.org/browse/SI-7424
      sym.typeSignature                   // force loading method's signature
      sym.annotations.foreach(_.tree.tpe) // force loading all the annotations

      sym.annotations.exists(_.tree.tpe =:= typeOf[nonGeneric])
    }

    // See https://issues.scala-lang.org/browse/SI-7561
    check(sym) ||
    (sym.isTerm && sym.asTerm.isAccessor && check(sym.asTerm.accessed)) ||
    sym.overrides.exists(isNonGeneric)
  }

  def isTuple(tpe: Type): Boolean =
    tpe <:< typeOf[Unit] || definitions.TupleClass.seq.contains(tpe.typeSymbol)

  def isVararg(tpe: Type): Boolean =
    tpe.typeSymbol == c.universe.definitions.RepeatedParamClass

  /**
   * Convert a varargs type to corresponding Seq type.
   *
   * {{{
   *   String* -> Seq[String]
   * }}}
   */
  def devarargify(tpe: Type): Type =
    tpe match {
      case TypeRef(_, _, args) if isVararg(tpe) =>
        appliedType(varargTC, args)
      case _ => tpe
    }

  def unByName(tpe: Type): Type =
    tpe match {
      case TypeRef(_, sym, List(tpe)) if sym == definitions.ByNameParamClass => tpe
      case tpe => tpe
    }

  def equalTypes(as: List[Type], bs: List[Type]): Boolean =
    as.length == bs.length && (as zip bs).foldLeft(true) { case (acc, (a, b)) => acc && unByName(a) =:= unByName(b) }

  def alignFields(tpe: Type, args: List[(TermName, Type)]): Option[List[(TermName, Type)]] = for {
    fields <- Option(fieldsOf(tpe))
    if fields.size == args.size
    if fields.zip(args).forall { case ((fn, ft), (an, at)) =>
      (fn == an || at.typeSymbol == definitions.ByNameParamClass) && ft =:= unByName(at)
    }
  } yield fields

  def numNonCaseParamLists(tpe: Type): Int = {
    val companion = patchedCompanionSymbolOf(tpe.typeSymbol).typeSignature
    val apply = companion.member(TermName("apply"))
    if (!apply.isMethod || isNonGeneric(apply) || !isAccessible(companion, apply)) 0
    else nonImplicitParamLists(apply.typeSignatureIn(companion)).length.max(1) - 1
  }

  object HasApply {
    def unapply(tpe: Type): Option[List[(TermName, Type)]] = for {
      companion <- Option(patchedCompanionSymbolOf(tpe.typeSymbol).typeSignature)
      apply <- Option(companion.member(TermName("apply")))
      if apply.isTerm && !apply.asTerm.isOverloaded
      if apply.isMethod && !isNonGeneric(apply)
      if isAccessible(companion, apply)
      Seq(params) <- Option(nonImplicitParamLists(apply.typeSignatureIn(companion)))
      aligned <- alignFields(tpe, for (param <- params)
        yield param.name.toTermName -> param.typeSignature)
    } yield aligned
  }

  object HasUnapply {
    def unapply(tpe: Type): Option[List[Type]] = for {
      companion <- Option(patchedCompanionSymbolOf(tpe.typeSymbol).typeSignature)
      unapply <- Option(companion.member(TermName("unapply")))
      if unapply.isTerm && !unapply.asTerm.isOverloaded
      if unapply.isMethod && !isNonGeneric(unapply)
      if isAccessible(companion, unapply)
      returnTpe <- unapply.typeSignatureIn(companion).finalResultType
        .baseType(symbolOf[Option[_]]).typeArgs.headOption
    } yield if (returnTpe <:< typeOf[Product]) returnTpe.typeArgs else List(returnTpe)
  }

  object HasUniqueCtor {
    def unapply(tpe: Type): Option[List[(TermName, Type)]] = for {
      constructor <- accessiblePrimaryCtorOf(tpe)
      if !isNonGeneric(constructor)
      Seq(params) <- Option(nonImplicitParamLists(constructor.typeSignatureIn(tpe)))
      aligned <- alignFields(tpe, for (param <- params)
        yield param.name.toTermName -> param.typeSignature)
    } yield aligned
  }

  object HasApplyUnapply {
    def apply(tpe: Type): Boolean = unapply(tpe).isDefined
    def unapply(tpe: Type): Option[List[(TermName, Type)]] =
      (tpe, tpe) match {
        case (HasApply(as), HasUnapply(bs)) if equalTypes(as.map(_._2), bs) => Some(as)
        case _ => None
      }
  }

  object HasCtorUnapply {
    def apply(tpe: Type): Boolean = unapply(tpe).isDefined
    def unapply(tpe: Type): Option[List[(TermName, Type)]] =
      (tpe, tpe) match {
        case(HasUniqueCtor(as), HasUnapply(bs)) if equalTypes(as.map(_._2), bs) => Some(as)
        case _ => None
      }
  }

  @deprecated("Use CtorDtor.fromTo instead", "2.3.9")
  trait CtorDtor {
    def construct(args: List[Tree]): Tree
    def binding: (Tree, List[Tree])
    def reprBinding: (Tree, List[Tree])
  }

  object CtorDtor {
    @deprecated("Use CtorDtor.fromTo instead", "2.3.9")
    def apply(tpe: Type): CtorDtor = new CtorDtor {
      private[this] val (from, to) =
        fromTo(tpe, TypeTree(tpe))
      private[this] val (funs, argss) =
        from.body.collect { case Apply(fun, args) => (fun, args) }.reverse.unzip
      def construct(args: List[Tree]): Tree =
        funs.headOption.fold(from.body)(ctr => q"$ctr(...${args :: argss.drop(1)})")
      val binding: (Tree, List[Tree]) =
        (from.pat, argss.headOption.getOrElse(Nil))
      val reprBinding: (Tree, List[Tree]) =
        (to.pat, to.body.collect { case Apply(_, List(arg, _*)) => arg })
    }

    final def fromTo(tpe: Type, reprTpt: Tree): (CaseDef, CaseDef) = {
      import c.internal.gen

      val wildcard = Ident(termNames.WILDCARD)
      // like pq"_*" except that it does work
      val repWCard = Star(wildcard)

      def narrow(tree: Tree, tpe: Type): Tree = tpe match {
        case ConstantType(c) => q"$c.asInstanceOf[$tpe]"
        case _ => tree
      }

      def const(tree: Tree): CaseDef =
        cq"_ => $tree"

      def mkHListPattern(elems: List[TermName]): Tree = {
        val cons = objectRef[::.type]
        elems.foldRight(objectRef[HNil.type]) {
          case (elem, acc) => pq"$cons($elem, $acc)"
        }
      }

      def from(fields: List[(TermName, Type)])(construct: List[Tree] => Tree): CaseDef = {
        val (pats, args) = fields.map { case (field, tpe) =>
          val pat = c.freshName(field)
          (pat, if (isVararg(tpe)) q"$pat: _*" else narrow(q"$pat", tpe))
        }.unzip
        cq"${mkHListPattern(pats)} => ${construct(args)}"
      }

      def to(pattern: Tree, args: List[Tree]): CaseDef =
        cq"$pattern => ${mkHListValue(args)}.asInstanceOf[$reprTpt]"

      def fromApply(fields: List[(TermName, Type)]): CaseDef = from(fields) { args =>
        val nonCaseArgs = List.fill(numNonCaseParamLists(tpe))(List.empty[Tree])
        q"${companionRef(tpe)}[..${tpe.typeArgs}](..$args)(...$nonCaseArgs)"
      }

      def fromConstructor(fields: List[(TermName, Type)]): CaseDef =
        from(fields)(args => q"new $tpe(..$args)")

      def toUnapply(fields: List[(TermName, Type)]): CaseDef = {
        val (pats, args) = fields.map { case (field, tpe) =>
          val pat = c.freshName(field)
          (Bind(pat, if (isVararg(tpe)) repWCard else wildcard), narrow(Ident(pat), tpe))
        }.unzip
        to(pq"${companionRef(tpe)}(..$pats)", args)
      }

      def toGetters(fields: List[(TermName, Type)]): CaseDef = {
        val pattern = c.freshName(TermName("x"))
        to(pq"$pattern", fields.map { case (field, tpe) => narrow(q"$pattern.$field", tpe) })
      }

      lowerKind(tpe) match {
        // case 1: Unit
        case tpe if tpe =:= typeOf[Unit] =>
          (const(q"()"), const(objectRef[HNil.type]))
        // case 2: singleton
        case tpe if isCaseObjectLike(tpe.typeSymbol.asClass) =>
          val singleton = tpe match {
            case SingleType(pre, sym) => gen.mkAttributedRef(pre, sym)
            case TypeRef(pre, sym, Nil) if sym.isModule => gen.mkAttributedRef(pre, sym.asModule)
            case TypeRef(pre, sym, Nil) if sym.isModuleClass => gen.mkAttributedRef(pre, sym.asClass.module)
            case _ => abort(s"Bad case object-like type $tpe")
          }
          (const(singleton), const(objectRef[HNil.type]))
        // case 3: case class
        case tpe if tpe.typeSymbol.asClass.isCaseClass =>
          val companion = patchedCompanionSymbolOf(tpe.typeSymbol)
          val unapply = companion.typeSignature.member(TermName("unapply"))
          val fields = fieldsOf(tpe)
          (fromApply(fields), if (unapply.isSynthetic) toUnapply(fields) else toGetters(fields))
        // case 4: exactly one matching public apply/unapply
        case HasApplyUnapply(args) =>
          (fromApply(args), toUnapply(args))
        // case 5: concrete, exactly one public constructor with matching public unapply
        case HasCtorUnapply(args) =>
          (fromConstructor(args), toUnapply(args))
        // case 6: concrete, exactly one public constructor with matching accessible fields
        case HasUniqueCtor(args) =>
          (fromConstructor(args), toGetters(args))
        case _ =>
          abort(s"Bad product type $tpe")
      }
    }
  }
}

class GenericMacros(val c: whitebox.Context) extends CaseClassMacros {
  import c.universe._

  private val generic = objectRef[Generic.type]

  def materialize[T: WeakTypeTag, R: WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[T].dealias
    if (isReprType(tpe))
      abort("No Generic instance available for HList or Coproduct")

    if (isProduct(tpe)) mkProductGeneric(tpe)
    else mkCoproductGeneric(tpe)
  }

  def mkProductGeneric(tpe: Type): Tree = {
    val repr = mkHListTpe(fieldsOf(tpe).map(_._2))
    val (from, to) = CtorDtor.fromTo(tpe, TypeTree(repr))
    q"$generic.instance[$tpe, $repr]({ case $to }, { case $from })"
  }

  def mkCoproductGeneric(tpe: Type): Tree = {
    def mkCoproductCases(tpe0: Type, index: Int): Tree = tpe0 match {
      case TypeRef(pre, sym, Nil) if sym.isModuleClass =>
        cq"p if p eq ${mkAttributedRef(pre, sym.asClass.module)} => $index"
      case singleton: SingleType =>
        cq"p if p eq ${mkAttributedRef(singleton)} => $index"
      case _ =>
        cq"_: $tpe0 => $index"
    }

    val coproduct = objectRef[Coproduct.type]
    val ctors = ctorsOf(tpe)
    val repr = mkCoproductTpe(ctors)
    val toCases = ctors.zipWithIndex.map((mkCoproductCases _).tupled)
    val to = q"$coproduct.unsafeMkCoproduct((p: @_root_.scala.unchecked) match { case ..$toCases }, p).asInstanceOf[$repr]"
    q"$generic.instance[$tpe, $repr]((p: $tpe) => $to, $coproduct.unsafeGet(_).asInstanceOf[$tpe])"
  }

  def mkIsTuple[T: WeakTypeTag]: Tree = {
    val tTpe = weakTypeOf[T]
    if (!isTuple(tTpe))
      abort(s"Unable to materialize IsTuple for non-tuple type $tTpe")

    q"new ${weakTypeOf[IsTuple[T]]}"
  }

  def mkHasProductGeneric[T: WeakTypeTag]: Tree = {
    val tTpe = weakTypeOf[T]
    if (isReprType(tTpe) || !isProduct(tTpe))
      abort(s"Unable to materialize HasProductGeneric for $tTpe")

    q"new ${weakTypeOf[HasProductGeneric[T]]}"
  }

  def mkHasCoproductGeneric[T: WeakTypeTag]: Tree = {
    val tTpe = weakTypeOf[T]
    if (isReprType(tTpe) || !isCoproduct(tTpe))
      abort(s"Unable to materialize HasCoproductGeneric for $tTpe")

    q"new ${weakTypeOf[HasCoproductGeneric[T]]}"
  }
}
