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

import scala.annotation.tailrec
import scala.language.experimental.macros
import scala.reflect.macros.{blackbox, whitebox}

trait GenericScalaCompat {

  implicit def materialize[T, R]: Generic.Aux[T, R] = macro GenericMacros.materialize[T, R]
}

trait LabelledGenericScalaCompat {

  implicit def materialize[T, R]: LabelledGeneric.Aux[T, R] =
    macro LabelledMacros.mkLabelledGeneric[T, R]
}

trait IsTupleScalaCompat {
  implicit def apply[T]: IsTuple[T] = macro GenericMacros.mkIsTuple[T]
}

trait HasProductGenericScalaCompat {
  implicit def apply[T]: HasProductGeneric[T] = macro GenericMacros.mkHasProductGeneric[T]
}

trait HasCoproductGenericScalaCompat {
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
  val c: blackbox.Context

  import c.universe._

  def abort(msg: String): Nothing =
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
      val cls = classSym(tpe)
      isCaseObjectLike(cls) || isCaseClassLike(cls) || HasApplyUnapply(tpe) || HasCtorUnapply(tpe)
    }

  def isProduct(tpe: Type): Boolean =
    tpe =:= definitions.UnitTpe || (!(tpe =:= definitions.AnyRefTpe) && isProductAux(tpe))

  def isProduct1(tpe: Type): Boolean =
    isProduct(lowerKind(tpe))

  def isCoproduct(tpe: Type): Boolean =
    tpe.typeSymbol.isClass && {
      val cls = classSym(tpe)
      (cls.isTrait || cls.isAbstract) && cls.isSealed
    }

  def ownerChain(sym: Symbol): List[Symbol] = {
    @tailrec
    def loop(sym: Symbol, acc: List[Symbol]): List[Symbol] =
      if(sym.owner == NoSymbol) acc
      else loop(sym.owner, sym :: acc)

    loop(sym, Nil)
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
    val isCaseClass = clazz.isCaseClass
    if (isCaseObjectLike(clazz) || isAnonOrRefinement(clazz)) Nil
    else tpe.decls.sorted.collect {
      case sym: TermSymbol if isCaseAccessorLike(sym, isCaseClass) =>
        (sym.name, sym.typeSignatureIn(tpe).finalResultType)
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

      /*
      def isLess(sym1: Symbol, sym2: Symbol): Boolean = {
        val global = c.universe.asInstanceOf[scala.tools.nsc.Global]
        val gSym1 = sym1.asInstanceOf[global.Symbol]
        val gSym2 = sym2.asInstanceOf[global.Symbol]
        gSym1.isLess(gSym2)
      }
      */

      /*
      def orderSyms(s1: Symbol, s2: Symbol): Boolean = {
        val fn1 = s1.fullName
        val fn2 = s2.fullName
        fn1 < fn2 || (fn1 == fn2 && isLess(s1, s2))
      }
       */

      val ctors = collectCtors(baseSym).flatMap { sym =>
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
    name.decodedName.toString.trim

  def nameAsValue(name: Name): Constant =
    Constant(nameAsString(name))

  def nameOf(tpe: Type): Name =
    tpe.typeSymbol.name

  def mkHListValue(elems: List[Tree]): Tree =
    elems.foldRight(q"_root_.shapeless.HNil": Tree) {
      case (elem, acc) => q"_root_.shapeless.::($elem, $acc)"
    }

  /**
   * Fold `items` into a type using `cons` as a type constructor.
   *
   * {{{
   *   mkCompoundTpe(hnilTpe, hconsTpe, Seq(typeOf[String], typeOf[Int])) -> String :: Int :: HNil
   * }}}
   */
  def mkCompoundTpe(nil: Type, cons: Type, items: Seq[Type]): Type =
    items.foldRight(nil) { (tpe, acc) =>
      appliedType(cons, List(devarargify(tpe), acc))
    }

  /**
   * Convert `items` to corresponding HList type.
   */
  def mkHListTpe(items: Seq[Type]): Type =
    mkCompoundTpe(hnilTpe, hconsTpe, items)

  /**
   * Convert `items` to corresponding Coproduct type.
   */
  def mkCoproductTpe(items: Seq[Type]): Type =
    mkCompoundTpe(cnilTpe, cconsTpe, items)

  def unpackHList(tpe: Type): Vector[Type] =
    unpackReprType(tpe, hnilTpe, hconsTpe)

  def unpackCoproduct(tpe: Type): Vector[Type] =
    unpackReprType(tpe, cnilTpe, cconsTpe)

  def unpackReprType(tpe: Type, nil: Type, cons: Type): Vector[Type] = {
    val consSym = cons.typeSymbol
    @tailrec def unpack(tpe: Type, acc: Vector[Type]): Vector[Type] =
      if (tpe <:< nil) acc else tpe.baseType(consSym) match {
        case TypeRef(_, _, List(head, tail)) => unpack(tail, acc :+ head)
        case _ => abort(s"$tpe is not an HList or Coproduct type")
      }

    unpack(tpe, Vector.empty)
  }

  object FieldType {
    import internal._

    private val KeyTagSym = keyTagTpe.typeSymbol

    def apply(key: Type, value: Type): Type =
      appliedType(fieldTypeTpe, key, value)

    def unapply(field: Type): Option[(Type, Type)] = field.dealias match {
      case RefinedType(List(value, TypeRef(_, KeyTagSym, List(key, _))), scope)
        if scope.isEmpty => Some(key -> value)
      case RefinedType(parents :+ TypeRef(_, KeyTagSym, List(key, value)), scope)
        if value =:= refinedType(parents, scope) => Some(key -> value)
      case _ =>
        None
    }
  }

  def findField(record: Type, key: Type): Option[(Type, Type, Int)] =
    findField(unpackHList(record), key)

  def findField(fields: Seq[Type], key: Type): Option[(Type, Type, Int)] =
    fields.iterator.zipWithIndex.collectFirst {
      case (FieldType(k, v), i) if k =:= key => (k, v, i)
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

  def mkCompoundTypTree1(nil: Type, cons: Type, items: List[Type], param: Type, arg: TypeName): Tree =
    items.foldRight(mkAttributedRef(nil): Tree) { case (tpe, acc) =>
      AppliedTypeTree(mkAttributedRef(cons), List(appliedTypTree1(tpe, param, arg), acc))
    }

  def mkHListTypTree1(items: List[Type], param: Type, arg: TypeName): Tree =
    mkCompoundTypTree1(hnilTpe, hconsTpe, items, param, arg)

  def mkCoproductTypTree1(items: List[Type], param: Type, arg: TypeName): Tree =
    mkCompoundTypTree1(cnilTpe, cconsTpe, items, param, arg)

  def param1(tpe: Type): Type =
    tpe match {
      case t if tpe.takesTypeArgs => t.typeParams.head.asType.toType
      case TypeRef(_, _, List(arg)) => arg
      case _ => NoType
    }

  def reprTypTree1(tpe: Type, arg: TypeName): Tree = {
    val param = param1(tpe)
    if(isProduct1(tpe)) mkHListTypTree1(fieldsOf(tpe).map(_._2), param, arg)
    else mkCoproductTypTree1(ctorsOf1(tpe), param, arg)
  }

  def isCaseClassLike(sym: ClassSymbol): Boolean = {
    def isConcrete = !(sym.isAbstract || sym.isTrait || sym == symbolOf[Object])
    def isFinalLike = sym.isFinal || sym.knownDirectSubclasses.isEmpty
    def ctor = for {
      ctor <- accessiblePrimaryCtorOf(sym.typeSignature)
      Seq(params) <- Option(ctor.typeSignature.paramLists)
      if params.size == fieldsOf(sym.typeSignature).size
    } yield ctor
    sym.isCaseClass || (isConcrete && isFinalLike && ctor.isDefined)
  }

  def isCaseObjectLike(sym: ClassSymbol): Boolean = sym.isModuleClass

  def isCaseAccessorLike(sym: TermSymbol, inCaseClass: Boolean): Boolean = {
    val isGetter =
      if (inCaseClass) sym.isCaseAccessor && !sym.isMethod
      else sym.isGetter && sym.isPublic && (sym.isParamAccessor || sym.isLazy)
    isGetter && !isNonGeneric(sym)
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
    val SingleType(pre, sym) = (singleton: @unchecked)
    val getter = sym.asTerm.getter.orElse(sym)
    mkAttributedRef(pre, getter)
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
    if (apply.isMethod && !isNonGeneric(apply) && isAccessible(companion, apply)) {
      val paramLists = apply.typeSignatureIn(companion).paramLists
      val numParamLists = paramLists.length
      if (numParamLists <= 1) 0
      else {
        if (paramLists.last.headOption.exists(_.isImplicit))
          numParamLists-2
        else
          numParamLists-1
      }
    } else 0
  }

  object HasApply {
    def unapply(tpe: Type): Option[List[(TermName, Type)]] = for {
      companion <- Option(patchedCompanionSymbolOf(tpe.typeSymbol).typeSignature)
      apply = companion.member(TermName("apply"))
      if apply.isMethod && !isNonGeneric(apply)
      if isAccessible(companion, apply)
      Seq(params) <- Option(apply.typeSignatureIn(companion).paramLists)
      aligned <- alignFields(tpe, for (param <- params)
        yield param.name.toTermName -> param.typeSignature)
    } yield aligned
  }

  object HasUnapply {
    def unapply(tpe: Type): Option[List[Type]] = for {
      companion <- Option(patchedCompanionSymbolOf(tpe.typeSymbol).typeSignature)
      unapply = companion.member(TermName("unapply"))
      if unapply.isMethod && !isNonGeneric(unapply)
      if isAccessible(companion, unapply)
      returnTpe <- unapply.asMethod.typeSignatureIn(companion).finalResultType
        .baseType(symbolOf[Option[_]]).typeArgs.headOption
    } yield if (returnTpe <:< typeOf[Product]) returnTpe.typeArgs else List(returnTpe)
  }

  object HasUniqueCtor {
    def unapply(tpe: Type): Option[List[(TermName, Type)]] = for {
      ctor <- accessiblePrimaryCtorOf(tpe)
      if !isNonGeneric(ctor)
      Seq(params) <- Option(ctor.typeSignatureIn(tpe).paramLists)
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

  trait CtorDtor {
    def construct(args: List[Tree]): Tree
    def binding: (Tree, List[Tree])
    def reprBinding: (Tree, List[Tree])
  }

  object CtorDtor {
    def apply(tpe: Type): CtorDtor = {
      val sym = tpe.typeSymbol
      val isCaseClass = sym.asClass.isCaseClass

      val repWCard = Star(Ident(termNames.WILDCARD))  // like pq"_*" except that it does work

      def narrow(tree: Tree, tpe: Type): Tree =
        tpe match {
          case ConstantType(c) =>
            q"$c.asInstanceOf[$tpe]"
          case _ =>
            tree
        }

      def narrow1(tree: Tree, tpe: Type): Tree =
        if(isVararg(tpe))
          q"$tree: _*"
        else
          narrow(tree, tpe)

      def mkCtorDtor0(elems0: List[(TermName, Type)]) = {
        val elems = elems0.map { case (_, tpe) => (TermName(c.freshName("pat")), tpe) }
        val pattern = pq"${companionRef(tpe)}(..${elems.map { case (binder, tpe) => if(isVararg(tpe)) pq"$binder @ $repWCard" else pq"$binder"}})"
        val reprPattern =
          elems.foldRight(q"_root_.shapeless.HNil": Tree) {
            case ((bound, _), acc) => pq"_root_.shapeless.::($bound, $acc)"
          }
        val nonCaseParamLists: List[List[Tree]] = List.fill(numNonCaseParamLists(tpe))(Nil)
        new CtorDtor {
          def construct(args: List[Tree]): Tree = q"${companionRef(tpe)}[..${tpe.typeArgs}](...${args :: nonCaseParamLists})"
          def binding: (Tree, List[Tree]) = (pattern, elems.map { case (binder, tpe) => narrow(q"$binder", tpe) })
          def reprBinding: (Tree, List[Tree]) = (reprPattern, elems.map { case (binder, tpe) => narrow1(q"$binder", tpe) })
        }
      }

      def mkCtorDtor1(elems: List[(TermName, TermName, Type)], pattern: Tree, rhs: List[Tree]) = {
        val reprPattern =
          elems.foldRight(q"_root_.shapeless.HNil": Tree) {
            case ((bound, _, _), acc) => pq"_root_.shapeless.::($bound, $acc)"
          }
        new CtorDtor {
          def construct(args: List[Tree]): Tree = q"new $tpe(..$args)"
          def binding: (Tree, List[Tree]) = (pattern, rhs)
          def reprBinding: (Tree, List[Tree]) = (reprPattern, elems.map { case (binder, _, tpe) => narrow1(q"$binder", tpe) })
        }
      }

      lowerKind(tpe) match {
        // case 1: Unit
        case tpe if tpe =:= typeOf[Unit] =>
          new CtorDtor {
            def construct(args: List[Tree]): Tree = q"()"
            def binding: (Tree, List[Tree]) = (pq"()", Nil)
            def reprBinding: (Tree, List[Tree]) = (pq"_root_.shapeless.HNil", Nil)
          }

        // case 2: singleton
        case tpe if isCaseObjectLike(tpe.typeSymbol.asClass) =>
          val singleton =
            tpe match {
              case SingleType(pre, sym) =>
                c.internal.gen.mkAttributedRef(pre, sym)
              case TypeRef(pre, sym, List()) if sym.isModule =>
                c.internal.gen.mkAttributedRef(pre, sym.asModule)
              case TypeRef(pre, sym, List()) if sym.isModuleClass =>
                c.internal.gen.mkAttributedRef(pre, sym.asClass.module)
              case _ =>
                abort(s"Bad case object-like type $tpe")
            }
          new CtorDtor {
            def construct(args: List[Tree]): Tree = q"$singleton: $tpe"
            def binding: (Tree, List[Tree]) = (pq"_: $tpe", Nil)
            def reprBinding: (Tree, List[Tree]) = (pq"_root_.shapeless.HNil", Nil)
          }

        // case 3: case class
        case tpe if isCaseClass => mkCtorDtor0(fieldsOf(tpe))

        // case 4: exactly one matching public apply/unapply
        case HasApplyUnapply(args) => mkCtorDtor0(args)

        // case 5: concrete, exactly one public constructor with matching public unapply
        case HasCtorUnapply(args) =>
          val elems = args.map { case (name, tpe) => (TermName(c.freshName("pat")), name, tpe) }
          val pattern = pq"${companionRef(tpe)}(..${elems.map { case (binder, _, tpe) => if(isVararg(tpe)) pq"$binder @ $repWCard" else pq"$binder" }})"
          val rhs = elems.map { case (binder, _, tpe) => narrow(q"$binder", tpe) }
          mkCtorDtor1(elems, pattern, rhs)

        // case 6: concrete, exactly one public constructor with matching accessible fields
        case HasUniqueCtor(args) =>
          val elems = args.map { case (name, tpe) => (TermName(c.freshName("pat")), name, tpe) }
          val binder = TermName(c.freshName("pat"))
          val pattern = pq"$binder"
          val rhs = elems.map { case (_, name, tpe) => narrow(q"$binder.$name", tpe) }
          mkCtorDtor1(elems, pattern, rhs)

        case _ => abort(s"Bad product type $tpe")
      }
    }
  }
}

class GenericMacros(val c: whitebox.Context) extends CaseClassMacros {
  import c.universe._

  private val generic = objectRef[Generic.type]

  def materialize[T: WeakTypeTag, R]: Tree = mkGeneric[T]

  def mkGeneric[T: WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[T]
    if (isReprType(tpe))
      abort("No Generic instance available for HList or Coproduct")

    if (isProduct(tpe)) mkProductGeneric(tpe)
    else mkCoproductGeneric(tpe)
  }

  def mkProductGeneric(tpe: Type): Tree = {
    val repr = mkHListTpe(fieldsOf(tpe).map(_._2))
    val ctorDtor = CtorDtor(tpe)
    val (p, ts) = ctorDtor.binding
    val to = cq"$p => ${mkHListValue(ts)}.asInstanceOf[$repr]"
    val (rp, rts) = ctorDtor.reprBinding
    val from = cq"$rp => ${ctorDtor.construct(rts)}"
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
