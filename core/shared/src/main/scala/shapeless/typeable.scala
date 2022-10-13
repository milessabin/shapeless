/*
 * Copyright (c) 2011-18 Miles Sabin
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

import scala.reflect.macros.blackbox

/**
 * Type class supporting type safe cast.
 *
 * @author Miles Sabin
 */
trait Typeable[T] extends Serializable {
  def cast(t: Any): Option[T]
  def describe: String
  override def toString = s"Typeable[$describe]"
}

trait LowPriorityTypeable {
  implicit def dfltTypeable[T]: Typeable[T] = macro TypeableMacros.dfltTypeableImpl[T]
}

/**
 * Provides instances of `Typeable`. Also provides an implicit conversion which enhances arbitrary values with a
 * `cast[T]` method.
 */
object Typeable extends TupleTypeableInstances with LowPriorityTypeable {
  import java.{ lang => jl }
  import scala.reflect.ClassTag
  import syntax.typeable._

  def apply[T](implicit castT: Typeable[T]): Typeable[T] = castT

  // This is supported by type arguments on patterns, available in Typelevel Scala 4+
  def unapply[T: Typeable](t: Any): Option[T] = t.cast[T]

  private[shapeless] def instance[T](description: => String)(f: Any => Option[T]): Typeable[T] = new Typeable[T] {
    def describe = description
    def cast(t: Any) = f(t)
  }

  case class ValueTypeable[T, B](cB: Class[B], describe: String) extends Typeable[T] {
    def cast(t: Any): Option[T] = if (t != null && cB.isInstance(t)) Some(t.asInstanceOf[T]) else None
  }

  /** Typeable instance for `Byte`. */
  implicit val byteTypeable: Typeable[Byte] = ValueTypeable[Byte, jl.Byte](classOf[jl.Byte], "Byte")
  /** Typeable instance for `Short`. */
  implicit val shortTypeable: Typeable[Short] = ValueTypeable[Short, jl.Short](classOf[jl.Short], "Short")
  /** Typeable instance for `Char`. */
  implicit val charTypeable: Typeable[Char] = ValueTypeable[Char, jl.Character](classOf[jl.Character], "Char")
  /** Typeable instance for `Int`. */
  implicit val intTypeable: Typeable[Int] = ValueTypeable[Int, jl.Integer](classOf[jl.Integer], "Int")
  /** Typeable instance for `Long`. */
  implicit val longTypeable: Typeable[Long] = ValueTypeable[Long, jl.Long](classOf[jl.Long], "Long")
  /** Typeable instance for `Float`. */
  implicit val floatTypeable: Typeable[Float] = ValueTypeable[Float, jl.Float](classOf[jl.Float], "Float")
  /** Typeable instance for `Double`. */
  implicit val doubleTypeable: Typeable[Double] = ValueTypeable[Double, jl.Double](classOf[jl.Double], "Double")
  /** Typeable instance for `Boolean`. */
  implicit val booleanTypeable: Typeable[Boolean] = ValueTypeable[Boolean, jl.Boolean](classOf[jl.Boolean], "Boolean")
  /** Typeable instance for `Unit`. */
  implicit val unitTypeable: Typeable[Unit] = ValueTypeable[Unit, runtime.BoxedUnit](classOf[runtime.BoxedUnit], "Unit")

  def isValClass[T](clazz: Class[T]) =
    clazz == classOf[jl.Byte] ||
    clazz == classOf[jl.Short] ||
    clazz == classOf[jl.Integer] ||
    clazz == classOf[jl.Long] ||
    clazz == classOf[jl.Float] ||
    clazz == classOf[jl.Double] ||
    clazz == classOf[jl.Boolean] ||
    clazz == classOf[jl.Character] ||
    clazz == classOf[runtime.BoxedUnit]

  /** Typeable instance for `Any`. */
  implicit val anyTypeable: Typeable[Any] =
    instance("Any")(Some.apply)

  /** Typeable instance for `AnyVal`. */
  implicit val anyValTypeable: Typeable[AnyVal] = instance("AnyVal") { t =>
    if (t != null && isValClass(t.getClass)) Some(t.asInstanceOf[AnyVal]) else None
  }

  /** Typeable instance for `AnyRef`. */
  implicit val anyRefTypeable: Typeable[AnyRef] = instance("AnyRef") { t =>
    if (t == null || isValClass(t.getClass)) None else Some(t.asInstanceOf[AnyRef])
  }

  /** Typeable instance for simple monomorphic types */
  def simpleTypeable[T](erased: Class[T]): Typeable[T] =
    namedSimpleTypeable(erased, safeSimpleName(erased))

  /** Typeable instance for simple monomorphic types, specifying the name explicitly */
  def namedSimpleTypeable[T](erased: Class[T], name: => String): Typeable[T] = instance(name) { t =>
    if (t != null && erased.isInstance(t)) Some(t.asInstanceOf[T]) else None
  }

  /** Typeable instance defined by a partial function and given an explicit name */
  def partialFunctionTypeable[T](pf: PartialFunction[Any, T], name: => String): Typeable[T] =
    instance(name)(pf.lift)

  /** Typeable instance for singleton value types */
  def valueSingletonTypeable[T](value: T, name: String): Typeable[T] =
    instance(s"$name($value)")(t => if (t == value) Some(value) else None)

  /** Typeable instance for singleton reference types (not serializable by default) */
  def referenceSingletonTypeable[T <: AnyRef](value: T, name: String): Typeable[T] =
    referenceSingletonTypeable(value, name, serializable = false)

  /**
   * Typeable instance for singleton reference types
   *
   * @param value The singleton value
   *
   * @param name The name of the singleton
   *
   * @param serializable Whether the instance should be
   * serializable. For singleton types of object definitions
   * and symbols, this should be true, since they preserve
   * their identity after serialization/deserialization.
   * For other cases, it should be false, since the deserialized
   * instance wouldn't work correctly.
   */
  def referenceSingletonTypeable[T <: AnyRef](value: T, name: String, serializable: Boolean): Typeable[T] =
    new Typeable[T] {
      def describe = s"$name.type"

      def cast(t: Any): Option[T] =
        if (t.asInstanceOf[AnyRef] eq value) Some(value) else None

      @throws(classOf[java.io.IOException])
      private def writeObject(out: java.io.ObjectOutputStream): Unit =
        if (serializable) out.defaultWriteObject()
        else throw new java.io.NotSerializableException("referenceSingletonTypeable")
    }

  /** Typeable instance for intersection types with typeable parents */
  def intersectionTypeable[T](parents: Array[Typeable[_]]): Typeable[T] =
    instance(parents.map(_.describe).mkString(" with ")) { t =>
      if (t != null && parents.forall(_.cast(t).isDefined)) Some(t.asInstanceOf[T]) else None
    }
  
  /** Typeable instance for `Option`. */
  implicit def optionTypeable[T](implicit castT: Typeable[T]): Typeable[Option[T]] =
    instance(s"Option[${castT.describe}]") {
      case o: Option[_] =>
        if (o.isEmpty) Some(o.asInstanceOf[Option[T]])
        else for (e <- o; _ <- e.cast[T]) yield o.asInstanceOf[Option[T]]
      case _ =>
        None
    }

  /** Typeable instance for `Either`. */
  implicit def eitherTypeable[A, B](
    implicit castA: Typeable[A], castB: Typeable[B]
  ): Typeable[Either[A, B]] = instance(s"Either[${castA.describe}, ${castB.describe}]") { t =>
    t.cast[Left[A, B]] orElse t.cast[Right[A, B]]
  }

  /** Typeable instance for `Left`. */
  implicit def leftTypeable[A, B](implicit castA: Typeable[A]): Typeable[Left[A, B]] =
    instance(s"Left[${castA.describe}]") {
      case l: Left[_, _] =>
        for (_ <- l.value.cast[A]) yield l.asInstanceOf[Left[A, B]]
      case _ =>
        None
    }

  /** Typeable instance for `Right`. */
  implicit def rightTypeable[A, B](implicit castB: Typeable[B]): Typeable[Right[A, B]] =
    instance(s"Right[${castB.describe}]") {
      case r: Right[_, _] =>
        for (_ <- r.value.cast[B]) yield r.asInstanceOf[Right[A, B]]
      case _ =>
        None
    }

  /** Typeable instance for `Traversable`.
   *  Note that the contents be will tested for conformance to the element type.
   */
  // Nb. the apparently redundant `with Iterable[T]` is a workaround for a
  // Scala bug which causes conflicts between this instance and `anyTypeable`.
  implicit def genTraversableTypeable[CC[X] <: Iterable[X], T](
    implicit mCC: ClassTag[CC[_]], castT: Typeable[T]
  ): Typeable[CC[T] with Iterable[T]] = instance(s"${safeSimpleName(mCC)}[${castT.describe}]") { t =>
    if (t == null) None
    else if (mCC.runtimeClass isInstance t) {
      val cc = t.asInstanceOf[CC[Any]]
      if (cc.forall(_.cast[T].isDefined)) Some(t.asInstanceOf[CC[T]]) else None
    } else None
  }

  /** Typeable instance for `Map`. Note that the contents will be tested for conformance to the key/value types. */
  implicit def genMapTypeable[M[X, Y], K, V](
    implicit ev: M[K, V] <:< Map[K, V], mM: ClassTag[M[_, _]], castK: Typeable[K], castV: Typeable[V]
  ): Typeable[M[K, V]] = instance(s"${safeSimpleName(mM)}[${castK.describe}, ${castV.describe}]") { t =>
    if (t == null) None
    else if (mM.runtimeClass isInstance t) {
      val m = t.asInstanceOf[Map[Any, Any]]
      if (m.forall(_.cast[(K, V)].isDefined)) Some(t.asInstanceOf[M[K, V]]) else None
    } else None
  }

  /** Typeable instance for polymorphic case classes with typeable elements */
  def caseClassTypeable[T](erased: Class[T], fields: Array[Typeable[_]]): Typeable[T] =
    namedCaseClassTypeable(erased, fields, safeSimpleName(erased))

  /** Typeable instance for polymorphic case classes with typeable elements, specifying the name explicitly. */
  def namedCaseClassTypeable[T](erased: Class[T], fields: Array[Typeable[_]], name: => String): Typeable[T] =
    instance(s"$name[${fields.map(_.describe).mkString(",")}]") { t =>
      if (classOf[Product].isAssignableFrom(erased) && erased.isInstance(t)) {
        val c = t.asInstanceOf[Product with T]
        val f = c.productIterator.toList
        if ((f zip fields).forall { case (f, castF) => castF.cast(f).isDefined }) Some(c) else None
      } else None
    }

  /** Typeable instance for `HNil`. */
  implicit val hnilTypeable: Typeable[HNil] = instance("HNil") { t =>
    if (t != null && t.isInstanceOf[HNil]) Some(t.asInstanceOf[HNil]) else None
  }

  /** Typeable instance for `HList`s. Note that the contents will be tested for conformance to the element types. */
  implicit def hlistTypeable[H, T <: HList](
    implicit castH: Typeable[H], castT: Typeable[T]
  ): Typeable[H :: T] = instance(s"${castH.describe} :: ${castT.describe}") {
    case l: ::[_, _] =>
      for (_ <- l.head.cast[H]; _ <- (l.tail: Any).cast[T]) yield l.asInstanceOf[H :: T]
    case _ =>
      None
  }

  /** Typeable instance for `CNil`. */
  implicit val cnilTypeable: Typeable[CNil] =
    instance("CNil")(_ => None)

  /**
   * Typeable instance for `Coproduct`s.
   * Note that the contents will be tested for conformance to one of the element types.
   */
  implicit def coproductTypeable[H, T <: Coproduct](
    implicit castH: Typeable[H], castT: Typeable[T]
  ): Typeable[H :+: T] = instance(s"${castH.describe} :+: ${castT.describe}") { t =>
    t.cast[Inl[H, T]] orElse t.cast[Inr[H, T]]
  }

  /** Typeable instance for `Inl`. */
  implicit def inlTypeable[H, T <: Coproduct](implicit castH: Typeable[H]): Typeable[Inl[H, T]] =
    instance(s"Inl[${castH.describe}}]") {
      case l: Inl[_, _] =>
        for (_ <- l.head.cast[H]) yield l.asInstanceOf[Inl[H, T]]
      case _ =>
        None
    }

  /** Typeable instance for `Inr`. */
  implicit def inrTypeable[H, T <: Coproduct](implicit castT: Typeable[T]): Typeable[Inr[H, T]] =
    instance(s"Inr[${castT.describe}}]") {
      case r: Inr[_, _] =>
        for (_ <- r.tail.cast[T]) yield r.asInstanceOf[Inr[H, T]]
      case _ =>
        None
    }

  // Workaround for https://issues.scala-lang.org/browse/SI-5425
  private def safeSimpleName(erased: Class[_]): String =
    try erased.getSimpleName
    catch { case _: InternalError => erased.getName }

  private def safeSimpleName(tag: ClassTag[_]): String =
    safeSimpleName(tag.runtimeClass)
}

/**
 * Extractor for use of `Typeable` in pattern matching.
 *
 * Thanks to Stacy Curl for the idea.
 *
 * @author Miles Sabin
 */
trait TypeCase[T] extends Serializable {
  def unapply(t: Any): Option[T]
}

object TypeCase {
  import syntax.typeable._
  def apply[T](implicit tt:Typeable[T]): TypeCase[T] = new TypeCase[T] {
    def unapply(t: Any): Option[T] = t.cast[T]
    override def toString = s"TypeCase[${tt.describe}]"
  }
}

class TypeableMacros(val c: blackbox.Context) extends SingletonTypeUtils {
  import c.universe._
  import definitions.NothingClass

  val typeableTpe: Type = typeOf[Typeable[_]].typeConstructor
  val genericTpe: Type = typeOf[Generic[_]].typeConstructor

  def dfltTypeableImpl[T: WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[T]
    val dealiased = tpe.dealias

    dealiased match {
      case t: TypeRef if t.sym == NothingClass =>
        c.abort(c.enclosingPosition, "No Typeable for Nothing")

      case ExistentialType(_, _) =>
        val tArgs = dealiased.typeArgs
        val normalized = appliedType(dealiased.typeConstructor, tArgs)
        val normalizedTypeable = c.inferImplicitValue(appliedType(typeableTpe, List(normalized)))
        if (normalizedTypeable.isEmpty)
          c.abort(c.enclosingPosition, s"No default Typeable for parametrized type $tpe")
        normalizedTypeable

      case SingletonSymbolType(c) =>
        q"""_root_.shapeless.Typeable.valueSingletonTypeable[$tpe](${mkSingletonSymbol(c)}, "Symbol")"""

      case RefinedType(parents, decls) =>
        if (decls.nonEmpty)
          c.abort(c.enclosingPosition, "No Typeable for a refinement with non-empty decls")
        val parentTypeables = parents.filterNot(_ =:= typeOf[AnyRef]).map { parent =>
          c.inferImplicitValue(appliedType(typeableTpe, List(parent)))
        }
        if (parentTypeables.exists(_.isEmpty))
          c.abort(c.enclosingPosition, "Missing Typeable for parent of a refinement")

        q"""_root_.shapeless.Typeable.intersectionTypeable(
          _root_.scala.Array[_root_.shapeless.Typeable[_]](..$parentTypeables)
        )"""

      case pTpe if pTpe.typeArgs.nonEmpty =>
        val pSym = {
          val sym = pTpe.typeSymbol
          if (!sym.isClass)
            c.abort(c.enclosingPosition, s"No default Typeable for parametrized type $tpe")

          val pSym0 = sym.asClass
          pSym0.typeSignature // Workaround for <https://issues.scala-lang.org/browse/SI-7755>

          pSym0
        }

        if(!pSym.isCaseClass)
          c.abort(c.enclosingPosition, s"No default Typeable for parametrized type $tpe")
        else
          mkCaseClassTypeable(tpe)

      case SingleType(_, v) if !v.isParameter =>
        q"""_root_.shapeless.Typeable.referenceSingletonTypeable[$tpe](
           $v.asInstanceOf[$tpe], ${nameOf(v)}, serializable = ${v.isModule}
        )"""

      case ConstantType(c) =>
        q"""_root_.shapeless.Typeable.valueSingletonTypeable[$tpe]($c.asInstanceOf[$tpe], ${nameOf(c.tpe)})"""

      // Outer#Inner is unsound in general since Inner can capture type members of Outer.
      case TypeRef(TypeRef(_, outer, args), inner, _) if !outer.isFinal || args.nonEmpty =>
        if (inner.isClass && inner.asClass.isCaseClass) mkCaseClassTypeable(tpe)
        else c.abort(c.enclosingPosition, s"No default Typeable for type projection $tpe")

      case _ =>
        val tsym = tpe.typeSymbol
        if (tsym.isStatic || tsym.isFinal || (tsym.isClass && tsym.asClass.isTrait)) {
          // scala/bug#4440 Final inner classes and traits have no outer accessor.
          q"_root_.shapeless.Typeable.namedSimpleTypeable(_root_.scala.Predef.classOf[$tpe], ${nameOf(tsym)})"
        } else {
          q"_root_.shapeless.Typeable.partialFunctionTypeable({ case x: $tpe => x }, ${nameOf(tsym)})"
        }
    }
  }

  private def mkCaseClassTypeable(tpe: Type): Tree = {
    // an unsafe accessor is one that isn't a case class accessor but has an abstract type.
    def isUnsafeAccessor(sym: TermSymbol): Boolean =
      !sym.isCaseAccessor && sym.typeSignature.typeSymbol.isAbstract && (
        sym.isVal || sym.isVar || (sym.isParamAccessor && !(sym.accessed.isTerm && sym.accessed.asTerm.isCaseAccessor))
      )

    val nonCaseAccessor = tpe.decls.exists {
      case sym: TermSymbol if isUnsafeAccessor(sym) => true
      case _ => false
    }
    if (nonCaseAccessor) {
      // there is a symbol, which is not a case accessor but a val,
      // var or param, so we won't be able to type check it safely:
      c.abort(c.enclosingPosition, s"No default Typeable for parametrized type $tpe")
    }
    val fields = tpe.decls.sorted collect {
      case sym: TermSymbol if sym.isVal && sym.isCaseAccessor => sym.typeSignatureIn(tpe)
    }
    val fieldTypeables = fields.map { field => c.inferImplicitValue(appliedType(typeableTpe, List(field))) }
    if(fieldTypeables.contains(EmptyTree))
      c.abort(c.enclosingPosition, "Missing Typeable for field of a case class")

    q"""_root_.shapeless.Typeable.namedCaseClassTypeable(
      _root_.scala.Predef.classOf[$tpe], _root_.scala.Array[_root_.shapeless.Typeable[_]](..$fieldTypeables), ${nameOf(tpe)}
    )"""
  }

  private def nameOf(sym: Symbol): String =
    sym.name.decodedName.toString

  private def nameOf(tpe: Type): String =
    nameOf(tpe.typeSymbol)
}
