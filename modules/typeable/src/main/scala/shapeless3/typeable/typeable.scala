/*
 * Copyright (c) 2011-19 Miles Sabin
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

package shapeless3.typeable

import scala.compiletime._
import scala.quoted._

/**
 * Type class supporting type safe cast.
 *
 * @author Miles Sabin
 */
trait Typeable[T] extends Serializable {
  def cast(t: Any): Option[T] = if (castable(t)) Some(t.asInstanceOf[T]) else None
  def castable(t: Any): Boolean
  def describe: String
  override def toString = s"Typeable[$describe]"
}

object syntax {
  object typeable {
    implicit class Ops[T](t: T) {
      /**
       * Cast the receiver to a value of type `U` if possible. This operation
       * will be as precise wrt erasure as possible given the in-scope
       * `Typeable` instances available.
       */
      inline def cast[U](using tu: Typeable[U]): Option[U] = tu.cast(t)

      /**
       * Test whether the receiver can be cast to a value of type `U`. This
       * operation will be as precise wrt erasure as possible given the
       * in-scope `Typeable` instances available.
       */
      inline def castable[U](using tu: Typeable[U]): Boolean = tu.castable(t)

      /**
       * Cast the receiver to a value of subtype `U` of the receiver's static
       * type if possible. This operation will be as precise wrt erasure as
       * possible given the in-scope `Typeable` instances available.
       */
      inline def narrowTo[U](using ev: U <:< T, tu: Typeable[U]): Option[U] = t.cast[U]
    }
  }
}

/**
 * Provides instances of `Typeable`.
 */
object Typeable extends Typeable0 {
  import java.{ lang => jl }
  import scala.reflect.ClassTag
  import syntax.typeable.{given _}

  inline def apply[T](using tt: Typeable[T]): Typeable[T] = tt

  case class ValueTypeable[T, B](cB: Class[B], describe: String) extends Typeable[T] {
    def castable(t: Any): Boolean = t != null && cB.isInstance(t)
  }

  /** Typeable instance for `Byte`. */
  given byteTypeable as Typeable[Byte] = ValueTypeable[Byte, jl.Byte](classOf[jl.Byte], "Byte")
  /** Typeable instance for `Short`. */
  given shortTypeable as Typeable[Short] = ValueTypeable[Short, jl.Short](classOf[jl.Short], "Short")
  /** Typeable instance for `Char`. */
  given charTypeable as Typeable[Char] = ValueTypeable[Char, jl.Character](classOf[jl.Character], "Char")
  /** Typeable instance for `Int`. */
  given intTypeable as Typeable[Int] = ValueTypeable[Int, jl.Integer](classOf[jl.Integer], "Int")
  /** Typeable instance for `Long`. */
  given longTypeable as Typeable[Long] = ValueTypeable[Long, jl.Long](classOf[jl.Long], "Long")
  /** Typeable instance for `Float`. */
  given floatTypeable as Typeable[Float] = ValueTypeable[Float, jl.Float](classOf[jl.Float], "Float")
  /** Typeable instance for `Double`. */
  given doubleTypeable as Typeable[Double] = ValueTypeable[Double, jl.Double](classOf[jl.Double], "Double")
  /** Typeable instance for `Boolean`. */
  given booleanTypeable as Typeable[Boolean] = ValueTypeable[Boolean, jl.Boolean](classOf[jl.Boolean], "Boolean")
  /** Typeable instance for `Unit`. */
  given unitTypeable as Typeable[Unit] = ValueTypeable[Unit, runtime.BoxedUnit](classOf[runtime.BoxedUnit], "Unit")

  def isAnyValClass[T](clazz: Class[T]) =
    (classOf[jl.Number] isAssignableFrom clazz) ||
    clazz == classOf[jl.Boolean] ||
    clazz == classOf[jl.Character] ||
    clazz == classOf[runtime.BoxedUnit]

  /** Typeable instance for `Any`. */
  given anyTypeable as Typeable[Any] {
    def castable(t: Any): Boolean = true
    def describe = "Any"
  }

  /** Typeable instance for `AnyVal`. */
  given anyValTypeable as Typeable[AnyVal] {
    def castable(t: Any): Boolean = t != null && isAnyValClass(t.getClass)
    def describe = "AnyVal"
  }

  /** Typeable instance for `AnyRef`. */
  given anyRefTypeable as Typeable[AnyRef] {
    def castable(t: Any): Boolean = t != null && !isAnyValClass(t.getClass)
    def describe = "AnyRef"
  }

  /** Typeable instance for `Iterable`. Note that the contents be will tested
   *  for conformance to the element type.
   */
  given iterableTypeable[CC[t] <: Iterable[t], T](using CCTag: ClassTag[CC[Any]], tt: Typeable[T]) as Typeable[CC[T]] {
    def castable(t: Any): Boolean =
      t match {
        case (cc: CC[_] @unchecked) if CCTag.runtimeClass.isAssignableFrom(t.getClass) =>
          cc.forall(_.castable[T])
        case _ => false
      }
    def describe = s"${CCTag.runtimeClass.getSimpleName}[${tt.describe}]"
  }

  /** Typeable instance for `Map`. Note that the contents will be tested for
   *  conformance to the key/value types.
   */
  given mapTypeable[M[k, v] <: Map[k, v], K, V](using MTag: ClassTag[M[Any, Any]], tk: Typeable[K], tv: Typeable[V]) as Typeable[M[K, V]] {
    def castable(t: Any): Boolean =
      t match {
        case (m: Map[Any, Any] @unchecked) if MTag.runtimeClass.isAssignableFrom(t.getClass) =>
          m.forall { case (k, v) => k.castable[K] && v.castable[V] }
        case _ => false
      }
    def describe = s"${MTag.runtimeClass.getSimpleName}[${tk.describe}, ${tv.describe}]"
  }

  /** Typeable instance for simple monomorphic types */
  def namedSimpleTypeable[T](clazz: Class[T], name: String): Typeable[T] =
    new Typeable[T] {
      def castable(t: Any): Boolean = t != null && clazz.isAssignableFrom(t.getClass)
      def describe = name
    }

  /** Typeable instance for singleton value types */
  def valueSingletonTypeable[T](value: T, name: String): Typeable[T] =
    new Typeable[T] {
      def castable(t: Any): Boolean = t == value
      def describe = s"$name($value)"
    }

  /**
   * Typeable instance for singleton reference types
   *
   * @param value The singleton value
   *
   * @param name The name of the singleton
   *
   * @param serializable Whether the instance should be serializable. For
   * singleton types of object definitions and symbols, this should be true,
   * since they preserve their identity after serialization/deserialization.
   * For other cases, it should be false, since the deserialized instance
   * would lose its singleton property.
   */
  def referenceSingletonTypeable[T](value: T, name: String, serializable: Boolean): Typeable[T] =
    new Typeable[T] {
      def castable(t: Any): Boolean = t.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef]
      def describe = s"$name.type"

      @throws(classOf[java.io.IOException])
      private def writeObject(out: java.io.ObjectOutputStream): Unit = {
        if (serializable) out.defaultWriteObject()
        else throw new java.io.NotSerializableException("referenceSingletonTypeable")
      }
    }

  /** Typeable instance for intersection types with typeable conjuncts */
  def intersectionTypeable[T](parents: Seq[Typeable[_]]): Typeable[T] =
    new Typeable[T] {
      def castable(t: Any): Boolean = t != null && parents.forall(_.castable(t))
      def describe = parents.map(_.describe) mkString " & "
    }

  /** Typeable instance for union types with typeable disjuncts */
  def unionTypeable[T](parents: Seq[Typeable[_]]): Typeable[T] =
    new Typeable[T] {
      def castable(t: Any): Boolean = t != null && parents.exists(_.castable(t))
      def describe = parents.map(_.describe) mkString " | "
    }

  /** Typeable instance for polymorphic case classes with typeable elements. */
  def namedCaseClassTypeable[T](clazz: Class[T], fields: Seq[Typeable[_]], name: String): Typeable[T] =
    new Typeable[T] {
      def castable(t: Any): Boolean =
        t match {
          case p: Product if clazz.isAssignableFrom(t.getClass) =>
            fields.iterator.zip(p.productIterator).forall { case (f, p) => f.castable(p) }
          case _ => false
        }
      def describe = name
    }

  /** Typeable instance for polymorphic sums with typeable elements. */
  def namedSumTypeable[T](elems: Seq[Typeable[_]], name: String): Typeable[T] =
    new Typeable[T] {
      def castable(t: Any): Boolean = elems.exists(_.castable(t))
      def describe = name
    }
}

trait Typeable0 {
  inline def mkDefaultTypeable[T]: Typeable[T] = ${ TypeableMacros.impl[T] }

  inline given [T] as Typeable[T] = mkDefaultTypeable[T]
}

object TypeableMacros {
  import Typeable._

  def impl[T: Type](using qctx: QuoteContext): Expr[Typeable[T]] = {
    import qctx.tasty._
    import util._

    val TypeableType = typeOf[Typeable[_]] match {
      case tp: AppliedType => tp.tycon
    }

    val target = typeOf[T]

    def isAbstract(tp: Type): Boolean =
      tp.typeSymbol.isAbstractType ||
        (tp match {
          case tp: AppliedType =>
            isAbstract(tp.tycon) || tp.args.exists {
              case tp: Type => isAbstract(tp)
              case _ => false
            }
          case _ => false
        })

    def normalize(tp: Type): Type = tp match {
      case TypeBounds(lo, _) => lo
      case tp: Type => tp
    }

    def simpleName(tp: Type): String =
      normalize(tp).dealias match {
        case tp: AppliedType =>
          simpleName(tp.tycon) + tp.args.map(simpleName).mkString("[", ", ", "]")
        case TypeRef(_, name) => name
        case tp => tp.show
      }

    def collectConjuncts(tp: Type): List[Type] = tp match {
      case tp: AndType =>
        collectConjuncts(tp.left) ++ collectConjuncts(tp.right)
      case tp => List(tp)
    }

    def collectDisjuncts(tp: Type): List[Type] = tp match {
      case tp: OrType =>
        collectDisjuncts(tp.left) ++ collectDisjuncts(tp.right)
      case tp => List(tp)
    }

    def summonAllTypeables(tps: Seq[Type]): Option[Expr[Seq[Typeable[_]]]] = {
      val ttps = tps.map(tp => TypeableType.appliedTo(List(tp)))
      val instances = ttps.flatMap(ttp => searchImplicit(ttp) match {
        case iss: ImplicitSearchSuccess => List(iss.tree.seal.cast[Typeable[_]])
        case _: ImplicitSearchFailure => Nil
      })

      if (tps.length == instances.length) Some(Expr.ofSeq(instances))
      else None
    }

    def mkCaseClassTypeable = {
      val sym = target.classSymbol.get
      val fields = sym.fields
      val caseFields = sym.caseFields.filter(f => fields.contains(f))
      def fieldTpe(f: Symbol) = f.tree match {
        case tree: ValDef => tree.tpt.tpe
      }
      if (!sym.fields.forall(f => caseFields.contains(f) || !isAbstract(fieldTpe(f)))) {
        report.error(s"No Typeable for case class ${target.show} with non-case fields")
        '{???}
      } else {
        val fieldTps = caseFields.map(f => target.memberType(f))
        summonAllTypeables(fieldTps) match {
          case None =>
            report.error(s"Missing Typeable for field of case class ${target.show}")
            '{???}
          case Some(ftps) =>
            val clazz = Ref(defn.Predef_classOf).appliedToType(target).seal.cast[Class[T]]
            val name = Expr(simpleName(target))

            '{ namedCaseClassTypeable($clazz, $ftps, $name) }
        }
      }
    }

    def mkSumTypeable = {
      val r = new ReflectionUtils(qctx)
      import r._

      Mirror(target) match {
        case Some(rm) =>
          val elemTps = rm.MirroredElemTypes
          summonAllTypeables(elemTps) match {
            case None =>
              report.error(s"Missing Typeable for child of sum type ${target.show}")
              '{???}
            case Some(etps) =>
              val name = Expr(simpleName(target))

              '{ namedSumTypeable[T]($etps, $name) }
          }

        case None =>
          report.error(s"Typeable for sum type ${target.show} with no Mirror")
          '{???}
      }
    }

    def mkNamedSimpleTypeable = {
      val name = Expr(simpleName(target))
      val clazz = Ref(defn.Predef_classOf).appliedToType(target).seal.cast[Class[T]]
      '{ namedSimpleTypeable($clazz, $name) }
    }

    target.dealias match {
      case tp: TermRef =>
        val ident = Ident(tp).seal.cast[T]
        val sym = tp.termSymbol
        val name = Expr(sym.name.toString)
        val serializable = Expr(sym.flags.is(Flags.Object))
        '{ referenceSingletonTypeable[T]($ident, $name, $serializable) }

      case ConstantType(Constant(c)) =>
        val value = Literal(Constant(c)).seal.cast[T]
        val name = Expr(target.widen.typeSymbol.name.toString)
        '{ valueSingletonTypeable[T]($value, $name) }

      case tp: TypeRef =>
        val qual = tp.qualifier match {
          case tp: NoPrefix => None
          case tp: ThisType => Some(tp.tref)
          case tp: Type => Some(tp)
          case _ => None
        }

        val sym = tp.typeSymbol

        def normalizeModuleClass(sym: Symbol): Symbol =
          if (sym.flags.is(Flags.ModuleClass)) sym.companionModule else sym

        val owner = normalizeModuleClass(sym.owner)

        qual match {
          case Some(_) if sym.flags.is(Flags.Case) => mkCaseClassTypeable
          case None =>
            mkNamedSimpleTypeable
          case Some(tp: TypeRef) if normalizeModuleClass(tp.typeSymbol) == owner =>
            mkNamedSimpleTypeable
          case Some(tp: TermRef) if normalizeModuleClass(tp.termSymbol) == owner =>
            mkNamedSimpleTypeable
          case Some(_) if sym.flags.is(Flags.Sealed) => mkSumTypeable
          case _ =>
            report.error(s"No Typeable for type ${target.show} with a dependent prefix")
            '{???}
        }

      case tp: AppliedType =>
        val tycon = tp.tycon
        val args = tp.args

        if (tp.typeSymbol.flags.is(Flags.Case)) mkCaseClassTypeable
        else if (tp.typeSymbol.flags.is(Flags.Sealed)) mkSumTypeable
        else {
          report.error(s"No Typeable for parametrized type ${target.show}")
          '{???}
        }

      case tp: AndType =>
        val conjuncts = collectConjuncts(tp)
        summonAllTypeables(conjuncts) match {
          case Some(ctps) =>
            '{ intersectionTypeable($ctps) }
          case None =>
            report.error(s"No Typeable for & type ${target.show} with missing conjunct(s)")
            '{???}
        }

      case tp: OrType =>
        val disjuncts = collectDisjuncts(tp)
        summonAllTypeables(disjuncts) match {
          case Some(dtps) =>
            '{ unionTypeable($dtps) }
          case None =>
            report.error(s"No Typeable for | type ${target.show} with missing disjunct(s)")
            '{???}
        }

      case other =>
        report.error(s"No Typeable for type ${target.show}")
        '{???}
    }
  }
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
  import syntax.typeable.{given _}
  def apply[T](using tt: Typeable[T]): TypeCase[T] =
    new TypeCase[T] {
      def unapply(t: Any): Option[T] = t.cast[T]
      override def toString = s"TypeCase[${tt.describe}]"
    }
}
