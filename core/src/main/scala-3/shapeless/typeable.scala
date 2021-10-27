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

import scala.annotation.tailrec
import scala.compiletime.*
import scala.quoted.*
import scala.deriving.*
import shapeless.Typeable.{instance, safeSimpleName}

trait TypeableScalaCompat {

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
  def referenceSingletonTypeable[T](value: T, name: String, serializable: Boolean): Typeable[T] =
    new Typeable[T] {
      def describe = s"$name.type"

      def cast(t: Any): Option[T] =
        if (t.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef]) Some(value) else None

      @throws(classOf[java.io.IOException])
      private def writeObject(out: java.io.ObjectOutputStream): Unit =
        if (serializable) out.defaultWriteObject()
        else throw new java.io.NotSerializableException("referenceSingletonTypeable")
    }

  /** Typeable instance for intersection types with typeable parents */
  def intersectionTypeable[T](parents: Array[Typeable[_]]): Typeable[T] =
    instance(parents.map(_.describe).mkString(" & ")) { t =>
      if (t != null && parents.forall(_.cast(t).isDefined)) Some(t.asInstanceOf[T]) else None
    }

  /** Typeable instance for union types with typeable disjuncts */
  def unionTypeable[T](parents: Seq[Typeable[_]]): Typeable[T] =
    instance(parents.map(_.describe).mkString(" | ")) { t =>
      if (t != null && parents.exists(_.cast(t).isDefined)) Some(t.asInstanceOf[T]) else None
    }

  /** Typeable instance for polymorphic case classes with typeable elements */
  def caseClassTypeable[T](erased: Class[T], fields: Array[Typeable[_]]): Typeable[T] =
    namedCaseClassTypeable(erased, fields, s"${safeSimpleName(erased)}[${fields.map(_.describe).mkString(",")}]")

  /** Typeable instance for polymorphic case classes with typeable elements, specifying the name explicitly. */
  def namedCaseClassTypeable[T](erased: Class[T], fields: Array[Typeable[_]], name: => String): Typeable[T] =
    instance(s"$name[${fields.map(_.describe).mkString(",")}]") { t =>
      if (classOf[Product].isAssignableFrom(erased) && erased.isInstance(t)) {
        val cp = t.asInstanceOf[Product]
        val ct = t.asInstanceOf[T]
        val f = cp.productIterator.toList
        if ((f zip fields).forall { case (f, castF) => castF.cast(f).isDefined }) Some(ct) else None
      } else None
    }

  /** Typeable instance for polymorphic sums with typeable elements. */
  def namedSumTypeable[T](elems: Seq[Typeable[_]], name: String): Typeable[T] =
    instance(name) { t =>
      if (elems.exists(_.cast(t).isDefined)) Some(t.asInstanceOf[T]) else None
    }
}

trait LowPriorityTypeableScalaCompat {
  inline def mkDefaultTypeable[T]: Typeable[T] = ${ LowPriorityTypeableScalaCompat.impl[T] }

  inline given [T]: Typeable[T] = mkDefaultTypeable[T]
}

object LowPriorityTypeableScalaCompat {
  import Typeable._

  def impl[T: Type](using Quotes): Expr[Typeable[T]] = {
    import quotes.reflect._
    import util._

    val TypeableType = TypeRepr.of[Typeable[_]] match {
      case tp: AppliedType => tp.tycon
    }

    val target = TypeRepr.of[T]

    def isAbstract(tp: TypeRepr): Boolean =
      tp.typeSymbol.isAbstractType ||
        (tp match {
          case tp: AppliedType =>
            isAbstract(tp.tycon) || tp.args.exists(isAbstract)
          case _ => false
        })

    def normalize(tp: TypeRepr): TypeRepr = tp match {
      case tp: TypeBounds => tp.low
      case tp => tp
    }

    def simpleName(tp: TypeRepr): String =
      normalize(tp).dealias match {
        case tp: AppliedType =>
          simpleName(tp.tycon) + tp.args.map(simpleName).mkString("[", ", ", "]")
        case TypeRef(_, name) => name
        case tp => tp.show
      }

    def collectConjuncts(tp: TypeRepr): List[TypeRepr] = tp match {
      case tp: AndType =>
        collectConjuncts(tp.left) ++ collectConjuncts(tp.right)
      case tp => List(tp)
    }

    def collectDisjuncts(tp: TypeRepr): List[TypeRepr] = tp match {
      case tp: OrType =>
        collectDisjuncts(tp.left) ++ collectDisjuncts(tp.right)
      case tp => List(tp)
    }

    def summonAllTypeables(tps: Seq[TypeRepr]): Option[Expr[Seq[Typeable[_]]]] = {
      val ttps = tps.map(tp => TypeableType.appliedTo(tp))
      val instances = ttps.flatMap(ttp => Implicits.search(ttp) match {
        case iss: ImplicitSearchSuccess => List(iss.tree.asExprOf[Typeable[_]])
        case _: ImplicitSearchFailure => Nil
      })

      if (tps.length == instances.length) Some(Expr.ofSeq(instances))
      else None
    }

    def mkCaseClassTypeable = {
      val sym = target.classSymbol.get
      val fields = sym.declaredFields
      val caseFields = sym.caseFields.filter(f => fields.contains(f))
      def fieldTpe(f: Symbol) = f.tree match {
        case tree: ValDef => tree.tpt.tpe
      }
      if (!fields.forall(f => caseFields.contains(f) || !isAbstract(fieldTpe(f)))) {
        report.throwError(s"No Typeable for case class ${target.show} with non-case fields")
      } else {
        val fieldTps = caseFields.map(f => target.memberType(f))
        summonAllTypeables(fieldTps) match {
          case None =>
            report.throwError(s"Missing Typeable for field of case class ${target.show}")
          case Some(ftps) =>
            val clazz = Ref(defn.Predef_classOf).appliedToType(target).asExprOf[Class[T]]
            val name = Expr(simpleName(target))

            '{ namedCaseClassTypeable($clazz, $ftps.toArray, $name) }
        }
      }
    }

    def mkSumTypeable = {
      val r = new ReflectionUtils(quotes)
      import r._

      Mirror(target) match {
        case Some(rm) =>
          val elemTps = rm.MirroredElemTypes
          summonAllTypeables(elemTps) match {
            case None =>
              report.throwError(s"Missing Typeable for child of sum type ${target.show}")
            case Some(etps) =>
              val name = Expr(simpleName(target))

              '{ namedSumTypeable[T]($etps, $name) }
          }

        case None =>
          report.throwError(s"Typeable for sum type ${target.show} with no Mirror")
      }
    }

    def mkNamedSimpleTypeable = {
      val name = Expr(simpleName(target))
      val clazz = Ref(defn.Predef_classOf).appliedToType(target).asExprOf[Class[T]]
      '{ namedSimpleTypeable($clazz, $name) }
    }

    target.dealias match {
      case tp: TermRef =>
        val ident = Ident(tp).asExprOf[T]
        val sym = tp.termSymbol
        val name = Expr(sym.name.toString)
        val serializable = Expr(sym.flags.is(Flags.Module))
        '{ referenceSingletonTypeable[T]($ident, $name, $serializable) }

      case ConstantType(c) =>
        val value = Literal(c).asExprOf[T]
        val name = Expr(target.widen.typeSymbol.name.toString)
        '{ valueSingletonTypeable[T]($value, $name) }

      case tp: TypeRef =>
        val qual = tp.qualifier match {
          case NoPrefix() => None
          case tp: ThisType => Some(tp.tref)
          case tp => Some(tp)
        }

        val sym = tp.typeSymbol

        def normalizeModuleClass(sym: Symbol): Symbol =
          if (sym.flags.is(Flags.Module)) sym.companionModule else sym

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
            report.throwError(s"No Typeable for type ${target.show} with a dependent prefix")
        }

      case tp: AppliedType =>
        val tycon = tp.tycon
        val args = tp.args

        if (tp.typeSymbol.flags.is(Flags.Case)) mkCaseClassTypeable
        else if (tp.typeSymbol.flags.is(Flags.Sealed)) mkSumTypeable
        else report.throwError(s"No Typeable for parametrized type ${target.show}")


      case tp: AndType =>
        val conjuncts = collectConjuncts(tp)
        summonAllTypeables(conjuncts) match {
          case Some(ctps) =>
            '{ intersectionTypeable($ctps.toArray) }
          case None =>
            report.throwError(s"No Typeable for & type ${target.show} with missing conjunct(s)")
        }

      case tp: OrType =>
        val disjuncts = collectDisjuncts(tp)
        summonAllTypeables(disjuncts) match {
          case Some(dtps) =>
            '{ unionTypeable($dtps) }
          case None =>
            report.throwError(s"No Typeable for | type ${target.show} with missing disjunct(s)")
        }

      case other =>
        report.throwError(s"No Typeable for type ${target.show}")
    }
  }
}

class ReflectionUtils[Q <: Quotes & Singleton](val q: Q) {
  given q.type = q
  import q.reflect._

  case class Mirror(
                     MirroredType: TypeRepr,
                     MirroredMonoType: TypeRepr,
                     MirroredElemTypes: Seq[TypeRepr],
                     MirroredLabel: String,
                     MirroredElemLabels: Seq[String]
                   )

  object Mirror {
    def apply(mirror: Expr[scala.deriving.Mirror]): Option[Mirror] = {
      val mirrorTpe = mirror.asTerm.tpe.widen
      for {
        mt   <- findMemberType(mirrorTpe, "MirroredType")
        mmt  <- findMemberType(mirrorTpe, "MirroredMonoType")
        mets <- findMemberType(mirrorTpe, "MirroredElemTypes")
        ml   <- findMemberType(mirrorTpe, "MirroredLabel")
        mels <- findMemberType(mirrorTpe, "MirroredElemLabels")
      } yield {
        val mets0 = tupleTypeElements(mets)
        val ConstantType(StringConstant(ml0)) = ml
        val mels0 = tupleTypeElements(mels).map { case ConstantType(StringConstant(l)) => l }
        Mirror(mt, mmt, mets0, ml0, mels0)
      }
    }

    def apply(tpe: TypeRepr): Option[Mirror] = {
      val MirrorType = TypeRepr.of[scala.deriving.Mirror]

      val mtpe = Refinement(MirrorType, "MirroredType", TypeBounds(tpe, tpe))
      val instance = Implicits.search(mtpe) match {
        case iss: ImplicitSearchSuccess => Some(iss.tree.asExprOf[scala.deriving.Mirror])
        case _: ImplicitSearchFailure => None
      }
      instance.flatMap(Mirror(_))
    }
  }

  def tupleTypeElements(tp: TypeRepr): List[TypeRepr] = {
    @tailrec def loop(tp: TypeRepr, acc: List[TypeRepr]): List[TypeRepr] = tp match {
      case AppliedType(pairTpe, List(hd: TypeRepr, tl: TypeRepr)) => loop(tl, hd :: acc)
      case _ => acc
    }
    loop(tp, Nil).reverse
  }

  def low(tp: TypeRepr): TypeRepr = tp match {
    case tp: TypeBounds => tp.low
    case tp => tp
  }

  def findMemberType(tp: TypeRepr, name: String): Option[TypeRepr] = tp match {
    case Refinement(_, `name`, tp) => Some(low(tp))
    case Refinement(parent, _, _) => findMemberType(parent, name)
    case AndType(left, right) => findMemberType(left, name).orElse(findMemberType(right, name))
    case _ => None
  }
}
