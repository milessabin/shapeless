/*
 * Copyright (c) 2014 Miles Sabin
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
package examples

import labelled.FieldType

/*
 * Demo of type class derivation using Lazy for recursive and mutually
 * recursive ADTs, with specialized instances, recursion through non-ADT
 * members (List, Option) and references to ADT constructors.
 *
 * Original example due to Edward Pierzchalski,
 *
 *   https://github.com/milessabin/shapeless/issues/196
 */
object TypeClassesDemo {
  import TypeClassesDemoAux._
  import ShowSyntax._

  sealed trait ADT
  case class Ctor(s: String) extends ADT
  case class ExtCtor(b: Boolean) extends ADT
  case class ADTRec(a: ADT) extends ADT
  case class CtorRec(c: Ctor) extends ADT
  case class IndirectADTRec(s: String, as: List[ADT]) extends ADT
  case class ExtIndirectADTRec(s: String, oa: Option[ADT]) extends ADT
  case class ExtADT(a2: ADT2) extends ADT
  case class RefMutual(m: Option[Mutual]) extends ADT
  case class Show2Dep(i: Int) extends ADT

  implicit def showExtCtor: Show[ExtCtor] =
    t => s"ExtCtor!(${t.b})"

  implicit def showShow2Dep: Show[Show2Dep] =
    t => "Show2Dep: >"+implicitly[Show2[Show2Dep]].show2(t)+"<"

  sealed trait ADT2
  case class Ctor2a(s: String) extends ADT2
  case class Ctor2b(b: Boolean) extends ADT2

  sealed trait Mutual
  case class RefADT(a: ADT) extends Mutual

  def main(args: Array[String]): Unit = {
    import ExtInstances._

    //implicitly[Show[ADT]]
    //implicitly[Show[ADT2]]

    val adt: ADT =
      IndirectADTRec("top",
        List(
          Ctor("child"),
          ExtCtor(true),
          ADTRec(ExtCtor(false)),
          CtorRec(Ctor("otherchild")),
          IndirectADTRec("treeChild",
            List(
              Ctor("grandchild"),
              Ctor("otherGrandchild"),
              ExtIndirectADTRec("blah", Option(Ctor("wibble"))),
              RefMutual(Some(RefADT(Ctor("mutual")))),
              Show2Dep(23)
            )
          )
        )
      )

    val out = adt.show
    val exp =
      "IndirectADTRec(s = top, "+
        "as = List("+
          "Ctor(s = child), "+
          "ExtCtor(ExtCtor!(true)), "+
          "ADTRec(a = ExtCtor(ExtCtor!(false))), "+
          //"CtorRec(c = Ctor(s = otherchild)), "+
          "CtorRec(c = s = otherchild), "+
          "IndirectADTRec(s = treeChild, "+
            "as = List("+
              "Ctor(s = grandchild), "+
              "Ctor(s = otherGrandchild), "+
              "ExtIndirectADTRec(s = blah, oa = Ctor(s = wibble)), "+
              "RefMutual(m = RefADT(a = Ctor(s = mutual))), "+
              "Show2Dep(Show2Dep: >23<)"+
            ")"+
          ")"+
        ")"+
      ")"
    assert(out == exp)

    val adt2: ADT2 = Ctor2a("foo")
    val out2 = adt2.show
    val exp2 = "Ctor2a(s = foo)"
    assert(out2 == exp2)

    val mut: Mutual = RefADT(RefMutual(Some(RefADT(Ctor("foo")))))
    val out3 = mut.show
    val exp3 = "RefADT(a = RefMutual(m = RefADT(a = Ctor(s = foo))))"
    assert(out3 == exp3)
  }
}

object TypeClassesDemoAux {
  object ExtInstances {
    implicit def showOption[A](implicit showA: Show[A]): Show[Option[A]] =
      _.map(showA.show).getOrElse("<None>")
  }

  object ShowSyntax {
    implicit class ShowSyntaxEnrich[A](a: A)(implicit shower: Show[A]) {
      def show: String = shower.show(a)
    }
  }

  trait Show[T] {
    def show(t: T): String
  }

  object Show extends ShowLowPriority {
    def apply[T](implicit show: Show[T]): Show[T] = show
    def instance[T](f: T => String): Show[T] = f(_)

    implicit val showString: Show[String] = x => x
    implicit val showBoolean: Show[Boolean] = _.toString
    implicit def showList[A](implicit showA: => Show[A]): Show[List[A]] =
      _.map(showA.show).mkString("List(", ", ", ")")
  }

  trait ShowLowPriority {
    implicit val deriveHNil: Show[HNil] = _ => ""
    implicit val deriveCNil: Show[CNil] = _ => ""

    implicit def deriveHCons[K <: String with Singleton, V, T <: HList](
      implicit key: ValueOf[K], sv: => Show[V], st: Show[T]
    ): Show[FieldType[K, V] :: T] = { case kv :: t =>
      val head = s"${key.value} = ${sv.show(kv)}"
      val tail = st.show(t)
      if (tail.isEmpty) head else s"$head, $tail"
    }

    implicit def deriveCCons[K <: String with Singleton, V, T <: Coproduct](
      implicit key: ValueOf[K], sv: => Show[V], st: Show[T]
    ): Show[FieldType[K, V] :+: T] =  {
      case Inl(l) => s"${key.value}(${sv.show(l)})"
      case Inr(r) => st.show(r)
    }

    implicit def deriveInstance[F, G](
      implicit gen: LabelledGeneric.Aux[F, G], sg: => Show[G]
    ): Show[F] = f => sg.show(gen.to(f))
  }

  trait Show2[T] {
    def show2(t: T): String
  }

  object Show2 {
    implicit val show2Int: Show2[Int] = _.toString
    implicit val deriveHNil: Show2[HNil] = _ => ""

    implicit def deriveHCons[H, T <: HList](
      implicit sv: => Show2[H], st: Show2[T]
    ): Show2[H :: T] = { p =>
      val head = sv.show2(p.head)
      val tail = st.show2(p.tail)
      if(tail.isEmpty) head else s"$head, $tail"
    }

    implicit def deriveInstance[F, G](implicit gen: Generic.Aux[F, G], sg: => Show2[G]): Show2[F] =
      f => sg.show2(gen.to(f))
  }
}
