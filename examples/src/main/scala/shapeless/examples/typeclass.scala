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

import test._

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

  implicit def showExtCtor: Show[ExtCtor] = new Show[ExtCtor] {
    def show(t: ExtCtor) = s"ExtCtor!(${t.b})"
  }

  implicit def showShow2Dep: Show[Show2Dep] = new Show[Show2Dep] {
    def show(t: Show2Dep) = "Show2Dep: >"+implicitly[Show2[Show2Dep]].show2(t)+"<"
  }

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
          "CtorRec(c = Ctor(s = otherchild)), "+
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
    implicit def showOption[A](implicit showA: Show[A]): Show[Option[A]] = new Show[Option[A]] {
      def show(t: Option[A]) = t.map(showA.show).getOrElse("<None>")
    }
  }

  object ShowSyntax {
    implicit class ShowSyntaxEnrich[A](a: A)(implicit shower: Show[A]) {
      def show: String = shower.show(a)
    }
  }

  trait Show[T] {
    def show(t: T): String
  }

  object Show extends SimpleTypeClassCompanion[Show] {
    implicit val showString: Show[String] = new Show[String] {
      def show(t: String) = t
    }

    implicit val showBoolean: Show[Boolean] = new Show[Boolean] {
      def show(t: Boolean) = t.toString
    }

    implicit def showList[A](implicit showA: Show[A]): Show[List[A]] = new Show[List[A]] {
      def show(t: List[A]) = t.map(showA.show).mkString("List(", ", ", ")")
    }

    object typeClass extends SimpleTypeClass with LabelledTypeClass {
      def emptyProduct = new Show[HNil] {
        def show(t: HNil) = ""
      }

      def product[F, T <: HList](name: String, CHead: Show[F], CTail: Show[T]) = new Show[F :: T] {
        def show(ft: F :: T) = {
          val head = CHead.show(ft.head)
          val tail = CTail.show(ft.tail)
          if (tail.isEmpty)
            s"$name = $head"
          else
            s"$name = $head, $tail"
        }
      }

      def emptyCoproduct = new Show[CNil] {
        def show(t: CNil) = ""
      }

      def coproduct[L, R <: Coproduct](name: String, CL: => Show[L], CR: => Show[R]) = new Show[L :+: R] {
        def show(lr: L :+: R) = lr match {
          case Inl(l) => s"$name(${CL.show(l)})"
          case Inr(r) => s"${CR.show(r)}"
        }
      }

      def project[F, G](instance: => Show[G], to: F => G, from: G => F) = new Show[F] {
        def show(f: F) = instance.show(to(f))
      }
    }
  }

  trait Show2[T] {
    def show2(t: T): String
  }

  object Show2 extends SimpleTypeClassCompanion[Show2] {
    implicit val show2Int: Show2[Int] = new Show2[Int] {
      def show2(i: Int) = i.toString
    }

    object typeClass extends SimpleTypeClass with ProductTypeClass {
      def emptyProduct = new Show2[HNil] {
        def show2(t: HNil) = ""
      }

      def product[F, T <: HList](CHead: Show2[F], CTail: Show2[T]) = new Show2[F :: T] {
        def show2(ft: F :: T) = {
          val head = CHead.show2(ft.head)
          val tail = CTail.show2(ft.tail)
          if (tail.isEmpty)
            s"$head"
          else
            s"$head, $tail"
        }
      }

      def project[F, G](instance: => Show2[G], to: F => G, from: G => F) = new Show2[F] {
        def show2(f: F) = instance.show2(to(f))
      }
    }
  }
}
