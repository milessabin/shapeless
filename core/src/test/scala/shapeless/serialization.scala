/*
 * Copyright (c) 2015 Miles Sabin
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

import java.io._

import org.junit.Test
import org.junit.Assert._

import scala.collection.generic.CanBuildFrom

import labelled._
import nat._
import ops.function._
import ops.nat._
import ops.traversable._
import poly.{ ~>> }
import record._
import syntax.{ CoproductOps, GenericZipperOps, HListOps, HListZipperOps, NatOps, RecordOps, TypeableOps, UnionOps }
import syntax.std.TupleOps
import syntax.singleton._
import syntax.zipper._
import test._
import union._

object SerializationTestDefns {
  def serializable[M](m: M): Boolean = {
    val baos = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(baos)
    var ois: ObjectInputStream = null
    try {
      oos.writeObject(m)
      oos.close()
      val bais = new ByteArrayInputStream(baos.toByteArray())
      ois = new ObjectInputStream(bais)
      val m2 = ois.readObject() // just ensure we can read it back
      ois.close()
      true
    } catch {
      case thr: Throwable =>
        thr.printStackTrace
        false
    } finally {
      oos.close()
      if (ois != null) ois.close()
    }
  }

  def assertSerializable[T](t: T): Unit = assertTrue(serializable(t))

  def assertSerializableBeforeAfter[T, U](t: T)(op: T => U): Unit = {
    assertSerializable(t)
    op(t)
    assertSerializable(t)
  }

  object isDefined extends (Option ~>> Boolean) {
    def apply[T](o : Option[T]) = o.isDefined
  }

  object combineL extends Poly2 {
    implicit def ci = at[Int, Int]((acc, i) => acc+i)
    implicit def cs = at[Int, String]((acc, s) => acc+s.length)
    implicit def cb = at[Int, Boolean]((acc, b) => acc+(if(b) 1 else 0))
  }

  object combineR extends Poly2 {
    implicit def ci = at[Int, Int]((i, acc) => acc+i)
    implicit def cs = at[String, Int]((s, acc) => acc+s.length)
    implicit def cb = at[Boolean, Int]((b, acc) => acc+(if(b) 1 else 0))
  }

  object selInt extends Poly1 {
    implicit def ci = at[Int] { x => x }
  }

  object smear extends Poly {
    implicit val caseIntInt    = use((x: Int, y: Int) => x + y)
    implicit val caseStringInt = use((x: String, y: Int) => x.toInt + y)
    implicit val caseIntString = use((x: Int, y: String) => x + y.toInt)
  }

  object coIdentity extends Poly1 {
    implicit def default[A] = at[A](a => Coproduct[A :+: CNil](a))
  }

  object gsize extends Poly1 {
    implicit def caseInt = at[Int](_ => 1)
    implicit def caseString = at[String](_.length)
    implicit def default[T] = at[T](_ => 1)
  }

  object plus extends Poly2 {
    implicit val caseInt = at[Int, Int](_ + _)
    implicit val caseDouble = at[Double, Double](_ + _)
    implicit val caseString = at[String, String](_ + _)
    implicit def caseList[T] = at[List[T], List[T]](_ ::: _)
  }

  trait Quux
  class Foo extends Quux
  class Bar extends Quux
  class Baz extends Quux

  trait TC1[F[_]]
  object TC1 extends TC10 {
    implicit def tc1Id: TC1[Id] = new TC1[Id] {}
  }

  trait TC10 {
    implicit def tc1[F[_]]: TC1[F] = new TC1[F] {}
  }

  object Sing extends Serializable

  case class Wibble(i: Int, s: String)

  case class Box[T](t: T)

  type K = HList.`'a, 'b, 'c`.T
  type R = Record.`'a -> Int, 'b -> String, 'c -> Boolean`.T
  type U = Union.`'a -> Int, 'b -> String, 'c -> Boolean`.T
  type RM = Record.`'c -> Boolean, 'd -> Double`.T
  type KA = Witness.`'a`.T
  type KB = Witness.`'b`.T
  type KC = Witness.`'c`.T

  sealed trait Tree[T]
  case class Leaf[T](t: T) extends Tree[T]
  case class Node[T](l: Tree[T], r: Tree[T]) extends Tree[T]

  trait Functor[F[_]] extends Serializable {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  object Functor extends Functor0 {
    def apply[F[_]](implicit f: Lazy[Functor[F]]): Functor[F] = f.value

    implicit val idFunctor: Functor[Id] =
      new Functor[Id] {
        def map[A, B](a: A)(f: A => B): B = f(a)
      }

    // Induction step for products
    implicit def hcons[F[_]](implicit ihc: IsHCons1[F, Functor, Functor]): Functor[F] =
      new Functor[F] {
        def map[A, B](fa: F[A])(f: A => B): F[B] = {
          val (hd, tl) = ihc.unpack(fa)
          ihc.pack((ihc.fh.map(hd)(f), ihc.ft.map(tl)(f)))
        }
      }

    // Induction step for coproducts
    implicit def ccons[F[_]](implicit icc: IsCCons1[F, Functor, Functor]): Functor[F] =
      new Functor[F] {
        def map[A, B](fa: F[A])(f: A => B): F[B] =
          icc.pack(icc.unpack(fa).fold(hd => Left(icc.fh.map(hd)(f)), tl => Right(icc.ft.map(tl)(f))))
      }

    implicit def generic[F[_]](implicit gen: Generic1[F, Functor]): Functor[F] =
      new Functor[F] {
        def map[A, B](fa: F[A])(f: A => B): F[B] =
          gen.from(gen.fr.map(gen.to(fa))(f))
      }
  }

  trait Functor0 {
    implicit def constFunctor[T]: Functor[Const[T]#λ] =
      new Functor[Const[T]#λ] {
        def map[A, B](t: T)(f: A => B): T = t
      }
  }

  trait Show[T] extends Serializable {
    def show(t: T): String
  }

  object Show extends LabelledTypeClassCompanion[Show] {
    implicit def stringShow: Show[String] = new Show[String] {
      def show(t: String) = t
    }

    implicit def intShow: Show[Int] = new Show[Int] {
      def show(n: Int) = n.toString
    }

    object typeClass extends LabelledTypeClass[Show] {
      def emptyProduct = new Show[HNil] {
        def show(t: HNil) = ""
      }

      def product[F, T <: HList](name: String, sh: Show[F], st: Show[T]) = new Show[F :: T] {
        def show(ft: F :: T) = {
          val head = sh.show(ft.head)
          val tail = st.show(ft.tail)
          if (tail.isEmpty)
            s"$name = $head"
          else
            s"$name = $head, $tail"
        }
      }

      def emptyCoproduct = new Show[CNil] {
        def show(t: CNil) = ""
      }

      def coproduct[L, R <: Coproduct](name: String, sl: => Show[L], sr: => Show[R]) = new Show[L :+: R] {
        def show(lr: L :+: R) = lr match {
          case Inl(l) => s"$name(${sl.show(l)})"
          case Inr(r) => s"${sr.show(r)}"
        }
      }

      def project[F, G](instance: => Show[G], to: F => G, from: G => F) = new Show[F] {
        def show(f: F) = instance.show(to(f))
      }
    }
  }

  /**
   * A `CanBuildFrom` for `List` implementing `Serializable`, unlike the one provided by the standard library.
   */
  implicit def listSerializableCanBuildFrom[T]: CanBuildFrom[List[T], T, List[T]] =
    new CanBuildFrom[List[T], T, List[T]] with Serializable {
      def apply(from: List[T]) = from.genericBuilder[T]
      def apply() = List.newBuilder[T]
    }

}

class SerializationTests {
  import SerializationTestDefns._

  @Test
  def testStructures {
    val l = 23 :: "foo" :: true :: HNil

    type ISB = Int :+: String :+: Boolean :+: CNil
    val ci = Coproduct[ISB](23)
    val cs = Coproduct[ISB]("foo")
    val cb = Coproduct[ISB](true)

    val r = 'foo ->> 23 :: 'bar ->> "foo" :: 'baz ->> true :: HNil

    assertSerializable(HNil)
    assertSerializable(l)

    assertSerializable(ci)
    assertSerializable(cs)
    assertSerializable(cb)

    assertSerializable(r)
  }

  @Test
  def testSyntax {
    val l = 23 :: "foo" :: true :: HNil

    type ISB = Int :+: String :+: Boolean :+: CNil
    val cs = Coproduct[ISB]("foo")

    val r = 'foo ->> 23 :: 'bar ->> "foo" :: 'baz ->> true :: HNil

    type U = Union.`'foo -> Int, 'bar -> String, 'baz -> Boolean`.T
    val u = Union[U](bar = "quux")

    val t = (23, "foo", true)

    assertSerializable(new GenericZipperOps(t))

    assertSerializable(new HListOps(l))

    assertSerializable(new HListZipperOps(l))

    assertSerializable(new CoproductOps(cs))

    assertSerializable(new NatOps(_0))

    assertSerializable(new RecordOps(r))

    assertSerializable(new UnionOps(u))

    assertSerializable(new TupleOps(()))
    assertSerializable(new TupleOps(t))

    assertSerializable(new TypeableOps(23))
  }

  @Test
  def testHListOps {
    import ops.hlist._

    type L = Int :: String :: Boolean :: HNil
    type LP = String :: Boolean :: Int :: HNil
    type R = Boolean :: String :: Int :: HNil
    type LL = List[Int] :: List[String] :: List[Boolean] :: HNil
    type SL = Set[Int] :: Set[String] :: Set[Boolean] :: HNil
    type OL = Option[Int] :: Option[String] :: Option[Boolean] :: HNil
    type FL = (Int :: HNil) :: (String :: HNil) :: (Boolean :: HNil) :: HNil
    type Q = Foo :: Bar :: Baz :: HNil
    type IS = Int :: String :: HNil
    type LT = (Int, String) :: (Boolean, Double) :: (Char, Float) :: HNil
    type AL = (Int => Double) :: (String => Char) :: (Boolean => Float) :: HNil
    type I3 = Int :: Int :: Int :: HNil

    assertSerializable(IsHCons[L])

    assertSerializable(Mapped[L, List])

    assertSerializable(Comapped[LL, List])

    assertSerializable(NatTRel[LL, List, SL, Set])

    assertSerializable(HKernel[HNil])
    assertSerializable(HKernel[L])

    assertSerializable(ToCoproduct[HNil])
    assertSerializable(ToCoproduct[L])

    assertSerializable(Length[HNil])
    assertSerializable(Length[L])

    assertSerializable(Mapper[poly.identity.type, HNil])
    assertSerializable(Mapper[poly.identity.type, L])

    assertSerializable(FlatMapper[poly.identity.type, HNil])
    assertSerializable(FlatMapper[poly.identity.type, FL])

    assertSerializable(ConstMapper[Int, HNil])
    assertSerializable(ConstMapper[Int, L])

    assertSerializable(MapFolder[HNil, Boolean, isDefined.type])
    assertSerializable(MapFolder[OL, Boolean, isDefined.type])

    assertSerializable(LeftFolder[HNil, Int, combineL.type])
    assertSerializable(LeftFolder[L, Int, combineL.type])

    assertSerializable(RightFolder[HNil, Int, combineR.type])
    assertSerializable(RightFolder[L, Int, combineR.type])

    assertSerializable(LeftReducer[L, combineL.type])

    assertSerializable(RightReducer[R, combineR.type])

    assertSerializable(Unifier[HNil])
    assertSerializable(Unifier[Q])

    assertSerializable(SubtypeUnifier[HNil, Quux])
    assertSerializable(SubtypeUnifier[Q, Quux])

    assertSerializable(ToTraversable[HNil, List])
    assertSerializable(ToTraversable[L, List])

    assertSerializable(ToList[HNil, Nothing])
    assertSerializable(ToList[HNil, Int])
    assertSerializable(ToList[L, Any])

    assertSerializable(ToSized[HNil, List])
    assertSerializable(ToSized[L, List])

    assertSerializable(Tupler[HNil])
    assertSerializable(Tupler[L])

    assertSerializable(Init[L])

    assertSerializable(Last[L])

    assertSerializable(Selector[L, Int])
    assertSerializable(Selector[L, String])

    assertSerializable(Partition[HNil, Int])
    assertSerializable(Partition[L, Int])

    assertSerializable(Filter[HNil, Int])
    assertSerializable(Filter[L, Int])

    assertSerializable(FilterNot[HNil, Int])
    assertSerializable(FilterNot[L, Int])

    assertSerializable(Remove[L, Int])

    assertSerializable(RemoveAll[L, IS])

    assertSerializable(Replacer[L, Int, String])

    assertSerializable(Modifier[L, Int, String])

    assertSerializable(ReplaceAt[L, _1, Double])

    assertSerializable(At[L, _0])
    assertSerializable(At[L, _1])

    assertSerializable(Drop[L, _0])
    assertSerializable(Drop[L, _1])

    assertSerializable(Take[L, _0])
    assertSerializable(Take[L, _1])

    assertSerializable(Split[L, _0])
    assertSerializable(Split[L, _1])

    assertSerializable(ReverseSplit[L, _0])
    assertSerializable(ReverseSplit[L, _1])

    assertSerializable(SplitLeft[L, Int])
    assertSerializable(SplitLeft[L, String])

    assertSerializable(ReverseSplitLeft[L, Int])
    assertSerializable(ReverseSplitLeft[L, String])

    assertSerializable(SplitRight[L, Int])
    assertSerializable(SplitRight[L, String])

    assertSerializable(ReverseSplitRight[L, Int])
    assertSerializable(ReverseSplitRight[L, String])

    assertSerializable(Reverse[HNil])
    assertSerializable(Reverse[L])

    assertSerializable(Align[L, LP])

    assertSerializable(Prepend[HNil, L])
    assertSerializable(Prepend[L, HNil])
    assertSerializable(Prepend[L, LP])

    assertSerializable(ReversePrepend[HNil, L])
    assertSerializable(ReversePrepend[L, HNil])
    assertSerializable(ReversePrepend[L, LP])

    assertSerializable(ZipOne[L, FL])

    assertSerializable(Transposer[HNil])
    assertSerializable(Transposer[FL])

    assertSerializable(Zip[HNil])
    assertSerializable(Zip[FL])

    assertSerializable(Unzip[HNil])
    assertSerializable(Unzip[LT])

    assertSerializable(ZipApply[HNil, HNil])
    assertSerializable(ZipApply[AL, L])

    assertSerializable(ZipConst[Int, L])

    assertSerializable(ZipWith[HNil, HNil, combineR.type])
    assertSerializable(ZipWith[L, I3, combineR.type])

    assertSerializable(ZipWithKeys[HNil, HNil])
    assertSerializable(ZipWithKeys[K, L])

    assertSerializable(Collect[HNil, selInt.type])
    assertSerializable(Collect[L, selInt.type])

    assertSerializable(Ordering[HNil])
    assertSerializable(Ordering[L])

    assertSerializable(MapCons[HNil, HNil])
    assertSerializable(MapCons[L, FL])

    assertSerializable(Interleave[Int, HNil])
    assertSerializable(Interleave[Int, L])

    assertSerializable(FlatMapInterleave[Int, HNil])
    assertSerializable(FlatMapInterleave[Int, FL])

    assertSerializable(Permutations[HNil])
    assertSerializable(Permutations[L])

    assertSerializable(RotateLeft[L, _0])
    assertSerializable(RotateLeft[L, _2])

    assertSerializable(RotateRight[L, _0])
    assertSerializable(RotateRight[L, _2])

    assertSerializable(LeftScanner[HNil, Int, smear.type])
    assertSerializable(LeftScanner[IS, Int, smear.type])

    assertSerializable(RightScanner[HNil, Int, smear.type])
    assertSerializable(RightScanner[IS, Int, smear.type])

    assertSerializable(Fill[_0, Int])
    assertSerializable(Fill[_3, Int])

    assertSerializable(Patcher[_0, _1, L, IS])
  }

  @Test
  def testRecords {
    import ops.record._

    type FA = FieldType[KA, Int]
    type FB = FieldType[KB, String]
    type FC = FieldType[KC, Boolean]

    assertSerializable(Selector[R, KA])
    assertSerializable(Selector[R, KB])

    assertSerializable(SelectAll[R, HNil])
    assertSerializable(SelectAll[R, KA :: HNil])
    assertSerializable(SelectAll[R, KA :: KB :: HNil])

    assertSerializable(Updater[HNil, FA])
    assertSerializable(Updater[R, FA])
    assertSerializable(Updater[R, FB])

    assertSerializable(Merger[HNil, R])
    assertSerializable(Merger[R, HNil])
    assertSerializable(Merger[R, RM])
    assertSerializable(Merger[RM, R])

    assertSerializable(Modifier[R, KA, Int, Char])
    assertSerializable(Modifier[R, KB, String, Char])

    assertSerializable(Remover[R, KA])
    assertSerializable(Remover[R, KB])

    assertSerializable(Renamer[R, KA, KB])
    assertSerializable(Renamer[R, KC, KA])
    assertSerializable(Renamer[R, KB, KC])

    assertSerializable(Keys[HNil])
    assertSerializable(Keys[R])

    assertSerializable(Values[HNil])
    assertSerializable(Values[R])

    assertSerializable(ToMap[HNil])
    assertSerializable(ToMap[R])

    assertSerializable(MapValues[poly.identity.type, HNil])
    assertSerializable(MapValues[poly.identity.type, R])
  }

  @Test
  def testCoproducts {
    import ops.coproduct._

    type L = Int :+: String :+: Boolean :+: CNil
    type LP = String :+: Boolean :+: Int :+: CNil
    type BS = Boolean :+: String :+: CNil

    assertSerializable(Inject[L, Int])
    assertSerializable(Inject[L, String])
    assertSerializable(Inject[L, Boolean])

    assertSerializable(Selector[L, Int])
    assertSerializable(Selector[L, String])
    assertSerializable(Selector[L, Boolean])

    assertSerializable(At[L, _0])
    assertSerializable(At[L, _1])
    assertSerializable(At[L, _2])

    assertSerializable(Partition[L, Int])
    assertSerializable(Partition[L, String])

    assertSerializable(Filter[L, Int])
    assertSerializable(Filter[L, String])

    assertSerializable(FilterNot[L, Int])
    assertSerializable(FilterNot[L, String])

    assertSerializable(Remove[L, Int])
    assertSerializable(Remove[L, String])

    assertSerializable(RemoveLast[L, Int])
    assertSerializable(RemoveLast[L, String])

    assertSerializable(FlatMap[CNil, coIdentity.type])
    assertSerializable(FlatMap[L, coIdentity.type])

    assertSerializable(Mapper[poly.identity.type, CNil])
    assertSerializable(Mapper[poly.identity.type, L])

    assertSerializable(Unifier[L])

    assertSerializable(Folder[poly.identity.type, L])

    assertSerializable(ZipWithKeys[HNil, CNil])
    assertSerializable(ZipWithKeys[K, L])

    assertSerializable(Length[CNil])
    assertSerializable(Length[L])

    assertSerializable(ExtendRight[L, Int])

    assertSerializable(ExtendBy[CNil, CNil])
    assertSerializable(ExtendBy[L, CNil])
    assertSerializable(ExtendBy[L, L])

    assertSerializable(ExtendLeftBy[CNil, CNil])
    assertSerializable(ExtendLeftBy[L, CNil])
    assertSerializable(ExtendLeftBy[L, L])
    assertSerializable(ExtendLeftBy[CNil, L])

    assertSerializable(ExtendRightBy[CNil, CNil])
    assertSerializable(ExtendRightBy[L, CNil])
    assertSerializable(ExtendRightBy[L, L])

    assertSerializable(RotateLeft[CNil, _0])
    assertSerializable(RotateLeft[L, _0])
    assertSerializable(RotateLeft[L, _1])
    assertSerializable(RotateLeft[L, _2])

    assertSerializable(RotateRight[CNil, _0])
    assertSerializable(RotateRight[L, _0])
    assertSerializable(RotateRight[L, _1])
    assertSerializable(RotateRight[L, _2])

    assertSerializable(IsCCons[L])

    assertSerializable(Split[L, _0])
    assertSerializable(Split[L, _1])
    assertSerializable(Split[L, _2])

    assertSerializable(Take[L, _0])
    assertSerializable(Take[L, _1])
    assertSerializable(Take[L, _2])

    assertSerializable(Drop[L, _0])
    assertSerializable(Drop[L, _1])
    assertSerializable(Drop[L, _2])

    assertSerializable(Reverse[CNil])
    assertSerializable(Reverse[L])

    assertSerializable(Align[CNil, CNil])
    assertSerializable(Align[L, L])
    assertSerializable(Align[L, LP])

    assertSerializable(InitLast[L])

    assertSerializable(Ordering[CNil])
    assertSerializable(implicitly[PartialOrdering[CNil]])
    assertSerializable(implicitly[PartialOrdering[L]])

    assertSerializable(ToHList[CNil])
    assertSerializable(ToHList[L])

    assertSerializable(Basis[L, CNil])
    assertSerializable(Basis[L, BS])
  }

  @Test
  def testUnions {
    import ops.union._

    assertSerializable(Selector[U, KA])
    assertSerializable(Selector[U, KB])

    assertSerializable(Keys[CNil])
    assertSerializable(Keys[U])

    assertSerializable(Values[CNil])
    assertSerializable(Values[U])

    assertSerializable(ToMap[CNil])
    assertSerializable(ToMap[U])

    assertSerializable(MapValues[poly.identity.type, CNil])
    assertSerializable(MapValues[poly.identity.type, U])
  }

  @Test
  def testTuples {
    import ops.tuple._

    type L = (Int, String, Boolean)
    type LP = (String, Boolean, Int)
    type R = (Boolean, String, Int)
    type LL = (List[Int], List[String], List[Boolean])
    type SL = (Set[Int], Set[String], Set[Boolean])
    type OL = (Option[Int], Option[String], Option[Boolean])
    type FL = (Tuple1[Int], Tuple1[String], Tuple1[Boolean])
    type Q = (Foo, Bar, Baz)
    type IS = (Int, String)
    type LT = ((Int, String), (Boolean, Double), (Char, Float))
    type AL = ((Int => Double), (String => Char), (Boolean => Float))
    type I3 = (Int, Int, Int)

    assertSerializable(IsComposite[L])

    assertSerializable(Prepend[Unit, L])
    assertSerializable(Prepend[L, Unit])
    assertSerializable(Prepend[L, LP])

    assertSerializable(ReversePrepend[Unit, L])
    assertSerializable(ReversePrepend[L, Unit])
    assertSerializable(ReversePrepend[L, LP])

    assertSerializable(At[L, _0])
    assertSerializable(At[L, _1])

    assertSerializable(Init[L])

    assertSerializable(Last[L])

    assertSerializable(Selector[L, Int])
    assertSerializable(Selector[L, String])

    assertSerializable(Filter[Unit, Int])
    assertSerializable(Filter[L, Int])

    assertSerializable(FilterNot[Unit, Int])
    assertSerializable(FilterNot[L, Int])

    assertSerializable(Remove[L, Int])

    assertSerializable(RemoveAll[L, IS])

    assertSerializable(Replacer[L, Int, String])

    assertSerializable(ReplaceAt[L, _1, Double])

    assertSerializable(Modifier[L, Int, String])

    assertSerializable(Take[L, _0])
    assertSerializable(Take[L, _1])

    assertSerializable(Drop[L, _0])
    assertSerializable(Drop[L, _1])

    assertSerializable(Split[L, _0])
    assertSerializable(Split[L, _1])

    assertSerializable(ReverseSplit[L, _0])
    assertSerializable(ReverseSplit[L, _1])

    assertSerializable(SplitLeft[L, Int])
    assertSerializable(SplitLeft[L, String])

    assertSerializable(ReverseSplitLeft[L, Int])
    assertSerializable(ReverseSplitLeft[L, String])

    assertSerializable(SplitRight[L, Int])
    assertSerializable(SplitRight[L, String])

    assertSerializable(ReverseSplitRight[L, Int])
    assertSerializable(ReverseSplitRight[L, String])

    assertSerializable(Reverse[Unit])
    assertSerializable(Reverse[L])

    assertSerializable(Mapper[poly.identity.type, Unit])
    assertSerializable(Mapper[poly.identity.type, L])

    assertSerializable(FlatMapper[poly.identity.type, Unit])
    assertSerializable(FlatMapper[poly.identity.type, FL])

    assertSerializable(ConstMapper[Unit, Int])
    assertSerializable(ConstMapper[L, Int])

    assertSerializable(MapFolder[Unit, Boolean, isDefined.type])
    assertSerializable(MapFolder[OL, Boolean, isDefined.type])

    assertSerializable(LeftFolder[Unit, Int, combineL.type])
    assertSerializable(LeftFolder[L, Int, combineL.type])

    assertSerializable(RightFolder[Unit, Int, combineR.type])
    assertSerializable(RightFolder[L, Int, combineR.type])

    assertSerializable(LeftReducer[L, combineL.type])

    assertSerializable(RightReducer[R, combineR.type])

    assertSerializable(Transposer[Unit])
    assertSerializable(Transposer[FL])

    assertSerializable(ZipApply[Unit, Unit])
    assertSerializable(ZipApply[AL, L])

    assertSerializable(ZipOne[L, FL])

    assertSerializable(ZipConst[L, Int])

    assertSerializable(Unifier[Unit])
    assertSerializable(Unifier[Q])

    assertSerializable(SubtypeUnifier[Unit, Quux])
    assertSerializable(SubtypeUnifier[Q, Quux])

    assertSerializable(Length[Unit])
    assertSerializable(Length[L])

    assertSerializable(ToTraversable[Unit, List])
    assertSerializable(ToTraversable[L, List])

    assertSerializable(ToList[Unit, Nothing])
    assertSerializable(ToList[Unit, Int])
    assertSerializable(ToList[L, Any])

    assertSerializable(ToSized[Unit, List])
    assertSerializable(ToSized[L, List])

    assertSerializable(Collect[Unit, selInt.type])
    assertSerializable(Collect[L, selInt.type])

    assertSerializable(Permutations[Unit])
    assertSerializable(Permutations[L])

    assertSerializable(RotateLeft[L, _0])
    assertSerializable(RotateLeft[L, _2])

    assertSerializable(RotateRight[L, _0])
    assertSerializable(RotateRight[L, _2])

    assertSerializable(LeftScanner[Unit, Int, smear.type])
    assertSerializable(LeftScanner[IS, Int, smear.type])

    assertSerializable(RightScanner[Unit, Int, smear.type])
    assertSerializable(RightScanner[IS, Int, smear.type])

    assertSerializable(Fill[_0, Int])
    assertSerializable(Fill[_3, Int])

    assertSerializable(Patcher[_0, _1, L, IS])
  }

  @Test
  def testPoly {
    assertSerializable(poly.identity)
    assertSerializable(isDefined)
    assertSerializable(productElements)
    assertSerializable(smear)
    assertSerializable(coIdentity)
    assertSerializable(tupled)
    assertSerializable(gsize)
    assertSerializable(plus)
  }

  @Test
  def testNats {
    assertSerializable(_0)
    assertSerializable(_1)
    assertSerializable(_2)
    assertSerializable(_3)
    assertSerializable(_4)

    assertSerializable(Pred[_1])
    assertSerializable(Sum[_2, _3])
    assertSerializable(Diff[_3, _2])
    assertSerializable(Prod[_3, _2])
    assertSerializable(Div[_2, _6])
    assertSerializable(Div[_6, _2])
    assertSerializable(Mod[_5, _2])
    assertSerializable(LT[_2, _3])
    assertSerializable(LTEq[_2, _2])
    assertSerializable(Min[_2, _3])
    assertSerializable(Pow[_3, _2])
    assertSerializable(ToInt[_3])
  }

  @Test
  def testFunctions {
    assertSerializable(FnToProduct[() => String])
    assertSerializable(FnToProduct[(Int) => String])
    assertSerializable(FnToProduct[(Int, Boolean) => String])

    assertSerializable(FnFromProduct[(HNil) => String])
    assertSerializable(FnFromProduct[(Int :: HNil) => String])
    assertSerializable(FnFromProduct[(Int :: Boolean :: HNil) => String])
  }

  @Test
  def testGeneric {
    assertSerializable(Generic[(Int, String, Boolean)])
    assertSerializable(Generic[Option[Int]])

    assertSerializable(DefaultSymbolicLabelling[(Int, String, Boolean)])
    assertSerializable(DefaultSymbolicLabelling[Option[Int]])

    assertSerializable(LabelledGeneric[(Int, String, Boolean)])
    assertSerializable(LabelledGeneric[Option[Int]])
    assertSerializable(Generic1[Some, TC1])
    assertSerializable(Generic1[List, TC1])
  }

  @Test
  def testTraversable {
    type L = Int :: String :: Boolean :: HNil
    assertSerializable(FromTraversable[L])
  }

  @Test
  def testTypeable {
    assertSerializable(Typeable[Any])
    assertSerializable(Typeable[AnyRef])
    assertSerializable(Typeable[AnyVal])
    assertSerializable(Typeable[Unit])
    assertSerializable(Typeable[Int])
    assertSerializable(Typeable[Double])
    assertSerializable(Typeable[String])
    assertSerializable(Typeable[Foo])
    assertSerializable(Typeable[Witness.`3`.T])
    assertSerializable(Typeable[Witness.`"foo"`.T])
    assertSerializable(Typeable[Witness.`'foo`.T])
    assertSerializable(Typeable[Sing.type])
    assertSerializable(Typeable[Foo with Bar])
    assertSerializable(Typeable[Option[Int]])
    assertSerializable(Typeable[Either[Int, String]])
    assertSerializable(Typeable[Left[Int, String]])
    assertSerializable(Typeable[Right[Int, String]])
    assertSerializable(Typeable[List[Int]])
    assertSerializable(Typeable[Map[Int, String]])
    assertSerializable(Typeable[Wibble])
    assertSerializable(Typeable[Box[Int]])
    assertSerializable(Typeable[HNil])
    assertSerializable(Typeable[Int :: String :: Boolean :: HNil])
    assertSerializable(Typeable[CNil])
    assertSerializable(Typeable[Int :+: String :+: Boolean :+: CNil])
    assertSerializable(Typeable[Inl[Int, CNil]])
    assertSerializable(Typeable[Inr[Int, CNil]])
    assertSerializable(TypeCase[List[Int]])
  }

  @Test
  def testHMap {
    assertSerializable(HMap[(Set ~?> Option)#λ](Set("foo") -> Option("bar"), Set(23) -> Option(13)))
    assertSerializable(new (Set ~?> Option))
    assertSerializable(implicitly[(Set ~?> Option)#λ[Set[Int], Option[Int]]])
  }

  @Test
  def testLazy {
    assertSerializable(Lazy(23))

    assertSerializableBeforeAfter(implicitly[Lazy[Generic[Wibble]]])(_.value)
    assertSerializableBeforeAfter(implicitly[Lazy[Generic1[Box, TC1]]])(_.value)

    assertSerializableBeforeAfter(implicitly[Lazy[Lazy.Values[Generic[Wibble] :: HNil]]])(_.value)
    assertSerializableBeforeAfter(implicitly[Lazy[Lazy.Values[Generic[Wibble] :: Generic1[Box, TC1] :: HNil]]])(_.value)
  }

  @Test
  def testZipper {
    import ops.zipper._

    val l = 23 :: "foo" :: true :: HNil
    val t = (23, "foo", (2.0, true))

    val tz0 = t.toZipper
    type Z0 = tz0.Self
    val tz1 = tz0.right
    type Z1 = tz1.Self
    val tz2 = tz0.right.right.down
    type Z2 = tz2.Self
    val tz3 = tz0.right.right
    type Z3 = tz3.Self
    val lz0 = l.toZipper
    type Z4 = lz0.Self

    assertSerializable(l.toZipper)
    assertSerializable(t.toZipper)

    assertSerializable(Right[Z0])
    assertSerializable(Right[Z1])
    assertSerializable(Right[Z2])

    assertSerializable(Left[Z1])

    assertSerializable(First[Z0])
    assertSerializable(First[Z1])
    assertSerializable(First[Z2])

    assertSerializable(Last[Z0])
    assertSerializable(Last[Z1])
    assertSerializable(Last[Z2])

    assertSerializable(RightBy[Z0, _1])
    assertSerializable(RightBy[Z1, _1])
    assertSerializable(RightBy[Z2, _1])

    assertSerializable(LeftBy[Z0, _0])
    assertSerializable(LeftBy[Z1, _1])
    assertSerializable(LeftBy[Z2, _0])

    assertSerializable(RightTo[Z0, String])
    assertSerializable(RightTo[Z1, String])
    assertSerializable(RightTo[Z2, Boolean])

    assertSerializable(LeftTo[Z1, Int])

    assertSerializable(Up[Z2])

    assertSerializable(Down[Z3])

    assertSerializable(Root[Z0])
    assertSerializable(Root[Z3])

    assertSerializable(Get[Z2])

    assertSerializable(Put[Z2, Double])
    assertSerializable(Put[Z4, Short])

    assertSerializable(Insert[Z4, Short])

    assertSerializable(Delete[Z4])

    assertSerializable(Reify[Z2])
    assertSerializable(Reify[Z4])
  }

  @Test
  def testConstraints {
    type L = Int :: String :: Boolean :: HNil
    type OL = Option[Int] :: Option[String] :: Option[Boolean] :: HNil
    type I3 = Int :: Int :: Int :: HNil
    type IS = Int :: String :: HNil

    assertSerializable(UnaryTCConstraint[HNil, Option])
    assertSerializable(UnaryTCConstraint[OL, Option])
    assertSerializable(UnaryTCConstraint[L, Id])
    assertSerializable(UnaryTCConstraint[I3, Const[Int]#λ])

    assertSerializable(BasisConstraint[HNil, L])
    assertSerializable(BasisConstraint[IS, L])

    assertSerializable(LUBConstraint[HNil, Int])
    assertSerializable(LUBConstraint[I3, Int])

    assertSerializable(KeyConstraint[HNil, K])
    assertSerializable(KeyConstraint[R, K])

    assertSerializable(ValueConstraint[HNil, L])
    assertSerializable(ValueConstraint[R, L])
  }

  @Test
  def testSybclass {
    type L = Int :: String :: Boolean :: HNil
    type C = Int :+: String :+: Boolean :+: CNil

    assertSerializable(Data[gsize.type, Wibble, Int])
    assertSerializable(Data[gsize.type, List[Int], Int])
    assertSerializable(Data[gsize.type, List[HNil], Int])
    assertSerializable(Data[gsize.type, List[L], Int])
    assertSerializable(Data[gsize.type, List[CNil], Int])
    assertSerializable(Data[gsize.type, List[C], Int])

    assertSerializable(DataT[poly.identity.type, Wibble])
    assertSerializable(DataT[poly.identity.type, List[Int]])
    assertSerializable(DataT[poly.identity.type, List[HNil]])
    assertSerializable(DataT[poly.identity.type, List[L]])
    assertSerializable(DataT[poly.identity.type, List[CNil]])
    assertSerializable(DataT[poly.identity.type, List[C]])

    assertSerializableBeforeAfter(implicitly[Everything[gsize.type, plus.type, Wibble]])(_(Wibble(2, "a")))
    assertSerializableBeforeAfter(implicitly[Everywhere[poly.identity.type, Wibble]])(_(Wibble(2, "a")))
  }

  @Test
  def testFunctor {
    assertSerializableBeforeAfter(Functor[Some])(_.map(Some(2))(_.toString))
    assertSerializableBeforeAfter(Functor[Option])(_.map(Option(2))(_.toString))
    assertSerializableBeforeAfter(Functor[Tree])(_.map(Leaf(2))(_.toString))
    assertSerializableBeforeAfter(Functor[List])(_.map(List(2))(_.toString))
  }

  @Test
  def testShow {
    // I had to disable the first two during https://github.com/milessabin/shapeless/pull/435, with scala 2.12.0-M2.
    // Don't know why they keep capturing their outer class, and the next two don't.

    assertSerializableBeforeAfter(Show[Some[Int]])(_.show(Some(2)))
    assertSerializableBeforeAfter(Show[Option[Int]]) { show =>
      show.show(Some(2))
      show.show(None)
    }
    assertSerializable(Show[Tree[Int]])
    assertSerializable(Show[List[Int]])
  }

  @Test
  def testLenses {
    val l1 = optic[Tree[Int]]
    val l2 = optic[Tree[Int]][Node[Int]]
    val l3 = optic[Tree[Int]][Node[Int]].l
    val l4 = optic[Tree[Int]][Node[Int]].r
    val l5 = optic[Tree[Int]][Node[Int]].l[Node[Int]].r
    val l6 = optic[Tree[Int]][Node[Int]].l[Node[Int]].r[Leaf[Int]].t
    val l7 = l3 ~ l4
    val l8 = optic.hlistSelectLens[Int :: String :: Boolean :: HNil, String]
    val l9 = optic.coproductSelectPrism[Int :+: String :+: Boolean :+: CNil, String]
    val l10 = optic.hlistNthLens[Int :: String :: Boolean :: HNil, _1]
    val l11 = optic.recordLens[Record.`'foo -> Int, 'bar -> String, 'baz -> Boolean`.T]('bar)
    val l12 = optic[Tree[Int]].l.r.l.t
    val l13 = optic[Node[Int]] >> 'r
    val l14 = optic[Node[Int]] >> _1

    assertSerializable(l1)
    assertSerializable(l2)
    assertSerializable(l3)
    assertSerializable(l4)
    assertSerializable(l5)
    assertSerializable(l6)
    assertSerializable(l7)
    assertSerializable(l8)
    assertSerializable(l9)
    assertSerializable(l10)
    assertSerializable(l11)
    assertSerializable(l12)
    assertSerializable(l13)
    assertSerializable(l14)
  }

  @Test
  def testDefault {
    val d1 = Default[DefaultTestDefinitions.CC]
    val d2 = Default.AsRecord[DefaultTestDefinitions.CC]
    val d3 = Default.AsOptions[DefaultTestDefinitions.CC]

    assertSerializable(d1)
    assertSerializable(d2)
    assertSerializable(d3)
  }
}
