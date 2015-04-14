package shapeless


import java.util.Arrays
import test._
import scala.reflect.ClassTag
import scala.scalajs.js.annotation.JSExportDescendentClasses
import Prop.{Exception, Result}
import Gen.Parameters

@JSExportDescendentClasses
class InStringOps[A](a: => A)(implicit ev: (A) => Prop) extends Prop{
  def apply(prms: Parameters): Result = {
    try ev(a).apply(prms) catch {
      case e: Throwable => Result(status = Exception(e))
    }
  }
}

@JSExportDescendentClasses
class PropertyOpsWithProp(propName: String, prop: Prop, name:String, props: Properties) extends Properties(props.name) {
  for {(name, p) <- props.properties} property(name) = p
  property(propName) = prop
}

@JSExportDescendentClasses
abstract class SpecLite extends Properties(""){

  private var context: String = ""

  def checkAll(name: String, props: Properties) =
    for ((name2, prop) <- props.properties) yield {
      property(name + ":" + name2) = prop
    }


  def checkAll(props: Properties) = for ((name, prop) <- props.properties) yield  property(name) = prop

  implicit class PropertyOps(props: Properties) {

    def withProp(propName: String, prop: Prop) = new PropertyOpsWithProp(propName, prop, name, props)

  }

  def check(x: => Boolean): Prop = x must_== true

  def assetTrue(x: => Boolean): Prop = x must_== true

  def assertEquals[A](actual: => A, expected: => A): Prop = actual must_== expected

  def assertEquals[A](s:String, actual: => A, expected: => A): Prop = assertEquals(actual, expected)

  def assertFalse(actual: => Boolean): Prop = assertEquals[Boolean](actual, false)

  def assertTrue(actual: => Boolean): Prop = assertEquals[Boolean](actual, true)

  def assertTrue(): Prop = {}

  def assertFalse(): Prop = {}

  def assertEquals(v1: Double, v2: Double, delta:Double): Prop =
    assertTrue( Math.abs(v1 - v2) < delta)

  def assertArrayEquals(v1: Array[Object], v2: Array[Object]): Prop =
    assertTrue(Arrays.equals(v1, v2))

  def assertTypedEquals[A](expected: A, actual: A): Prop = assertEquals(expected, actual)

  def assertTypedSame[A <: AnyRef](expected: A, actual: A): Prop =  actual mustBeTheSameAs expected

  def fail(msg: String): Nothing = throw new AssertionError(msg)

  implicit class StringOps(s: String) {

    def should[A](a: => Any): Unit = {
      val saved = context
      context = s; try a finally context = saved
    }

    def ![A](a: => A)(implicit ev: (A) => Prop): Unit = in(a)

    def in[A](a: => A)(implicit ev: (A) => Prop): Unit = property(context + ":" + s) = new InStringOps(a)

  }

  implicit class AnyRefOps[A <: AnyRef](actual: => A) {

    def mustBeTheSameAs(expected: A): Unit = {
      val act = actual
      def test = expected eq act
      def koMessage = "Expected (%s) to be the same as (%s)".format(act, expected)
      if (!test)
        fail(koMessage)
    }
  }

  implicit class AnyOps[A](actual: => A) {

    def must_==(expected: A): Unit = {
     val act = actual
     def test = expected == act
     def koMessage = "Expected (%s) to equal (%s)".format(act, expected)
     if (!test)
       fail(koMessage)
    }

    def mustMatch(f: PartialFunction[A, Boolean]): Unit = {
     val act = actual
     def test = f.isDefinedAt(act) && f(act)
     def koMessage = "%s does not satisfy partial function".format(act)
     if (!test)
       fail(koMessage)
    }

    def and[B](b: => B): B = {
     actual
     b
    }

    def mustBe_<(x: Int)(implicit ev: A <:< Int) = {
     val act = actual
     def test = ev(act) < x
     def koMessage = "%s <! %s".format(actual, x)
     if (!test)
       fail(koMessage)
    }

    def mustThrowA[T <: Throwable](implicit man: ClassTag[T]): Unit = {
     val erasedClass = man.runtimeClass
     val failed:Boolean  =
       try {
         actual
         true
       } catch {
         case ex: Throwable =>
           if (!erasedClass.isInstance(ex))
             fail("wrong exception thrown, expected: " + erasedClass + " got: " + ex)
           else false
       }
       if (failed) fail("no exception thrown, expected " + erasedClass)
     }
  }

  implicit def propToProp(p: => Prop): Prop = p
  implicit def check1[T, R](result: T => R): Prop = booleanToProp(true)
  implicit def unitToProp(u: => Unit): Prop = booleanToProp({u; true})
  implicit def unitToProp2(u: Unit): Prop = booleanToProp(true)
  implicit def booleanToProp(b: => Boolean): Prop = Prop.secure(b)
}


