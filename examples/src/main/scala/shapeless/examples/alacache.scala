package shapeless
package examples

import java.lang.ref.WeakReference
import java.util.concurrent.ConcurrentHashMap
import scala.reflect.ClassTag

import ops.hlist.{ Length, Tupler }
import ops.nat.ToInt

import test._

import java.util.WeakHashMap

/**
 * A la carte facet that uses a `WeakHashMap` to cache instances
 * during construction and pattern matching.
 *
 * The memory cost is 2 references (`WeakReference`s) per instance and
 * 3 references for each instance that passes through a pattern
 * matcher. An alternative implementation of the cache could probably
 * half this overhead (e.g. a variant of `WeakHashMap` with the weak
 * keys exposed as the values, a `WeakHashSet` would not be enough
 * unless it had a `get` method).
 *
 * The CPU cost is to perform the synchronized reads and insertions to
 * the cache. Note that every time `apply` is called, a new instance
 * is created, but may be thrown away if the instance is already
 * cached. The JVM will hopefully get better at the escape analysis
 * needed to immediately clean up such objects.
 *
 * The benefit is a massive reduction in heap usage in situations
 * where large numbers of identical instances are created in the JVM
 * AND a massive reduction in churn of `Some` objects during prolonged
 * periods of pattern matching (many people may find the overhead is
 * not worth the benefit).
 *
 * Note that objects may be evicted from the cache if the GC needs to
 * reclaim the space, as the GC may decide that the `WeakReference`
 * container itself is worthy of collection.
 */
trait CachedFacet extends ProductISOFacet {
  trait CachedOps extends ProductISOOps {
    // we use our own synchronization to avoid unnecessary object creation
    // in getOrElseUpdate style queries.
    private val cache = new WeakHashMap[C, WeakReference[C]]()
    private val uncache = new WeakHashMap[C, WeakReference[Some[P]]]()
    def apply(p: P): C = intern(fromProduct(p))
    // use this variant if you don't want auto-caching
    // and uncomment the method in CachedMethods
    //def apply(p: P): C = fromProduct(p)
    def unapply(c: C): Option[P] = {
      val found = uncache.synchronized {
        uncache.get(c)
      }
      if (found != null) found.get
      else {
        val extracted = Some(toProduct(c))
        val found = uncache.synchronized {
          // this could potentially hit
          uncache.put(c, new WeakReference(extracted))
        }
        if (found != null) found.get else extracted
      }
    }
    def intern(c: C): C = cache.synchronized {
      val found = cache.get(c)
      if (found != null) found.get
      else {
        cache.put(c, new WeakReference(c))
        c
      }
    }
    def alive(): Long = cache.synchronized {cache.size()}
    def aliveExtracted(): Long = uncache.synchronized {uncache.size()}
  }

  val ops: CachedOps

  trait CachedMethods { self: C =>
    // def intern: C = ops.intern(self)
  }

  trait CachedCompanion {
    @nonGeneric def apply(elems: ops.P): C = ops.apply(elems)
    @nonGeneric def unapply(s: C): Option[ops.P] = ops.unapply(s)
    @nonGeneric def alive(): Long = ops.alive()
    @nonGeneric def aliveExtracted(): Long = ops.aliveExtracted()
  }
}

trait CachedCaseClassDefns extends
  CachedFacet with
  ProductFacet with
  PolymorphicEqualityFacet with
  CopyFacet with
  ToStringFacet {

  trait CaseClassOps extends
    CachedOps with
    ProductOps with
    PolymorphicEqualityOps with
    CopyOps with
    ToStringOps

  trait CaseClassCompanion extends
    CachedCompanion

  trait CaseClass extends
    CachedMethods with
    ProductMethods with
    PolymorphicEqualityMethods with
    CopyMethods with
    ToStringMethods { self: C => }

  val ops: CaseClassOps

  def Ops[Repr0 <: HList, LRepr0 <: HList, P0 <: Product, N <: Nat]
    (implicit
      gen0: Generic.Aux[C, Repr0],
      lgen0: LabelledGeneric.Aux[C, LRepr0],
      len: Length.Aux[Repr0, N],
      toInt: ToInt[N],
      tup: Tupler.Aux[Repr0, P0],
      pgen0: Generic.Aux[P0, Repr0],
      typ0: Typeable[C],
      tag0: ClassTag[C]
    ) =
      new CaseClassOps {
        type Repr = Repr0
        type LRepr = LRepr0
        type P = P0
        val gen = gen0
        val lgen = lgen0
        val pgen = pgen0
        val typ = typ0
        val tag = tag0
        val productPrefix = tag0.runtimeClass.getName.split("(\\.|\\$)").last
        val productArity = toInt()
      }
}


/**
 * Demo of a Shapeless a la carte case class with interning.
 *
 * shapeless-examples/runMain shapeless.examples.ALaCacheDemo
 */
object ALaCacheDemo extends App {
  object FooDefns extends CachedCaseClassDefns {
    type C = Foo
    val ops = Ops
    object Foo extends CaseClassCompanion
    // keep the constructor private so everybody has to go through .apply
    class Foo private[FooDefns] (val i: Int, val s: String) extends CaseClass
  }
  import FooDefns._

  // Companion apply
  val foo = Foo(23, "foo")
  typed[Foo](foo)

  val fooAlt = Foo(23, "foo")

  // identity equals, not just ==
  assert(foo == fooAlt)
  assert(foo eq fooAlt)
  assert(Foo.alive() == 1)

  // Companion unapply
  val Foo(i, s) = foo
  typed[Int](i)
  typed[String](s)
  val Foo(i2, s2) = foo

  assert(s eq s2)
  assert(Foo.aliveExtracted == 1)

  // product defns
  val foo_1 = foo.productElement(0)
  typed[Any](foo_1)
  assert(23 == foo_1)

  val foo_2 = foo.productElement(1)
  typed[Any](foo_2)
  assert("foo" == foo_2)

  val fooIterator = foo.productIterator
  assert(List(23, "foo") == fooIterator.toList)

  val fooPrefix = foo.productPrefix
  assert("Foo" == fooPrefix)

  val fooArity = foo.productArity
  assert(2 == fooArity)

  // polymorphic equality
  val foo2 = Foo(23, "foo")
  val foo3 = Foo(13, "bar")
  assert(foo == foo2)
  assert(foo.hashCode == foo2.hashCode)
  assert(foo != foo3)

  // copy
  val fooCopy = foo.copy()
  assert(fooCopy ne foo)
  assert(foo == fooCopy)
  assert(foo.hashCode == fooCopy.hashCode)

  val mod = Foo(13, "foo")
  val fooMod = foo.copy(i = 13)
  assert(fooMod ne foo)
  assert(mod == fooMod)
  assert(mod.hashCode == fooMod.hashCode)

  // toString
  val fooStr = foo.toString
  assert("Foo(23,foo)" == fooStr)
}
