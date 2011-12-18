shapeless : An exploration of generic/polytypic programming in Scala
====================================================================

shapeless is an exploration of generic (aka polytypic) programming in Scala
derived from the various talks I (Miles Sabin) have given over the course
of 2011 on implementing [Scrap your boilerplate](http://goo.gl/KmfVG) and
[higher rank polymorphism](http://goo.gl/zGRQ7) in Scala.

In the new year I'll be [posting](http://www.chuusai.com/blog) a (long
overdue) series of articles on the implementation techniques used: heavily 
type class based, with essential use of dependent types at various
junctures, which together enable the relatively smooth encoding of type level
functions. In the meantime you'll find Olivera, Moors and Odersky [Type
Classes as Object and Implicits](http://goo.gl/ZbcxY) useful background
material.

In more concrete terms, selected highlights include,

* A new encoding of polymorphic function values which optionally supports
  type specific cases, and which is interoperable with Scala's ordinary
  monomorphic function values.
  
```scala
// Function from Sets to Options: no type specific cases
object choose extends (Set ~> Option) {
  def default[T](s : Set[T]) = s.headOption 
}

// Convertible to a monomorphic function value
val lo : List[Option[Int]] = List(Set(1, 3, 5), Set(2, 4, 6)) map choose // == List(Option(1), Option(2))

// Function from an arbitrary type to its 'size': type specific cases
object size extends (Id ~> Const[Int]#λ) {
  def default[T](t : T) = 1
}
implicit def sizeInt = size.λ[Int](x => 1)
implicit def sizeString = size.λ[String](s => s.length)
implicit def sizeList[T] = size.λ[List[T]](l => l.length)
implicit def sizeOption[T](implicit cases : size.λ[T]) = size.λ[Option[T]](t => 1+size(t.get))
implicit def sizeTuple[T, U](implicit st : size.λ[T], su : size.λ[U]) = size.λ[(T, U)](t => size(t._1)+size(t._2))

size(23) == 1
size("foo") == 3
size((23, "foo")) == 4
```

* A `Typeable` type class which provides a type safe cast operation.

```scala
    val a : Any = List(Vector("foo", "bar", "baz"), Vector("wibble"))
    val lvs : Option[List[Vector[String]]] = a.cast[List[Vector[String]]]
    lvs.isDefined == true

    val lvi : Option[List[Vector[Int]]] = lvs.cast[List[Vector[Int]]]
    lvi.isEmpty == true
```

* The mother of all Scala `HList`'s, which amongst other things,
    * is covariant.
    
    trait Fruit
    case class Apple() extends Fruit
    case class Pear() extends Fruit
    
    type FFFF = Fruit :: Fruit :: Fruit :: Fruit :: HNil
    type APAP = Apple :: Pear :: Apple :: Pear :: HNil
    
    val a : Apple = Apple()
    val p : Pear = Pear()
    
    val apap : APAP = a :: p :: a :: p :: HNil
    val ffff : FFFF = apap  // APAP <: FFFF 
    
    * has a map operation, applying a polymorphic function value (possibly
      with type specific cases) across its elements. This means that it
      subsumes both typical `HList`'s and also `KList`'s (`HList`'s whose
      elements share a common outer type constructor).
      
    type SISS = Set[Int] :: Set[String] :: HNil
    type OIOS = Option[Int] :: Option[String] :: HNil
    
    val sets : SISS = Set(1) :: Set("foo") :: HNil
    val opts : OIOS = sets map choose
    opts == Option(1) :: Option("foo") :: HNil 
      
    * has a zipper for traversal and persistent update.
    
    val l = 1 :: "foo" :: 3.0 :: HNil

    val l2 = l.toZipper.right.put("wibble", 45).toHList
    l2 == 1 :: ("wibble", 45) :: 3.0 :: HNil
  
    val l3 = l.toZipper.right.delete.toHList
    l3 == 1 :: 3.0 :: HNil

    val l4 = l.toZipper.last.left.insert("bar").toHList
    l4 == 1 :: "foo" :: "bar" :: 3.0 :: HNil, l5)
    
    * has a `unify` operation which converts it to an `HList` of elements
      of the least upper bound of the original types.
      
    val ffff = apap.unify // type inferred as FFFF
      
    * supports conversion to an ordinary Scala `List` of elements of the
      least upper bound of the original types.
      
    val lf = apap.toList
    lf : List[Fruit] = List(a, p, a, p)
      
    * has a `Typeable` type class instance, allowing, eg. vanilla
      `List[Any]`'s or `HList`'s with elements of type `Any` to be safely
      cast to precisely typed `HList`'s.
      
    val ffff : FFFF = apap.unify               // discard precise typing 
    val apap2 : Option[APAP] = ffff.cast[APAP] // reestablish precise typing
    apap2.get == apap  
      
* Conversions between tuples and `HList`'s, and between ordinary Scala
  functions of arbitrary arity and functions which take a single
  corresponding `HList` argument. One application of this is the `liftO`
  function which lifts an ordinary function of arbitrary arity into `Option`.
  
    // Round trip from tuple to HList and back
    val t1 = (23, "foo", 2.0, true)
    
    val l1 = t1.hlisted
    h1 == 23 :: "foo" :: 2.0 :: true :: HNil
    
    val t2 = l1.tupled
    t1 == t2
    
    // Lift these ordinary Scala function values into Option 
    val sum : (Int, Int) => Int = _ + _
    val prd : (Int, Int, Int) => Int = _ * _ * _
    
    // Nb. the liftO abstracts over the arity of its function arguments 
    val sumO = liftO(sum) // (Option[Int], Option[Int]) => Option[Int]
    val prdO = liftO(prd) // (Option[Int], Option[Int], Option[Int]) => Option[Int]

    val s1 = sumO(Some(1), Some(2))
    s1 == Option(3)

    val s2 = sumO(Some(1), None)
    s2 == None

    val p1 = prdO(Some(2), Some(3), Some(4))
    p1 == Option(24)

    val p2 = prdO(Some(2), None, Some(4))
    p2 == None
      
The last three bullets under `HList` make them dramatically more practically
useful than they are typically thought to be: normally the full type
information required to work with them is too fragile to cross subtyping or
I/O boundaries. This new `HList` implementation supports the discarding of
precise information where necessary (eg. to serialize a precisely typed
record after construction), and its later reconstruction (eg. a weakly
typed deserialized record with a known schema can have it's precise typing
reestabilished).

The library is targetted at Scala 2.10-SNAPSHOT by default, but currently
should build against Scala 2.9.1.final with the `-Ydependent-method-types`
switch enabled. I make no promises that it'll continue to build with 2.9.x,
or even vanilla Scala 2.10-SNAPSHOT: in 2012 I plan to investigate, amongst
other things, what can be done with [singleton types for literal values]
(http://goo.gl/U18kK) ... at a minimum they would make the clunky encoding
of type level natural numbers (in `shapeless/nat.scala`) redundant, and
might enable a whole lot more.

Building
--------

shapeless is built using sbt. On Linux and Mac OS run the `sbt` script and
perform the `compile` task.

Eclipse project metadata can be generated by performing the `eclipse` task.
If you're working with Eclipse I recommend using the [nightly builds
relative to Scala master](http://goo.gl/iRgyc).

Testing
-------

shapeless comes with a collection of JUnit tests which exercise most of its
features. To run, perform the sbt `test` task.
