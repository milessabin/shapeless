shapeless : An exploration of generic/polytypic programming in Scala
====================================================================

shapeless is an exploration of generic (aka polytypic) programming in Scala
derived from the various talks I [(Miles Sabin)](http://goo.gl/oBCzn) have
given over the course of 2011 on implementing [scrap your boilerplate]
(http://goo.gl/KmfVG) and [higher rank polymorphism](http://goo.gl/zGRQ7) in
Scala.

In the new year I'll be [posting](http://www.chuusai.com/blog) a (long
overdue) series of articles on the implementation techniques used: heavily 
type class based, with essential use of dependent types at various
junctures, which together enable the relatively smooth encoding of type level
functions. In the meantime you'll find Olivera, Moors and Odersky [Type
Classes as Object and Implicits](http://goo.gl/ZbcxY) useful background
material.

There is a [mailing list](https://groups.google.com/group/shapeless-dev)
for discussion around generic programming in Scala in general and
shapeless in particular. 

Selected highlights of shapeless
--------------------------------

(All the examples below assume you have previously imported shapeless.\_)

### Polymorphic function values

A new encoding of polymorphic function values which optionally supports
type specific cases, and which is interoperable with Scala's ordinary
monomorphic function values.
  
```scala
    // choose is a function from Sets to Options with no type specific cases
    object choose extends (Set ~> Option) {
      def apply[T](s : Set[T]) = s.headOption
    }

    // choose is convertible to an ordinary monomorphic function value
    val lo = List(Set(1, 3, 5), Set(2, 4, 6)) map choose
    lo == List(Option(1), Option(2))

    // size is a function from values of arbitrary type to a 'size' which is
    // defined via type specific cases
    object size extends Poly1 {
      implicit def default[T] = at[T](t => 1)
      implicit def caseInt = at[Int](x => 1)
      implicit def caseString = at[String](_.length)
      implicit def caseList[T] = at[List[T]](_.length)
      implicit def caseOption[T](implicit st : Pullback1[T, Int]) =
        at[Option[T]](t => 1+(t map size).getOrElse(0))
      implicit def caseTuple[T, U]
        (implicit st : Pullback1[T, Int], su : Pullback1[U, Int]) =
          at[(T, U)](t => size(t._1)+size(t._2))
    }

    size(23) == 1
    size("foo") == 3
    size((23, "foo")) == 4
```

### Scrap your Boilerplate

An implementation of [Scrap your Boilerplate with Class](http://goo.gl/pR1OV)
which provides generic map and fold operations over arbitrarily nested data
structures,
  
```scala
    val nested = List(Option(List(Option(List(Option(23))))))
    
    val succ = everywhere(inc)(nested)
    succ == List(Option(List(Option(List(Option(24))))))
```

### Type safe cast

A `Typeable` type class which provides a type safe cast operation.

```scala
    val a : Any = List(Vector("foo", "bar", "baz"), Vector("wibble"))
    
    val lvs : Option[List[Vector[String]]] = a.cast[List[Vector[String]]]
    lvs.isDefined == true

    val lvi : Option[List[Vector[Int]]] = a.cast[List[Vector[Int]]]
    lvi.isEmpty == true
```

### Heterogenous lists

The mother of all Scala `HList`'s, which amongst other things,

* is covariant.

```scala
    trait Fruit
    case class Apple() extends Fruit
    case class Pear() extends Fruit
    
    type FFFF = Fruit :: Fruit :: Fruit :: Fruit :: HNil
    type APAP = Apple :: Pear :: Apple :: Pear :: HNil
    
    val a : Apple = Apple()
    val p : Pear = Pear()
    
    val apap : APAP = a :: p :: a :: p :: HNil
    val ffff : FFFF = apap  // APAP <: FFFF 
```

* has a map operation, applying a polymorphic function value (possibly
  with type specific cases) across its elements. This means that it
  subsumes both typical `HList`'s and also `KList`'s (`HList`'s whose
  elements share a common outer type constructor).
      
```scala
    type SISS = Set[Int] :: Set[String] :: HNil
    type OIOS = Option[Int] :: Option[String] :: HNil
    
    val sets : SISS = Set(1) :: Set("foo") :: HNil
    val opts : OIOS = sets map choose
    opts == Option(1) :: Option("foo") :: HNil 
```

* has a set of fully polymorphic fold operations which take a polymorphic
  binary function value. The fold is sensitive to the static types of all of
  the elements of the `HList`,
  
```scala
    // Polymorphic binary function value with type-specific cases:
    //  (c : Char, s : String) => s.indexOf(c)
    //  (i : Int, b : Boolean) => if ((i >= 0) == b) "pass" else "fail")
    object combine extends Poly2 {
      implicit def caseCharString = at[Char, String]((c, s) => s.indexOf(c))
      implicit def caseIntBoolean =
        at[Int, Boolean]((i, b) => if ((i >= 0) == b) "pass" else "fail")
    }
    
    val l1 = "foo" :: true :: HNil
    val f1 = l1.foldLeft('o')(combine)
    f1 == "pass"
```

* has a zipper for traversal and persistent update.
    
```scala
    import Zipper._

    val l = 1 :: "foo" :: 3.0 :: HNil

    val l2 = l.toZipper.right.put("wibble", 45).reify
    l2 == 1 :: ("wibble", 45) :: 3.0 :: HNil
  
    val l3 = l.toZipper.right.delete.reify
    l3 == 1 :: 3.0 :: HNil

    val l4 = l.toZipper.last.left.insert("bar").reify
    l4 == 1 :: "foo" :: "bar" :: 3.0 :: HNil
```
    
* has a `unify` operation which converts it to an `HList` of elements
  of the least upper bound of the original types.
      
```scala
    val ffff = apap.unify // type inferred as FFFF
```
      
* supports conversion to an ordinary Scala `List` of elements of the
  least upper bound of the original types.
      
```scala
    val lf = apap.toList  // type inferred as List[Fruit]
    lf == List(a, p, a, p)
```
      
* has a `Typeable` type class instance, allowing, eg. vanilla
 `List[Any]`'s or `HList`'s with elements of type `Any` to be safely
  cast to precisely typed `HList`'s.
      
```scala
    val ffff : FFFF = apap.unify               // discard precise typing 
    val apap2 : Option[APAP] = ffff.cast[APAP] // reestablish precise typing
    apap2.get == apap  
```
      
These last three bullets make this `HList` dramatically more practically
useful than `HList`'s are typically thought to be: normally the full
type information required to work with them is too fragile to cross subtyping
or I/O boundaries. This implementation supports the discarding of precise
information where necessary (eg. to serialize a precisely typed record after
construction), and its later reconstruction (eg. a weakly typed deserialized
record with a known schema can have it's precise typing reestabilished).

### Facilities for abstracting over arity

Conversions between tuples and `HList`'s, and between ordinary Scala
functions of arbitrary arity and functions which take a single
corresponding `HList` argument.

```scala
    // Round trip from tuple to HList and back
    val t1 = (23, "foo", 2.0, true)
    
    val l1 = t1.hlisted
    h1 == 23 :: "foo" :: 2.0 :: true :: HNil
    
    val t2 = l1.tupled
    t1 == t2
```    

One application of these is the `liftO` function which lifts an ordinary
function of arbitrary arity into `Option`.
  
```scala
    // Lift these ordinary Scala function values into Option 
    val sum : (Int, Int) => Int = _ + _
    val prd : (Int, Int, Int) => Int = _ * _ * _
    
    // Nb. liftO abstracts over the arity of its function arguments 
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
```

### Heterogenous maps

A heterogenous map which supports an arbitrary relation between key type
and corresponding value type,
  
```scala
    // Key/value relation to be enforced: Strings map to Ints and vice versa
    class BiMapIS[K, V]
    implicit val intToString = new BiMapIS[Int, String]
    implicit val stringToInt = new BiMapIS[String, Int]
  
    val hm = HMap[BiMapIS](23 -> "foo", "bar" -> 13)
    val hm2 = HMap[BiMapIS](23 -> "foo", 23 -> 13)   // Does not compile
    
    val s1 = hm.get(23)        // Inferred type is Option[String]
    s1 == Some("foo")

    val i1 = hm.get("bar")     // Inferred type is Option[Int]
    i1 == Some(13)
```

### Extensible records

An implementation of extensible records modelled as `HLists` of
associations. Keys are encoded using singleton types and fully determine
the types of their corresponding values,
  
```scala
    import Record._

    object author  extends Field[String]
    object title   extends Field[String]
    object price   extends Field[Double]
    object inPrint extends Field[Boolean]

    val book =
      (author -> "Benjamin Pierce") ::
      (title  -> "Types and Programming Languages") ::
      (price  ->  44.11) ::
      HNil
  
    // Read price field
    val currentPrice = book.get(price)  // Inferred type is Double
    currentPrice == 44.11

    // Update price field, relying on static type of currentPrice
    val updated = book + (price -> (currentPrice+2.0))
  
    // Add a new field
    val extended = updated + (inPrint -> true)
    
    extended ==
      (author -> "Benjamin Pierce") ::
      (title  -> "Types and Programming Languages") ::
      (price  ->  46.11) ::
      (inPrint -> true) ::
      HNil
```

### Representation of case classes as HLists

A type representing an isomorphism between an arbitrary case class an
`HList` composed of the case classes components. This has many
applications including,
  
* almost automatic derivation of type class instances for case classes
  given the instances for their components,

```scala
    // Given an isomorphism between `C` and an `HList` `L`, construct a
    // monoid instance for `C` given the monoid instance for `L`, which is 
    // in turn derived from the monoid instances for its/`C`'s element types.
    implicit def ccMonoid[C, L <: HList](implicit iso : Iso[C, L], ml : Monoid[L]) =
      new Monoid[C] {
        def zero = fromHList(ml.zero)
        def append(a : C, b : C) = fromHList(toHList(a) |+| toHList(b))
      }
  
    // An ordinary case class
    case class Foo(i : Int, s : String, d : Double)
  
    // Publish its `Iso`
    implicit def fooIso = Iso.hlist(Foo.apply _, Foo.unapply _)
  
    // And now it's a monoid ...
    
    val f = Foo(13, "foo", 1.0) |+| Foo(23, "bar", 3.0)
    f == Foo(36, "foobar", 4.0)
```
    
* boilerplate-free lenses for HList and tuple types, and almost
  boilerplate-free lenses for arbitrary case classes, 

```scala
    // A pair of ordinary case classes ...
    case class Address(street : String, city : String, postcode : String)
    case class Person(name : String, age : Int, address : Address)
    
    // One line of boilerplate per case class ...
    implicit val addressIso = Iso.hlist(Address.apply _, Address.unapply _)
    implicit val personIso = Iso.hlist(Person.apply _, Person.unapply _)
    
    // Some lenses over Person/Address ...
    val nameLens     = Lens[Person] >> _0
    val ageLens      = Lens[Person] >> _1
    val addressLens  = Lens[Person] >> _2
    val streetLens   = Lens[Person] >> _2 >> _0
    val cityLens     = Lens[Person] >> _2 >> _1
    val postcodeLens = Lens[Person] >> _2 >> _2
  
    // Starting value
    val person = Person("Joe Grey", 37, Address("Southover Street", "Brighton", "BN2 9UA"))
    
    // Read a field
    val age1 = ageLens.get(person) // Type inferred is Int
    age1 == 37
  
    // Update a field
    val person2 = ageLens.set(person)(38)
    person2.age == 38
    
    // Transform a field
    val person3 = ageLens.modify(person2)(_ + 1)
    person3.age == 39
    
    // Read a nested field
    val street = streetLens.get(person3)
    street == "Southover Street"
    
    // Update a nested field
    val person4 = streetLens.set(person3)("Montpelier Road")
    person4.address.street == "Montpelier Road"
    
    // Cumulative result of above updates
    person4 == Person("Joe Grey", 39, Address("Montpelier Road", "Brighton", "BN2 9UA"))
```

### Sized types

Collection types with statically known sizes. These can prevent runtime
errors that would result from attempting to take the head of an empty list,
and can also verify more complex and useful relationships. 

```scala
    def row(cols : Seq[String]) = cols.mkString("\"", "\", \"", "\"")
    def csv[N <: Nat]
      (hdrs : Sized[Seq[String], N],
       rows : List[Sized[Seq[String], N]]) = row(hdrs) :: rows.map(row(_))

    val hdrs = Sized("Title", "Author")
    val rows = List(
      Sized("Types and Programming Languages", "Benjamin Pierce"),
      Sized("The Implementation of Functional Programming Languages", "Simon Peyton-Jones")
    )
  
    // hdrs and rows statically known to have the name number of columns
    val formatted = csv(hdrs, rows)                        // Compiles
    
    // extendedHdrs has the wrong number of columns for rows
    val extendedHdrs = Sized("Title", "Author", "ISBN")
    val badFormatted = csv(extendedHdrs, rows)             // Does not compile
```

### Newtype

A mostly unboxed approximation to Haskell's newtype, 

```scala
    // MyString is a new type with String as its underlying representation
    // and with its operations provided by MyStringOps
    type MyString = Newtype[String, MyStringOps]
  
    // MyString constructor
    def MyString(s : String) : MyString = newtype(s)
  
    // Expose String#size as MyString#mySize. No other operations of String 
    // are accessible
    case class MyStringOps(s : String) {
      def mySize = s.size
    }
    implicit val mkOps = MyStringOps
  
    val ms = MyString("foo")
  
    val s : String = ms        // Does not compile
    val ms2 : MyString = "foo" // Does not compile
  
    //ms.size                  // Does not compile
    ms.mySize == 3             // Compiles.
    
    // Verify that this is an unboxed representation
    ms.getClass == classOf[String]
```

Using shapeless
---------------

shapeless is published to the Sonatype OSS Repository Hosting Service
(OSSRH) and will be synced to Maven Central. Release builds are published
relative to Scala 2.9.2 and 2.10.0. Snapshot builds are also published
relative to Scala 2.11.0-SNAPSHOT.

To build with Scala 2.10.0 add the following to your SBT (0.12.0 or later)
configuration,

```scala
scalaVersion := "2.10.0"

resolvers ++= Seq(
  "Sonatype OSS Releases" at "http://oss.sonatype.org/content/repositories/releases/",
  "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
)

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "1.2.4"
)
```

If your project is built with Scala 2.9.2, then you will also need to specify the
`-Ydependent-method-types` compiler flag,

```scala
scalaVersion := "2.9.2"

scalacOptions += "-Ydependent-method-types"

resolvers ++= Seq(
  "Sonatype OSS Releases" at "http://oss.sonatype.org/content/repositories/releases/",
  "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
)

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "1.2.4"
)
```

If you want to be able to support building relative to both 2.9.2 and 2.10.0
then you should use the 2.10.0 configuration above and add the following,
 
```scala
scalacOptions <++= scalaVersion map { version =>
  val Some((major, minor)) = CrossVersion.partialVersion(version)
  if (major < 2 || (major == 2 && minor < 10)) 
    Seq("-Ydependent-method-types")
  else Nil
}
```

which will set the `-Ydependent-method-types` compiler flag conditionally on
the actual Scala version in use.

To build with Scala 2.11.0 milestone releases or snapshots then you'll need to
add the following to your SBT configuration,

```scala
scalaVersion := "2.11.0-SNAPSHOT"

resolvers ++= Seq(
  "Sonatype OSS Releases" at "http://oss.sonatype.org/content/repositories/releases/",
  "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
)

libraryDependencies ++= Seq(
  "com.chuusai" % "shapeless" % "1.2.5-SNAPSHOT" cross CrossVersion.full
)
```

Building shapeless
------------------

By default shapeless is built with Scala 2.11.0-SNAPSHOT, but it will build
against any Scala 2.9.x or 2.10.x release. You can build against Scala 2.10.0 by
running `sbt` and entering `++ 2.10.0` at the SBT prompt.

Testing
-------

shapeless comes with a collection of JUnit tests which exercise most of its
features. To run, perform the sbt `test` task.
