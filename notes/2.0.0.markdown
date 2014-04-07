This is the final release of [shapeless-2.0.0][shapeless]. A detailed
[feature overview][features] and a [guide to migration from shapeless1.2.4][migration]
are available on the [shapeless wiki][wiki].

A big "Thank You!" to everyone who has contributed to this release, either
directly in code or by using and discussing it, by inviting me to talk about
it, and especially by ignoring my advice that shapeless was experimental and
should under no circumstances be used in production. In particular I would like
to thank Mathias Doenitz ([@sirthias][sirthias]) and Eric Torreborre
([@etorreborre][etorreborre]) for taking the plunge and using shapeless in
Spray routing and Specs 2 respectively. I am also very grateful to Eugene
Burmako ([@xeno_by][xeno_by]) for his work on Scala macros, implicit macros in
particular, which have enabled much of what's new.

[shapeless]: https://github.com/milessabin/shapeless
[features]: https://github.com/milessabin/shapeless/wiki/Feature-overview:-shapeless-2.0.0
[migration]: https://github.com/milessabin/shapeless/wiki/Migration-guide:-shapeless-1.2.4-to-2.0.0
[wiki]: https://github.com/milessabin/shapeless/wiki
[sirthias]: https://twitter.com/sirthias
[etorreborre]: https://twitter.com/etorreborre
[xeno_by]: https://twitter.com/xeno_by

Code contributors for shapeless 2.0.0 are, 

* Alois Cochard ([@aloiscochard](https://twitter.com/aloiscochard))
* Cody Allen ([@fourierstrick](https://twitter.com/FouriersTrick))
* Dario Rexin ([@evonox](https://twitter.com/evonox))
* George Leontiev ([@folone](https://twitter.com/folone))
* Johannes Rudolph ([@virtualvoid](https://twitter.com/virtualvoid))
* Joni Freeman ([@jonifreeman](https://twitter.com/jonifreeman))
* Julien Tournay ([@skaalf](https://twitter.com/skaalf))
* Kevin Wright ([@thecoda](https://twitter.com/thecoda))
* Lars Hupel ([@larsr_h](https://twitter.com/larsr_h))
* Michael Donaghy
* Michael Pilquist ([@mpilquist](https://twitter.com/mpilquist))
* Nikolas Evangelopoulos 
* Stacy Curl
* Stephen Compall ([@S11001001](https://twitter.com/S11001001))
* Travis Brown ([@travisbrown](https://twitter.com/travisbrown))

Many thanks to all of you and everyone else who has contributed ideas,
enthusiasim and encouragement.

There are a large number of new features, refactorings and bugfixes in
shapeless 2.0.0. The features which were present in the 2.0.0-M1 preview
release are repeated further below. The following summarizes the most
significant changes between 2.0.0-M1 and this 2.0.0 final release.

### Changes since 2.0.0-M1 

* Added discriminated (ie. labelled) [unions][unions] which have the same relationship
  to `Coproducts` that shapeless records have to `HList`s. As with records,
  the selectors are encoded by intersecting the singleton type of the label
  with the value type and so have no runtime footprint.

[unions]: https://github.com/milessabin/shapeless/wiki/Feature-overview:-shapeless-2.0.0#coproducts-and-discriminated-unions

* Added an encoding of useful [singleton types][symbolsingletons]  for `Symbol` literals.

[symbolsingletons]: https://github.com/milessabin/shapeless/wiki/Feature-overview:-shapeless-2.0.0#singleton-typed-symbols

* Added the [LabelledGeneric][labelledgeneric] type class which maps between (sealed families)
  of case classes and discriminated unions of records (ie. labelled sums of
  labelled products).

[labelledgeneric]: https://github.com/milessabin/shapeless/wiki/Feature-overview:-shapeless-2.0.0#generic-representation-of-sealed-families-of-case-classes

* Lens improvements,
    * Lens selector paths can now be given in terms of `Symbols` corresponding
      to member names rather than `Nat` indices (thanks to Julien Tournay).
    * Added recordLens focussing on named fields of an extensible record
      (thanks to Stephen Compall).
    * Added hlistSelectLens focussing on HList elements selected by type
      (thanks to Stephen Compall).
    * The lens builder is now `lens[T] >> ...` rather than `Lens[T] >> ...`.

* Improvements in the `TypeClass` infrastucture (joint work with Lars Hupel),
    * Clearer separation between `ProductTypeClass` (supporting type classes
      which are well-defined for product types only) and the general
      `TypeClass` (which supports sums of products).
    * Added `LabelledTypeClass` and `LabelledProductTypeClass` which supply
      field and constructor name information to instances.
    * A (suppressible) warning is now generated on attempting to derive a type
      class instance for a single member of a sealed family of case classes.
    <p>

* Added the [Lazy][lazyimplicit] type constructor which supports the lazy materialization
  of implicit values, hence the implicit creation of recursive implicit values
  (aka lazy implicit knot tying).
  
[lazyimplicit]: https://github.com/milessabin/shapeless/wiki/Feature-overview:-shapeless-2.0.0#first-class-lazy-values-tie-implicit-recursive-knots

* Added [TypeCase][typecase] supporting the creation of `Typeable`-based extractors for
  use in pattern matches (thanks to Stacy Curl for the idea).

[typecase]: https://github.com/milessabin/shapeless/wiki/Feature-overview:-shapeless-2.0.0#type-safe-cast

* Various `HList`, `Coproduct` and extensible record improvements,
    * Added constraint to prevent generation of spurious `Transposer`
      instances (thanks to Travis Brown).
    * Added `zipWith` to `HList`, which zips a pair of `HList`s using a
      `Poly2` (thanks to Stacy Curl).
    * Added `zipWithKeys` to `HList` and `Coproduct`, which yields a record/
      discriminated union given a list of singleton-typed key names (thanks
      to Cody Allen)
    * Added `FieldPoly` which allows polymorphic functions to be indexed by
      record/union labels (thanks to Dario Rexin).
    * Added `Typeable` instance for `Coproduct`.
    <p>

* Added type-parametrized factory methods to the companion objects of all
  shapeless type classes, allowing instances to be acquired explictly without
  the use of (and attendant loss of precision with) `implicitly`.

* Various namespacing tweaks,
    * The generic `Zipper` has be repackaged into ops and syntax modules for
      consistency the rest of the library.
    * The `Nat` type `_0` is now aliased in `shapeless.nat` for consistency with
      the other fully applied `Nat` types.
    * The `Witness` implicits for `Nat` have been moved to the `Witness`
      companion object so that they are accessible in contexts which would
      previously have required an import of `Nat._` (thanks to Travis Brown).

* New test utilities,
    * Added `showType` which renders a type or the type of an expression using
      Scala's reflection infrastructure.
    * Added `sameTyped` which compiles iff two expressions are of the same
      type.
    * Factored upteen definitions of `typed` out of the existing tests.

* Build improvements,
    * Complete reworking of the compile-time boilerplate generation
      infrastructure using string interpolation (thanks to Kevin Wright).
    * Replaced dependency on scala-compiler with scala-reflect in
      shapeless-core (thanks to Johannes Rudolph).

### Changes previewed in 2.0.0-M1

New features and enhancements which were previously announced in the shapeless
2.0.0-M1 preview release included,

* Scala 2.10.2 or later is now required. The advantages offered by implicit
  macros are so significant that all current and future shapeless development
  has committed to them.
  <p>Regrettably this means that shapeless-1.2.4 will very likely be the last
  release for Scala 2.9.x. It might be possible to backport some of the
  cosmetic changes to Scala 2.9.x, but I'd like to gauge the level of interest
  in continued support for older Scala releases before embarking on that. It
  might also be feasible to backport the major new features via a compiler
  plugin for Scala 2.9.x.  Anyone interested in contributing to or sponsoring
  such work should [get in touch with me](mailto:miles@milessabin.com).

* `HList`-style operations are now available directly on native Scala tuples.
  This supports a wide variety of tuple manipulation operations which would
  otherwise be extremely tedious to implement manually. Examples can be found
  [here][flatten] and [here][slicingdicing].

  [flatten]: https://github.com/milessabin/shapeless/blob/master/examples/src/main/scala/shapeless/examples/flatten.scala
  [slicingdicing]: https://gist.github.com/milessabin/6081113

* shapeless's `Iso`s have been completely reworked as the new `Generic` type,
  which closely resembles the [generic programming capabilities introduced to
  GHC 7.2][ghcgeneric]. `Generic[T]`, where `T` is a case class or an
  abstract type at the root of a case class hierarchy, maps between values of
  `T` and a generic sum of products representation (`HList`s and `Coproduct`s).
  Values of `Generic` for a given case class are materialized using an implicit
  macro, allowing a wide variety of [structural programming
  problems][deepsearch] to be solved with no or minimal boilerplate.
  <p>In particular the existing [lens][lenses], [Scrap Your
  Boilerplate][sybclass] and [generic zipper][zipper] implementations are now
  available for any case class family ([recursive families included]
  [recursive]) without any additional boilerplate being required.

  [ghcgeneric]: http://www.haskell.org/haskellwiki/GHC.Generics
  [sybclass]: https://github.com/milessabin/shapeless/blob/master/examples/src/main/scala/shapeless/examples/sybclass.scala
  [zipper]: https://github.com/milessabin/shapeless/blob/master/examples/src/main/scala/shapeless/examples/zipper.scala
  [lenses]: https://github.com/milessabin/shapeless/blob/master/examples/src/main/scala/shapeless/examples/lenses.scala
  [deepsearch]: https://github.com/milessabin/shapeless/blob/master/examples/src/main/scala/shapeless/examples/deepsearch.scala
  [recursive]: https://github.com/milessabin/shapeless/blob/master/examples/src/main/scala/shapeless/examples/sybclass.scala#L89

* Based on and extending `Generic`, Lars Hupel has contributed the `TypeClass`
  type class, which provides automatic type class derivation facilities roughly
  equivalent to those available with GHC as described in ["A Generic Deriving
  Mechanism for Haskell"][genericderiving].
  There is a description of the Scala mechanism [here][larshderiving], and
  examples of its use deriving `Show` and `Monoid` instances [here][show] and
  [here][monoid]. The [shapeless-contrib][contrib] project also contains
  automatically derived type class instances for [Scalaz][tcscalaz],
  [Spire][tcspire] and [Scalacheck][tcscalacheck].

  [genericderiving]: http://dreixel.net/research/pdf/gdmh.pdf
  [larshderiving]: http://typelevel.org/blog/2013/06/24/deriving-instances-1.html
  [show]: https://github.com/milessabin/shapeless/blob/master/examples/src/main/scala/shapeless/examples/shows.scala
  [monoid]: https://github.com/milessabin/shapeless/blob/master/examples/src/main/scala/shapeless/examples/monoids.scala
  [contrib]: https://github.com/typelevel/shapeless-contrib
  [tcscalaz]: https://github.com/typelevel/shapeless-contrib/blob/master/scalaz/main/scala/typeclass.scala
  [tcspire]: https://github.com/typelevel/shapeless-contrib/blob/master/spire/main/scala/typeclass.scala
  [tcscalacheck]: https://github.com/typelevel/shapeless-contrib/blob/master/scalacheck/main/scala/package.scala

* Support has been added for working directly with singleton-typed literal
  values in Scala. Although Scala's typechecker has always represented these
  types internally, there has not previously been syntax available to express
  them, other than by [modifying the compiler][literaltype]. This omission has
  been remedied by the use of implicit macros.
  <p>Singleton types bridge the gap between the value level and the type level
  and hence allow the exploration in Scala of techniques which would typically
  only be available in languages with support for full-spectrum dependent
  types. The latest iteration of shapeless records (see next bullet) makes a
  start on that, and the [examples in the tests][singletons] illustrate other
  possibilities.

  [literaltype]: http://existentialtype.net/2008/07/21/literally-dependent-types/
  [singletons]: https://github.com/milessabin/shapeless/blob/master/core/src/test/scala/shapeless/singletons.scala

* As a first application of singleton-typed literals, [shapeless
  records][recordeg] are now represented as `HList`s of values tagged with the
  [singleton types of their keys][recordgist]. This means that there is no
  concrete representation needed at all for the keys. Amongst other things this
  will allow subsequent work on `Generic` to map case classes directly to
  records with their member names encoded in their element types.
  <p>Joni Freeman contributed an [updateWith][updatewith] operation. His
  library [sqltyped][sqltyped] [makes extensive use][sqltypedeg] of shapeless
  records.

  [recordeg]: https://github.com/milessabin/shapeless/blob/master/examples/src/main/scala/shapeless/examples/records.scala
  [recordgist]: https://gist.github.com/milessabin/6185537
  [updatewith]: https://github.com/milessabin/shapeless/blob/master/core/src/test/scala/shapeless/records.scala#L427
  [sqltyped]: https://github.com/jonifreeman/sqltyped 
  [sqltypedeg]: https://github.com/jonifreeman/sqltyped/blob/shapeless_records/core/src/test/scala/examples.scala

* The same implicit macro conversion mechanism which allows literal values to
  be converted to their singleton-typed equivalents is now also used to allow
  Int literals to be used where `Nat` values were previously required. Amongst
  other things, this allows [HLists][hindex] and [tuples][tindex] to be
  [indexed using Ints][tindex].

  [hindex]: https://github.com/milessabin/shapeless/blob/master/core/src/test/scala/shapeless/hlist.scala#L517
  [tindex]: https://github.com/milessabin/shapeless/blob/master/core/src/test/scala/shapeless/tuples.scala#L422 

* The library's namespace has been reorganized to bring it into better
  alignment with the other [typelevel][typelevel] projects, Scalaz and Spire,
  and to reduce general clutter. See the migration guide [for][auxmigrate]
  [more][polymigrate] [details][convmigrate].
    * Extension methods are now brought into scope by importing from an object
      in the `shapeless.syntax` package (eg. `import
      shapeless.syntax.singleton_` to import singleton type related extension
      methods). Extensions to standard Scala types can typically be found under
      `shapeless.syntax.std` (eg.  `import shapeless.syntax.std.tuple._`).
    * Type classes and the implicits which materialize them are now imported
      from the `shapeless.ops` package (eg. `import shapeless.ops.hlist._`).
    * The previous shapeless convention of providing two variants of all type
      classes, one with result types as members and one (with an `Aux` suffix)
      with result types as additional type parameters, has been refined. Now
      only the definition with type members is provided as a first-class trait
      or class, and the additional type parameter variant is defined via a type
      alias in the former's companion object. This results in a significantly
      smaller number of class files and also simplifies implicit resolution in
      some cases.
    * The `Poly` and `Case` name conventions have been aligned with the new
      shapeless type class name convention.
    * The internals of Poly had (ironically) a large amount of arity
      specific/restricted artefacts. These have now been largely eliminated,
      yielding a more consistent structure.
    * The type classes and extension methods for converting between functions
      of multiple arguments and functions with a single `HList` argument have
      been renamed more transparently.
    * All implict definitions now have explicit result types, which should
      result in improved compile times.

  [typelevel]: http://typelevel.org/
  [auxmigrate]: https://github.com/milessabin/shapeless/wiki/Migration-guide:-shapeless-1.2.4-to-2.0.0#3-type-class-aux-suffix-convention-change
  [polymigrate]: https://github.com/milessabin/shapeless/wiki/Migration-guide:-shapeless-1.2.4-to-2.0.0#8-poly-and-case-naming-convention-change
  [convmigrate]: https://github.com/milessabin/shapeless/wiki/Migration-guide:-shapeless-1.2.4-to-2.0.0#6-conversions-between-functions-and-hlist-functions

* Various improvements to `HList`,
    * Added `zipConst` to zip an `HList` with a constant (thanks to Cody
      Allen).
    * The base case for `removeAll` is now `HNil` allowing removals to be more
      uniform where the list of removed types is a type variable which might be
      `HNil` (thanks to Michael Donaghy).
    * Added the `SubtypeUnifier` type class for normalizing subtypes of a given
      type in an HList to that type (thanks to Travis Brown)
    * Some unexpected `Filter` type class instances are no longer created (thanks
      Travis Brown).
    * The `HNil` trait is now sealed, eliminating non-exhaustiveness warnings
      in pattern matching.
    * `zipped` and `unzipped` have been renamed to `zip` and `unzip` for
      consistency with the corresponding new operations provided for tuples,
      which are so-named to avoid clashing with the preexisting `zipped` on
      `Tuple2` and `Tuple3`.
    * Added minimally witnessed instances for `toList[Any]` and `toArray[Any]`.
    * Added `HKernel`, an attempt to reduce the number of implicit witnesses
      required to perform operations on `HList`s whose elements share a common
      outer type constructor.
    <p>

* Libraries like shapeless which make extensive use of type-level computation
  and implicit resolution often need to provide guarantees that certain
  expressions _don't_ typecheck. Testing these guarantees is now supported via
  the [illTyped macro][illtyped], and all tests have been [updated to use
  it][illtypedtest] where appropriate.

  [illtyped]: https://github.com/milessabin/shapeless/blob/master/core/src/main/scala/shapeless/test/typechecking.scala
  [illtypedtest]: https://github.com/milessabin/shapeless/commit/0a9abd5

* The [Sized type][sized] is now a value class, eliminating much of its runtime
  overhead.

  [sized]: https://github.com/milessabin/shapeless/blob/master/examples/src/main/scala/shapeless/examples/sized.scala

* Polymorphic methods can now be [automatically converted][poly] to polymorphic
  function values.

  [poly]: https://github.com/milessabin/shapeless/blob/master/core/src/test/scala/shapeless/poly.scala#L424

* `Nat` additions including `Min` and `Pow` type classes and type level
  [factorial][factorial] and [gcd][gcd] examples (thanks to George Leontiev)

  [factorial]: https://github.com/milessabin/shapeless/blob/master/examples/src/main/scala/shapeless/examples/factorial.scala
  [gcd]: https://github.com/milessabin/shapeless/blob/master/examples/src/main/scala/shapeless/examples/gcd.scala

* New and improved examples: the type class instance packing and unpacking
  [example][pack] now provides completely automatic unpacking as well as
  packing; and an [example][depind] illustrating the use of singleton types and
  path dependent types to encode things normally only expressible in full
  spectrum dependently typed languages has been added.

  [pack]: https://github.com/milessabin/shapeless/blob/master/examples/src/main/scala/shapeless/examples/pack.scala#L79
  [depind]: https://github.com/milessabin/shapeless/blob/master/examples/src/main/scala/shapeless/examples/boolinduction.scala

There have also been many improvements to the build,

* Support for Scala 2.11.x has been moved to [its own branch][shapeless211].
* Snapshot builds of master and the scala-2.11.x branch are now built and
  published automatically using [Travis CI][travis]
* shapeless is now built with SBT 0.13.2.
* The build now uses the [sbt-buildinfo][sbtbi] and [sbt-git][sbtgit] plugins
  to provide useful runtime build information (see `shapeless.BuildInfo`), and
  the [sbt-release][sbtrel] plugin is used to prepare releases.
* The [sbt-osgi][sbtosgi] plugin is now used to add OSGi metadata to the core
  library jar (thanks to Michael Pilquist).
* Whilst the examples are not strictly speaking tests, they are canaries for
  potential issues, and a `run-all` SBT task has been added to support
  verifying that they execute without runtime errors.
* A `.jvmopts` file has been added to the root of the project to provide a
  sensible configuration for the sbt-extras SBT launcher script.
* Added 'import shapeless. _ ' as a console initial command.

  [shapeless211]: https://github.com/milessabin/shapeless/tree/scala-2.11.x
  [travis]: https://travis-ci.org/milessabin/shapeless
  [sbtbi]: https://github.com/sbt/sbt-buildinfo
  [sbtgit]: https://github.com/sbt/sbt-git
  [sbtrel]: https://github.com/sbt/sbt-release
  [sbtosgi]: https://github.com/sbt/sbt-osgi

