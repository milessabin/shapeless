# shapeless on Scala 3

shapeless 2.4 is built for Scala 2.13 and Scala 3, on the JVM, Scala.js and Scala Native, and is tested with Scala
2.13.18, 3.3.8 (LTS), 3.9.0 (LTS) and 3.10.0. The Scala 3 (`_3`) artifacts are built with Scala 3.3, so they can be
used with Scala 3.3 or any later Scala 3 version.

```scala
scalaVersion := "3.3.8"

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.4.0-SNAPSHOT"
)
```

Most code using shapeless compiles unchanged with both Scala 2.13 and Scala 3. The macros behind `Generic`,
`LabelledGeneric`, `Generic1`, `Typeable`, `Annotation(s)`, `Default`, `Witness`, records, unions, `Nat` literals and so
on have Scala 3 implementations which produce the same representation types as on Scala 2. Binary compatibility with
earlier releases is not preserved.

## Writing code for both Scala 2 and Scala 3

* give implicit definitions explicit result types, which Scala 3 requires (eg. `implicit def caseInt:
  Case.Aux[Int, String] = at[Int](_.toString)`).
* don't rely on singleton types being inferred for vals: Scala 3 widens the inferred type of a val, so
  `val x = 1.narrow` has type `Int`, and neither `final`, `<: Singleton` bounds, `& Singleton` nor inline methods
  change that. Singleton types are kept within expressions (eg. `1.narrow :: "a".narrow :: HNil` has type
  `1 :: "a" :: HNil`, and likewise for tuples and records), so only the types of vals need attention,
  * in classes and objects, define a `final val` with a literal right hand side and no `narrow`
    (eg. `final val k = "foo"`, of type `"foo"` with both Scala 2 and Scala 3). Scala 3 keeps the inferred type of a
    final val only where it's a literal type, so `final val k = "foo".narrow` and `final val k = w.value` (for a
    `Witness`) are widened.
  * elsewhere (eg. local vals, which Scala 3 doesn't allow to be final, or singleton types other than literal types),
    ascribe the type (eg. `val k: "foo" = "foo"`).

  The same applies to literal record and union values.
* derive cached instances with `cachedImplicit` (eg. `implicit val m: Monoid[Foo] = cachedImplicit`) rather than by
  summoning them: on Scala 3 an implicit val with an explicit type is a candidate for its own initialization, and
  implicits defined in a block are visible throughout it, so `implicit val m: Monoid[Foo] = Monoid[Foo]` refers to
  itself. As on Scala 2, `cachedImplicit` excludes the val it initializes, and the other vals defined alongside it,
  from the implicit search.
* initialize a val by `cachedImplicit` rather than by summoning where the type of an implicit val defined alongside it
  depends on it (eg. `val gen0 = cachedImplicit[Generic[Foo]]` with
  `implicit val gen: Generic.Aux[Foo, gen0.Repr] = gen0`). Scala 3 computes the types of all the implicits in scope
  before choosing among them, so would otherwise fail with a cyclic reference error. Where these are local vals in a
  block, rather than members of a class or object, `cachedImplicit` fails in the same way, so spell out the type
  instead (eg. `Generic.Aux[Foo, Int :: String :: HNil]`).
* use `productTupleOps(t).op` for shapeless tuple operations which `scala.Tuple` also defines on Scala 3 (eg. `head`,
  `tail`, `take`, `drop`, `++`, `zip`, `map`, `toList`, `size`), as the built-in ones take precedence.
* write `Sized[IndexedSeq](a, b)` rather than `Sized(a, b)`.
* publish the companion object of a newtype's ops with its type (eg. `implicit val mkOps: MyStringOps.type =
  MyStringOps`). On Scala 3 case class companions don't extend `Function1`, so shapeless uses the companion's
  `apply` method.
* give the `ops` val of a-la-carte case class definitions its refined type explicitly, since on Scala 3 an overriding
  val takes the type of the val it overrides.
* on Scala 3, subclass `Lazy` by implementing `computeValue` rather than `value` (which Scala 3 doesn't allow to be
  implemented by a `lazy val`).
* use `Lazy` rather than by-name implicit parameters (`=> T`) for derivations through nested or recursive types. On
  Scala 3, shapeless checks searches for `Lazy` implicits for divergence as Scala 2 does, whereas Scala 3 checks by-name
  implicits by its own rules, which reject some derivations Scala 2 accepts (eg. through nested type constructors, such
  as a `Functor` for `Option[List[_]]` via `Generic1`, or which compute an `Aux` type's output).
* compile sources shared with Scala 2 with `-source:3.9` on Scala 3.10 and later, which otherwise reject `with` as a
  type operator (eg. `Int with String`), while Scala 2.13 only accepts `&` with `-Xsource:3`.
* give lower priority implicits at least as many implicit parameters as the higher priority ones they're alternatives
  to (eg. with `DummyImplicit`s). Scala 3 tries implicits with fewer implicit parameters first, and when it finds one
  it still tries those of higher priority, so a derivation which can be found in several ways may be found in every
  possible way at every level of nesting, taking exponential time.

## Differences from Scala 2

The following are only available on Scala 2, or are more limited on Scala 3,

* type providers yield values whose types are refinements carrying the requested types, so on Scala 3 their type
  members can be selected (eg. `` TypeOf.`...`.T ``), but not their singleton types (eg. `` TypeOf.`...`.type ``).
* `∃` and `∀` (which use existential types) are Scala 2 only.
* `Lub` follows Scala 3's type inference (eg. it computes union types where Scala 2 computes a common supertype) and
  is invariant.
* on Scala 3, `Lazy`, `cachedImplicit` and the type providers (eg. `` HList.`Int, String`.T ``, `` the.`...` ``,
  `` TypeOf.`...` ``) depend on compiler internals, for which shapeless includes support for Scala 3.3 to 3.10 (and
  uses the support for the latest of these with later versions). Where that support is missing,
  * `Lazy` falls back to a by-name implicit, and derivations through nested type constructors may be rejected as
    divergent.
  * `cachedImplicit` doesn't exclude any vals from the implicit search, and reports an error where it would resolve to
    the val it initializes or to an implicit val defined after it. An implicit val whose type depends on a val
    initialized by `cachedImplicit` causes a cyclic reference error.
  * type providers support literal types and type names which are fully qualified or visible from enclosing classes
    and packages, but not imports or local type aliases, and `` TypeOf.`...` `` supports only literals and stable paths.
* `compileTime` measures the time taken by Scala 3's `typeCheckErrors`, so it is only approximate.
* the `shapeless-plugin` compiler plugin and the `@generateGeneric` test macro are Scala 2 only.

## Scala 3 compiler issues

Scala 3.3.0 to 3.3.5, 3.4 and 3.5 report "Cannot call macro method $anonfun defined in the same source file" for a
macro, such as a type provider (eg. `` type R = Record.`"a" -> Int`.T ``), used in a type alias defined directly in a
class or object, rather than in a method. Later versions accept these.

Scala 3.4 to 3.7 underestimate the size of types with repeated parts when checking implicit searches for divergence,
so may wrongly reject a derivation which recurses on types of the same size, such as one which moves elements from one
`HList` or `Coproduct` to another (eg. `ReversePrepend[L, L]` or `coproduct.ExtendRightBy[L, L]`). Scala 3.3 and 3.8 and
later accept these.

Earlier Scala 3.3 versions (eg. 3.3.0) and Scala 3.4 to 3.6 don't provide a `Witness` for `this.type`, as they don't
provide its `ValueOf`.

Scala 3 compilers (3.3.8 up to at least 3.10.0) crash with "unresolved symbols: value $_lazy_implicit_$1" when
pickling a recursive by-name implicit derivation whose result type depends on the recursive instance (see the
Scala 2 version of the `delta` example).

## Building shapeless with Scala 3

shapeless cross-builds for Scala 3.3.8, 3.9.0 and 3.10.0 (eg. `sbt ++3.9.0 validateJVM`). Scala 3.8 and later require
JDK 17 or later.

Sources which differ between Scala versions live in `scala-2` and `scala-3` (and, if needed, `scala-<major>.<minor>`,
eg. `scala-3.3`) directories alongside the shared `scala` directories,

* in the library, where a shared definition needs a version-specific part (typically a macro), it extends a
  `XxxScalaCompat` trait defined in a `<file>compat.scala` file in each of the version-specific directories.
* a source in a more version-specific directory replaces the source with the same relative path in a less specific
  one. The tests and examples use this to keep the sources compiled with Scala 2 unchanged: where a test or example
  must differ on Scala 3, its Scala 3 version is `scala-3/<same path>`, and `diff scala/X.scala scala-3/X.scala` shows
  exactly what differs. A marker file `scala-3/<same path>.omit` omits a Scala 2 only test or example on Scala 3.

Most of the Scala 3 versions of tests and examples are generated from their Scala 2 versions. The scalafix rules in
`scalafix` make the changes Scala 3 requires which can be derived from the types Scala 2 infers (eg. giving implicit
definitions their inferred result types, and making explicit the conversions to shapeless' tuple operations where
`scala.Tuple` has members of the same names), and then a patch, `scala-3/X.scala.patch`, makes those they can't (eg.
expecting the types Scala 3 infers where they differ, or omitting tests of what is only available on Scala 2). A
generated source starts with a line saying so. The Scala 2 versions are unchanged, so,

* after changing the Scala 2 version of a test or example, regenerate its Scala 3 version with
  `sbt ++2.13.18 scala3Generate` (`scala3Generate <path of scala/X.scala> ...` generates the Scala 3 versions of
  further sources). If its patch no longer applies, edit the Scala 3 version as it should be.
* after editing the Scala 3 version of a test or example, record the edits as its patch with
  `sbt ++2.13.18 scala3RecordPatches`, before regenerating it, which would otherwise discard them.

`scala3GenerateCheck` fails if any generated source is out of date. The rules use the SemanticDB of the Scala 2
compilation, and scalafix can't read the class files of recent JDKs (eg. JDK 27), so run these with JDK 17 or 21. Types
which Scala 3 infers and which tests expect, but which Scala 2 code can't express (eg. union types), are defined in
`TestVersionSpecifics` (eg. `Lub2[A, B]` for the least upper bound of unrelated `A` and `B`).
