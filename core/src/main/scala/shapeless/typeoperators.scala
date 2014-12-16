/*
 * Copyright (c) 2011-14 Miles Sabin
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

import scala.language.dynamics
import scala.language.experimental.macros

import scala.reflect.macros.whitebox
import scala.util.Try

object tag {
  def apply[U] = new Tagger[U]

  trait Tagged[U]
  type @@[+T, U] = T with Tagged[U]

  class Tagger[U] {
    def apply[T](t : T) : T @@ U = t.asInstanceOf[T @@ U]
  }
}

object newtype {
  /**
   * Creates a value of the newtype given a value of its representation type.
   */
  def apply[Repr, Ops](r : Repr) : Newtype[Repr, Ops] = r.asInstanceOf[Any with Newtype[Repr, Ops]]

  /**
   * New type with `Repr` as representation type and operations provided by `Ops`.
   *
   * Values of the newtype will not add any additional boxing beyond what's required for
   * values of the representation type to conform to Any. In practice this means that value
   * types will receive their standard Scala AnyVal boxing and reference types will be unboxed.
   */
  type Newtype[Repr, Ops] = { type Tag = NewtypeTag[Repr, Ops] }
  trait NewtypeTag[Repr, Ops]

  /**
   * Implicit conversion of newtype to `Ops` type for the selection of `Ops` newtype operations.
   *
   * The implicit conversion `Repr => Ops` would typically be provided by publishing the companion
   * object of the `Ops` type as an implicit value.
   */
  implicit def newtypeOps[Repr, Ops](t : Newtype[Repr, Ops])(implicit mkOps : Repr => Ops) : Ops = t.asInstanceOf[Repr]
}

/**
 * An enhanced alternative to `Predef.implicitly`.
 *
 * Used as a term `the[T]` yields the unique implicit value of type `T` in the current
 * implicit scope, if any. It is a compile time error if there is no such value. Its
 * primary advantage over `Predef.implicitly` is that it will preserve any refinement that
 * the implicit definition has, resulting in more precisely typed, and hence often more
 * useful, values,
 *
 * {{{
 * scala> trait Foo { type T ; val t: T }
 * defined trait Foo
 *
 * scala> implicit val intFoo: Foo { type T = Int } = new Foo { type T = Int ; val t = 23 }
 * intFoo: Foo{type T = Int} = \$anon\$1@6067b682
 *
 * scala> implicitly[Foo].t  // implicitly loses precision
 * res0: Foo#T = 23
 *
 * scala> implicitly[Foo].t+13
 * <console>:13: error: type mismatch;
 *  found   : Int(13)
 *  required: String
 *               implicitly[Foo].t+13
 *                                 ^
 *
 * scala> the[Foo].t         // the retains it
 * res1: Int = 23
 *
 * scala> the[Foo].t+13
 * res2: Int = 36
 * }}}
 *
 * Unlike `implicitly`, `the` can also be used in type position, thanks to a trick
 * due to Denys Shabalin (@den_sh) and Eugene Burmako (@xeno_by). Here we use a
 * combination of `selectDynamic` and backticks to embed a type in a path which
 * appears to the compiler as stable,
 *
 * {{{
 * scala> val i: implicitly[Foo].T = 23  // syntax error
 * <console>:1: error: ';' expected but '.' found.
 *        val i: implicitly[Foo].T = 23
 *                              ^
 * scala> val i: the.`Foo`.T = 23        // OK
 * i: Int = 23
 * }}}
 */
object the extends Dynamic {
  def apply[T](implicit t: T): T = macro TheMacros.applyImpl

  def selectDynamic(tpeSelector: String): Any = macro TheMacros.implicitlyImpl
}

object TheMacros {
  def applyImpl(c: whitebox.Context)(t: c.Tree): c.Tree = t

  def implicitlyImpl(c: whitebox.Context)(tpeSelector: c.Tree): c.Tree = {
    import c.universe.{ Try => _, _ }
    import internal._, decorators._

    val q"${tpeString: String}" = tpeSelector
    val dummyNme = c.freshName

    val tpe =
      (for {
        parsed <- Try(c.parse(s"{ type $dummyNme = "+tpeString+" }")).toOption
        checked = c.typecheck(parsed, silent = true)
        if checked.nonEmpty
      } yield {
        val q"{ type $dummyNme = $tpt }" = checked
        tpt.tpe
      }).getOrElse(c.abort(c.enclosingPosition, s"Malformed type $tpeString"))

    // Bail for primitives because the resulting trees with type set to Unit
    // will crash the compiler
    if(tpe.typeSymbol.asClass.isPrimitive)
      c.abort(c.enclosingPosition, s"Primitive type $tpe may not be used in this context")

    val inferred = c.inferImplicitValue(tpe, silent = true)
    if(inferred.isEmpty)
      c.abort(c.enclosingPosition, s"Could not infer implicit value for $tpe")

    // We can't yield a useful value here, so return Unit instead which is at least guaranteed
    // to result in a runtime exception if the value is used in term position.
    c.universe.Literal(Constant(())).setType(inferred.tpe)
  }
}

/**
 * Type class witnessing the least upper bound of a pair of types and providing conversions from each to their common
 * supertype.
 *
 * @author Miles Sabin
 */
trait Lub[-A, -B, Out] {
  def left(a : A): Out
  def right(b : B): Out
}

object Lub {
  implicit def lub[T] = new Lub[T, T, T] {
    def left(a : T): T = a
    def right(b : T): T = b
  }
}

