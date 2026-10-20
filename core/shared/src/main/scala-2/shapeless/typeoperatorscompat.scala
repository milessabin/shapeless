/*
 * Copyright (c) 2011-16 Miles Sabin
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
import scala.util.{ Try, Success, Failure }

/**
 * Type class witnessing the least upper bound of a pair of types and providing conversions from each to their common
 * supertype.
 *
 * @author Miles Sabin
 */
trait Lub[-A, -B, Out] extends Serializable {
  def left(a : A): Out
  def right(b : B): Out
}

object Lub extends LubScalaCompat {
  def instance[A, B, Out](f: A => Out, g: B => Out): Lub[A, B, Out] = new Lub[A, B, Out] {
    def left(a : A): Out = f(a)
    def right(b : B): Out = g(b)
  }
}

trait LubScalaCompat {
  implicit def lub[T]: Lub[T, T, T] = new Lub[T, T, T] {
    def left(a : T): T = a
    def right(b : T): T = b
  }
}

trait TheScalaCompat {
  def apply[T](implicit t: T): T = macro TheMacros.applyImpl

  def selectDynamic(tpeSelector: String): Any = macro TheMacros.implicitlyImpl
}

class TheMacros(val c: whitebox.Context) {
  import c.universe.{ Try => _, _ }
  import internal._, decorators._

  def applyImpl(t: Tree): Tree = t

  def implicitlyImpl(tpeSelector: Tree): Tree = {

    val q"${tpeString: String}" = (tpeSelector: @unchecked)
    val dummyNme = c.freshName()

    val tpe =
      (for {
        parsed <- Try(c.parse(s"{ type $dummyNme = "+tpeString+" }")).toOption
        checked = c.typecheck(parsed, silent = true)
        if checked.nonEmpty
      } yield {
        val q"{ type $dummyNme = $tpt }" = (checked: @unchecked)
        tpt.tpe
      }).getOrElse(c.abort(c.enclosingPosition, s"Malformed type $tpeString"))

    // Bail for primitives because the resulting trees with type set to Unit
    // will crash the compiler
    if(tpe.typeSymbol.asClass.isPrimitive)
      c.abort(c.enclosingPosition, s"Primitive type $tpe may not be used in this context")


    Try(c.typecheck(q"_root_.shapeless.the.apply[$tpe]")) match {
      case Success(x) =>
        // We can't yield a useful value here, so return Unit instead which is at least guaranteed
        // to result in a runtime exception if the value is used in term position.
        Literal(Constant(())).setType(x.tpe)
      case Failure(e) => c.abort(c.enclosingPosition, e.getMessage)
    }
  }
}

object TypeOf extends Dynamic {

  def selectDynamic(code: String): Any = macro Macros.selectDynamic

  private[TypeOf] final class Macros(val c: whitebox.Context) {
    import c.universe.{Try => _, _}
    import internal._, decorators._

    def selectDynamic(code: Tree): Tree = {

      val q"${codeString: String}" = (code: @unchecked)
      val tpe = c.parse(codeString) match {
        case Typed(expr, tpt) =>
          val baseType = c.typecheck(tpt, mode = c.TYPEmode)
          c.typecheck(expr, pt = baseType.tpe).tpe
        case expr =>
          c.typecheck(expr).tpe
      }

      // Bail for primitives because the resulting trees with type set to Unit
      // will crash the compiler
      val symbol = tpe.typeSymbol
      if (symbol.isClass && symbol.asClass.isPrimitive)
        c.abort(c.enclosingPosition, s"Primitive type $tpe may not be used in this context")

      // We can't yield a useful value here, so return Unit instead which is at least guaranteed
      // to result in a runtime exception if the value is used in term position.
      Literal(Constant(())).setType(tpe)
    }
  }

}
