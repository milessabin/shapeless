package shapeless

import scala.language.experimental.macros
import scala.reflect.macros.whitebox
import scala.util.{Failure, Success, Try}

trait theScalaCompat {
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

trait TypeOfScalaCompat {
  def selectDynamic(code: String): Any = macro TypeOfMacros.selectDynamic
}

private[shapeless] final class TypeOfMacros(val c: whitebox.Context) {
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
