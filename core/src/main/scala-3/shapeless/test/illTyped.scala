package shapeless.test

import java.util.regex.Pattern
import scala.compiletime.*
import scala.compiletime.testing.*
import scala.quoted.*

object illTyped {

  inline def apply(inline code: String): Unit =
    ${impl('{typeCheckErrors(code)}, '{None})}

  inline def apply(inline code: String, inline expected: String): Unit =
    ${impl('{typeCheckErrors(code)}, '{Some(expected)})}

  private given FromExpr[ErrorKind] with {
    override def unapply(x: Expr[ErrorKind])(using Quotes): Option[ErrorKind] = x match {
      case '{ErrorKind.Parser} => Some(ErrorKind.Parser)
      case '{ErrorKind.Typer} => Some(ErrorKind.Typer)
      case _ => None
    }
  }

  private given FromExpr[Error] with {
    override def unapply(x: Expr[Error])(using Quotes): Option[Error] = {
      x match {
        case '{Error($a, $b, $c, $d)} =>
          (a.asExprOf[String], b.asExprOf[String], c.asExprOf[Int], d.asExprOf[ErrorKind]) match {
            case (Expr(message), Expr(lineContent), Expr(column), Expr(errorKind)) =>
              Some(Error(message, lineContent, column, errorKind))
            case _ => None
          }
        case _ => None
      }
    }
  }

  def impl(errorsE: Expr[List[Error]], expectedE: Expr[Option[String]])(using quotes: Quotes): Expr[Unit] = {
    import quotes.reflect.*

    val errors = errorsE.asTerm match {
      case Inlined(_, _, Apply(_, List(errorsVararg))) =>
        errorsVararg.asExprOf[Seq[Error]] match {
          case Varargs(Exprs(es)) => es
          case _ => report.throwError("Unexpected errors structure")
        }
      case _ => report.throwError("Unexpected errors structure")
    }
    val expectedOpt = expectedE.valueOrError

    errors.find(_.kind == ErrorKind.Parser).foreach { e =>
      report.throwError("Parsing failed.\n" + e.message)
    }

    if (errors.isEmpty) {
      report.throwError("Type-checking succeeded unexpectedly.\nExpected some error.")
    }

    expectedOpt.foreach { expected =>
      val expectedPattern: Pattern = Pattern.compile(expected, Pattern.CASE_INSENSITIVE | Pattern.DOTALL)

      if (!errors.exists(e => expectedPattern.matcher(e.message).matches)) {
        report.throwError("Type-checking failed in an unexpected way.\n" + expected + "\nActual error: "+ errors.head.message)
      }
    }

    '{()}
  }
}
