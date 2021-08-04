package shapeless.test

import java.util.regex.Pattern
import scala.compiletime.*
import scala.compiletime.testing.*

object illTyped {
  inline def apply(inline code: String): Unit = {
    val errors = typeCheckErrors(code)

    errors.find(_.kind == ErrorKind.Parser).foreach { e =>
      error(s"Parsing failed.\n${e.message}")
    }

    if (errors.isEmpty) {
      error("Type-checking succeeded unexpectedly.\nExpected some error.")
    }
  }

  inline def apply(inline code: String, inline expected: String): Unit = {
    val errors = typeCheckErrors(code)

    val expectedPattern: Pattern = Pattern.compile(expected, Pattern.CASE_INSENSITIVE | Pattern.DOTALL)

    errors.find(_.kind == ErrorKind.Parser).foreach { e =>
      error(s"Parsing failed.\n${e.message}")
    }

    if (errors.isEmpty) {
      error("Type-checking succeeded unexpectedly.\nExpected some error.")
    }

    if (!errors.exists(e => expectedPattern.matcher(e.message).matches)) {
      error("Type-checking failed in an unexpected way.\n" + expected + "\nActual error: "+ errors.head.message)
    }
  }
}
