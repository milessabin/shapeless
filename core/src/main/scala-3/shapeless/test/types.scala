package shapeless.test

import scala.quoted.*

def typed[T](t : => T): Unit = {}

def sameTyped[T](t1: => T)(t2: => T): Unit = {}

inline def showType[T]: String = ${showTypeImpl[T]}

inline def showType[T](t: => T): String = ${showTypeImpl[T]}

private def showTypeImpl[T: Type](using quotes: Quotes): Expr[String] = {
  import quotes.reflect.*
  Expr(TypeRepr.of[T].dealias.show)
}

inline def desugared[T](inline expr: T): String = ${desugaredImpl('{expr})}

private def desugaredImpl[T](expr: Expr[T])(using Quotes): Expr[String] =
  import quotes.reflect.*
  Expr(expr.show)
