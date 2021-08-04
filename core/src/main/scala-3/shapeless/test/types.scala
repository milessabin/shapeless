package shapeless.test

import scala.quoted.*

def typed[T](t : => T): Unit = {}

def sameTyped[T](t1: => T)(t2: => T): Unit = {}

inline def showType[T]: String = ${showTypeImpl[T]}

inline def showType[T](t: => T): String = ${showTypeImpl[T]}

private def showTypeImpl[T: Type](using Quotes): Expr[String] = Expr(Type.show[T])
