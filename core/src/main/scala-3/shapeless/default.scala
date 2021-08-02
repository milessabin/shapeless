package shapeless

import scala.quoted.*

trait DefaultScalaCompat {
  transparent inline given materialize[T]: Default[T] =  ${DefaultScalaCompat.defaultImpl[T]}
}

object DefaultScalaCompat {
  def defaultImpl[T: Type](using quotes: Quotes): Expr[Default[T]] = {
    import quotes.reflect.*
    val tpe = TypeRepr.of[T]

    val defaultConstructorMethodNames = "$lessinit$greater$default$"

    tpe.classSymbol match {
      case Some(classSymbol) =>
        val constructorParams = classSymbol.primaryConstructor.paramSymss
        if(constructorParams.sizeIs > 1) {
          report.throwError(s"Found more than one parameter list for ${tpe.show}")
        }

        val constructorParamsSize = constructorParams.head.size

        val moduleClass = classSymbol.companionModule.moduleClass
        val defaultMethods = (1 to constructorParamsSize).map { i =>
          moduleClass.declaredMethod(defaultConstructorMethodNames + i).headOption
        }

        val defaultExprs = defaultMethods.map {
          case Some(method) =>
            val DefDef(_, _, _, optDefault) = method.tree
            val default = optDefault.getOrElse(
              report.throwError("Found empty tree for default. Make sure that the compiler option -Yretain-trees is enabled")
            )

            default.asExpr match {
              case '{$defaultExpr: t} =>
                defaultExpr.asTerm.tpe.widen.asType match {
                  case '[t2] => '{Some($defaultExpr.asInstanceOf[t2])}
                }

                //'{Some($defaultExpr)}
            }

          case None => '{None}
        }

        def result[Acc <: HList: Type](acc: Expr[Acc], rest: Seq[Expr[Any]]): Expr[Default[T]] =
          rest match {
            case Nil => '{
              new Default[T] {
                override type Out = Acc
                override def apply(): Acc = $acc
              }
            }
            case Seq('{$head: tpe}, tail*) =>
              result('{new ::($head, $acc)}, tail)
          }

        result('{HNil: HNil}, defaultExprs.reverse)

      case _ =>
        report.throwError(s"Invalid type ${tpe.show}. Expected a class.")
    }
  }
}
