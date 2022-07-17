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

        if (classSymbol.moduleClass == classSymbol) {
          '{
            new Default[T] {
              override type Out = HNil
              override def apply(): HNil = HNil
            }
          }
        }
        else if (classSymbol.flags.is(Flags.Abstract) || classSymbol.flags.is(Flags.Trait)) {
          report.errorAndAbort(s"${tpe.show} is abstract")
        }
        else {
          val constructorParams = classSymbol.primaryConstructor.paramSymss
          if(constructorParams.sizeIs > 1) {
            report.errorAndAbort(s"Found more than one parameter list for ${tpe.show}")
          }

          val constructorParamsSize = constructorParams.head.size
          if(constructorParamsSize == 0) {
            report.errorAndAbort(s"${tpe.show} has an empty constructor")
          }

          val moduleClass = classSymbol.companionModule.moduleClass
          val defaultMethods = (1 to constructorParamsSize).map { i =>
            moduleClass.declaredMethod(defaultConstructorMethodNames + i).headOption
          }

          val defaultExprs = defaultMethods.map {
            case Some(method) =>
              method.tree match {
                case DefDef(_, _, tpeTree, optDefault) =>
                  val default = optDefault.getOrElse(
                    report.errorAndAbort("Found empty tree for default. Make sure that the compiler option -Yretain-trees is enabled")
                  )

                  tpeTree.tpe.asType match {
                    case '[t2] => '{Some(${default.asExpr}.asInstanceOf[t2])}
                  }
                case _ => '{None}
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
        }

      case _ =>
        report.errorAndAbort(s"Invalid type ${tpe.show}. Expected a class.")
    }
  }
}
