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

import shapeless.labelled.FieldType

import scala.compiletime._
import scala.quoted._

trait RecordScalaCompat {
  inline def applyDynamic(inline method: String)(inline rec: (String, Any)*): HNil =
    //inline if method != "apply" then ??? //error(s"this method must be called as 'apply' not $method")
    //else inline if rec.nonEmpty then ??? //error("this method must be called with named arguments")
    /*else*/ HNil

  transparent inline def applyDynamicNamed(inline method: String)(inline rec: (String, Any)*): HList =
    ${ RecordScalaCompat.applyDynamicNamedImpl('method)('rec) }
}
object RecordScalaCompat {
  def applyDynamicNamedImpl(method: Expr[String])(rec: Expr[Seq[(String, Any)]])(using Quotes): Expr[HList] = {
    import quotes.reflect.report
    val methodString = method.valueOrError
    if methodString != "apply" then
      report.throwError(s"this method must be called as 'apply' not '$methodString'")

    rec match {
      case Varargs(values) =>
        def transform[Acc <: HList: Type](acc: Expr[Acc], rest: Seq[Expr[(String, Any)]]): Expr[HList] =
          rest match {
            case Nil => acc
            case Seq('{($keyExpr: String, $value: tp)}, tail*) =>
              import quotes.reflect._
              keyExpr.asTerm match {
                case Literal(const) => ConstantType(const).asType
              } match {
                case '[keyTpe] => transform('{$value.asInstanceOf[FieldType[keyTpe, tp]] :: $acc}, tail)
              }

            case _ =>
              report.throwError("Got invalid arguments in varargs")
          }

        transform('{HNil}, values.reverse)

      case _ =>
        report.throwError("this method must be called with vararg arguments")
    }
  }
}
