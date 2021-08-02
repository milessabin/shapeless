/*
 * Copyright (c) 2013-16 Miles Sabin
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

import scala.quoted._

trait UnionScalaCompat {
  transparent inline def applyDynamicNamed[U <: Coproduct](inline method: String)(inline rec: (String, Any)*): U =
    ${ UnionScalaCompat.applyDynamicNamedImpl[U]('method)('rec) }
}
object UnionScalaCompat {
  def applyDynamicNamedImpl[U <: Coproduct: Type](method: Expr[String])(rec: Expr[Seq[(String, Any)]])(using Quotes): Expr[U] = {
    import quotes.reflect.report
    val methodString = method.valueOrError
    if methodString != "apply" then
      report.throwError(s"this method must be called as 'apply' not '$methodString'")

    rec match {
      case Varargs(Seq('{($keyExpr: String, $value: tp)})) =>
        import quotes.reflect._

        keyExpr.asTerm match {
          case Literal(const) => ConstantType(const).asType
        } match {
          case '[keyTpe] =>
            Expr.summon[ops.coproduct.Inject[U, FieldType[keyTpe, tp]]] match {
              case Some(injectExpr) => '{$injectExpr($value.asInstanceOf[FieldType[keyTpe, tp]])}
              case None =>
                report.throwError("Can not inject into coproduct")
            }
        }

      case Varargs(_) =>
        report.throwError("only one branch of a union may be inhabited")

      case _ =>
        report.throwError("this method must be called with vararg arguments")
    }
  }
}
