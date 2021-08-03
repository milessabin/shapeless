/*
 * Copyright (c) 2018 Miles Sabin
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

import scala.quoted.*

trait ScalaVersionSpecifics {
  def cachedImplicit[T]: T = ???
}

private[shapeless] def listExprToResult[Acc <: HList: Type, Out](acc: Expr[Acc], rest: Seq[Expr[Any]])(
  makeResult: [Acc <: HList] => Type[Acc] ?=> Expr[Acc] => Expr[Out]
)(using Quotes): Expr[Out] =
  rest match {
    case Nil => makeResult(acc)
    case Seq('{$head: tpe}, tail*) =>
      listExprToResult('{new ::($head, $acc)}, tail)(makeResult)
  }

private[shapeless] def listExprToHList(list: Seq[Expr[Any]])(using Quotes): Expr[HList] =
  listExprToResult[HNil, HList]('{HNil: HNil}, list)([Acc <: HList] => (tpe: Type[Acc]) ?=> (expr: Expr[Acc]) => expr)
