/*
 * Copyright (c) 2016 Miles Sabin
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

import shapeless.ops.adjoin._

import scala.language.experimental.macros

import scala.reflect.macros.whitebox

trait NestedInterface[T] extends Serializable {
  type Repr
  def to(t: T): Repr
  def from(r: Repr): T
}
object NestedInterface {
  type Aux[T, Repr0] = NestedInterface[T] { type Repr = Repr0 }
  def apply[T](implicit iface: NestedInterface[T]): Aux[T, iface.Repr] = iface
  implicit def materialize[T, Repr]: NestedInterface.Aux[T, Repr] =
    macro InterfaceMacros.materializeUnlabelledNestedInterface[T, Repr]
}

trait FlatInterface[T] extends Serializable {
  type Repr
  def to(t: T): Repr
  def from(r: Repr): T
}
object FlatInterface {
  type Aux[T, Repr0] = FlatInterface[T] { type Repr = Repr0 }
  def apply[T](implicit iface: FlatInterface[T]): Aux[T, iface.Repr] = iface
  implicit def materialize[T, Repr]: FlatInterface.Aux[T, Repr] =
    macro InterfaceMacros.materializeUnlabelledFlatInterface[T, Repr]
}

@macrocompat.bundle
class InterfaceMacros(val c: whitebox.Context) extends SingletonTypeUtils {

  import c.universe._
  import compat._

  case class InterfaceInfo(members: List[MemberInfo])
  case class MemberInfo(
    name: TermName,
    paramLists: List[List[ParamInfo]],
    returnType: Tree
  )
  case class ParamInfo(
    name: TermName,
    paramType: Tree
  )

  sealed trait ExtractionError
  case class NotTrait(tpe: Type) extends ExtractionError
  case class ConcreteMember(symbol: Symbol) extends ExtractionError
  case class GenericMember(symbol: Symbol) extends ExtractionError
  case class ByNameParameter(param: Symbol) extends ExtractionError

  def traverse[T](
    items: List[Either[ExtractionError, T]]
  ): Either[ExtractionError, List[T]] = {
    items.foldRight[Either[ExtractionError, List[T]]](Right(Nil)) {
      case (Right(added), Right(current)) => Right(added :: current)
      case (Right(ignored), Left(existingErr)) => Left(existingErr)
      case (Left(newErr), dropped) => Left(newErr)
    }
  }

  val defaultBaseTypeSymbols = {
    Set[Symbol](
      symbolOf[Any],
      symbolOf[AnyRef],
      symbolOf[AnyVal],
      symbolOf[Object]
    )
  }

  implicit class BiasEither[T](e: Either[ExtractionError, T]) {
    def map[U](
      f: T => U
    ): Either[ExtractionError, U] = {
      e.right.map(f)
    }
    def flatMap[U](
      f: T => Either[ExtractionError, U]
    ): Either[ExtractionError, U] = {
      e.right.flatMap(f)
    }
  }

  def extractInterfaceInfo(
    tpe: Type
  ): Either[ExtractionError, InterfaceInfo] = {
    for {
      memberSymbols <- {
        if (tpe.typeSymbol.isClass && tpe.typeSymbol.asClass.isTrait) Right {
          tpe
            .members
            .flatMap { member =>
              if (member.isTerm) {
                member.asTerm.alternatives.toList
              } else List(member)
            }.filterNot(member => defaultBaseTypeSymbols contains member.owner)
            .toList
        } else Left(NotTrait(tpe))
      }
      members <- traverse {
        memberSymbols.map(extractMemberInfo(tpe))
      }
    } yield InterfaceInfo(members = members)
  }

  def extractMemberInfo(
    containingType: Type
  )(
    memberSymbol: Symbol
  ): Either[ExtractionError, MemberInfo] = {
    for {
      signature <- {
          val signature = memberSymbol.typeSignatureIn(containingType)
          if (!memberSymbol.isAbstract) Left(ConcreteMember(memberSymbol))
          else if (signature.typeParams.nonEmpty) Left(GenericMember(memberSymbol))
          else Right(signature)
      }
      name <- Right(memberSymbol.name.toTermName)
      returnType <- Right(TypeTree(signature.finalResultType))
      paramLists <- traverse {
        signature.paramLists.map { paramList =>
          traverse {
            paramList.map(extractParamInfo(signature))
          }
        }
      }
    } yield MemberInfo(name = name, returnType = returnType, paramLists = paramLists)
  }

  def extractParamInfo(
    methodSignature: Type
  )(
    paramSymbol: Symbol
  ): Either[ExtractionError, ParamInfo] = {
    for {
      paramType <- {
        if (paramSymbol.asTerm.isByNameParam) Left(ByNameParameter(paramSymbol))
        else Right(TypeTree(paramSymbol.typeSignatureIn(methodSignature)))
      }
      name <- Right(paramSymbol.name.toTermName)
    } yield ParamInfo(name = name, paramType = paramType)
  }

  def tpeHCons(a: Tree, b: Tree): Tree = tq"_root_.shapeless.::[$a, $b]"
  val tpeHNil: Tree = tq"_root_.shapeless.HNil"

  def valueHCons(a: Tree, b: Tree): Tree = q"_root_.shapeless.::($a, $b)"
  val valueHNil: Tree = q"_root_.shapeless.HNil"

  def selectByIndex(l: Tree, n: Int): Tree = {
    if (n == 0) q"$l.head"
    else selectByIndex(q"$l.tail", n - 1)
  }

  def materializeInterface[T: WeakTypeTag, Repr: WeakTypeTag](
    typeclassName: String,
    paramRepr: ParamInfo => Tree,
    listRepr: (List[List[Tree]], List[Tree] => Tree) => Tree,
    functionRepr: TermName => Tree => Tree,
    concreteArg: (Tree, List[List[ParamInfo]]) => List[List[Tree]]
  ): Tree = {

    extractInterfaceInfo(weakTypeOf[T]) match {
      case Left(NotTrait(nonTrait)) =>
        if (c.compilerSettings contains "-Xlog-implicits") {
          c.error(
            nonTrait.typeSymbol.pos,
            "Note: unsupported non-trait defined here"
          )
        } else ()
        c.abort(
          c.enclosingPosition,
          "Unsupported Type: Interface-type generic represenation converters can only be generated for traits"
        )
      case Left(ByNameParameter(byNameParam)) =>
        if (c.compilerSettings contains "-Xlog-implicits") {
          c.error(
            byNameParam.pos,
            "Note: unsupported by-name parameter defined here"
          )
        } else ()
        c.abort(
          c.enclosingPosition,
          "Unsupported Type: Interface-type generic represenation converters can only be generated for interfaces when none of their member methods have by-name parameters"
        )
      case Left(GenericMember(genericMember)) =>
        if (c.compilerSettings contains "-Xlog-implicits") {
          c.error(
            genericMember.pos,
            "Note: unsupported generic member defined here"
          )
        } else ()
        c.abort(
          c.enclosingPosition,
          "Unsupported Type: Interface-type generic represenation converters can only be generated for traits with no generic members"
        )
      case Left(ConcreteMember(concreteMember)) =>
        if (c.compilerSettings contains "-Xlog-implicits") {
          c.error(
            concreteMember.pos,
            "Note: unsupported concrete member defined here"
          )
        } else ()
        c.abort(
          c.enclosingPosition,
          "Unsupported Type: Interface-type generic represenation converters can only be generated for traits with no concrete members"
        )
      case Right(interfaceInfo) =>
        val reprTypeTree = {
          interfaceInfo.members.map {
            case MemberInfo(name, paramLists, returnType) =>
              val unifiedParamLists = listRepr(paramLists.map(_.map(paramRepr)), _.foldRight(tpeHNil)(tpeHCons))
              functionRepr(name)(tq"$unifiedParamLists => $returnType")
          }.foldRight(tpeHNil)(tpeHCons)
        }
        val toGeneric = {
          val target = TermName(c.freshName("target"))
            val body = interfaceInfo.members.map {
              case MemberInfo(name, paramLists, returnType) =>
                val param = Ident(TermName(c.freshName("param")))
                val arguments = concreteArg(param, paramLists)
                val paramType = listRepr(paramLists.map(_.map(paramRepr)), _.foldRight(tpeHNil)(tpeHCons))
                val application = q"${target}.${name}(...${arguments})"
                val functionType = functionRepr(name)(tq"$paramType => $returnType")
                q"{ ($param: $paramType) => $application }.asInstanceOf[$functionType]"
            }.foldRight(valueHNil)(valueHCons)
            q"""
              val $target = t
              $body
            """
        }
        val toConcrete = {
          val target = TermName(c.freshName("target"))
          val members = interfaceInfo.members.zipWithIndex.map {
            case (MemberInfo(name, paramLists, returnType), idx) =>
              val body = {
                val arg = {
                  val unifiedArgLists = {
                    paramLists
                      .map { paramList =>
                        paramList.map { param =>
                          val paramReprTpe = paramRepr(param)
                          q"${param.name}.asInstanceOf[${paramReprTpe}]"
                        }
                      }
                  }
                  listRepr(unifiedArgLists, _.foldRight(valueHNil)(valueHCons))
                }
                q"${selectByIndex(q"$target", idx)}.apply($arg)"
              }
              val parameters = paramLists.map { paramList =>
                paramList.map { param =>
                  q"${param.name}: ${param.paramType}"
                }
              }
              q"""def $name(...$parameters) = $body"""
          }
          if (members.nonEmpty) {
            q"""
              val $target = r
              new ${weakTypeOf[T]} {
                ..$members
              }
            """
          } else {
            q"""
              val $target = r
              new ${weakTypeOf[T]} {}
            """
          }
        }
        val tpeNme = TypeName(typeclassName)
        val clsName = TypeName(c.freshName("anon$"))
        q"""
          class $clsName extends _root_.shapeless.$tpeNme[${weakTypeOf[T]}] {
            type Repr = ${reprTypeTree}
            def to(t: ${weakTypeOf[T]}): ${reprTypeTree} = $toGeneric
            def from(r: ${reprTypeTree}): ${weakTypeOf[T]} = $toConcrete
          }
          new $clsName(): _root_.shapeless.${tpeNme.toTermName}.Aux[${weakTypeOf[T]}, $reprTypeTree]
        """
    }

  }

  def unlabelledParam(paramInfo: ParamInfo): Tree = paramInfo.paramType
  def labelledParam(paramInfo: ParamInfo): Tree = {
    val paramType = paramInfo.paramType
    val paramNameTree = SingletonSymbolType(paramInfo.name.decodedName.toString)
    tq"_root_.shapeless.labelled.FieldType[$paramNameTree, $paramType]"
  }

  def unlabelledFunction(name: TermName): Tree => Tree = identity
  def labelledFunction(functionName: TermName): Tree => Tree = { functionTree =>
    val functionNameTree = SingletonSymbolType(functionName.decodedName.toString)
    tq"_root_.shapeless.labelled.FieldType[$functionNameTree, $functionTree]"
  }

  def nestedParamListRepr(paramReprs: List[List[Tree]], reduce: List[Tree] => Tree): Tree = {
    reduce(paramReprs.map(reduce))
  }

  def flatParamListRepr(paramReprs: List[List[Tree]], reduce: List[Tree] => Tree): Tree = {
    reduce(paramReprs.flatten)
  }

  def nestedConcreteArgRepr(target: Tree, paramLists: List[List[ParamInfo]]): List[List[Tree]] = {
    paramLists.zipWithIndex.map { case (paramList, listIndex) =>
      paramList.zipWithIndex.map { case (name, paramIndex) =>
        selectByIndex(selectByIndex(q"$target", listIndex), paramIndex)
      }
    }
  }

  def flatConcreteArgRepr(target: Tree, paramLists: List[List[ParamInfo]]): List[List[Tree]] = {
    val listSizes = paramLists.scanLeft(0)((offset, paramList) => paramList.size)
    (paramLists zip listSizes).map { case (paramList, offset) =>
      paramList.zipWithIndex.map { case (name, paramIndex) =>
        selectByIndex(q"$target", paramIndex+offset)
      }
    }
  }

  def materializeUnlabelledNestedInterface[T: WeakTypeTag, Repr: WeakTypeTag]: Tree = {
    materializeInterface[T, Repr](
      typeclassName = "NestedInterface",
      paramRepr = unlabelledParam,
      listRepr = nestedParamListRepr,
      functionRepr = unlabelledFunction,
      concreteArg = nestedConcreteArgRepr
    )
  }

  def materializeUnlabelledFlatInterface[T: WeakTypeTag, Repr: WeakTypeTag]: Tree = {
    materializeInterface[T, Repr](
      typeclassName = "FlatInterface",
      paramRepr = unlabelledParam,
      listRepr = flatParamListRepr,
      functionRepr = unlabelledFunction,
      concreteArg = flatConcreteArgRepr
    )
  }

  def materializeLabelledNestedInterface[T: WeakTypeTag, Repr: WeakTypeTag]: Tree = {
    materializeInterface[T, Repr](
      typeclassName = "LabelledNestedInterface",
      paramRepr = labelledParam,
      listRepr = nestedParamListRepr,
      functionRepr = labelledFunction,
      concreteArg = nestedConcreteArgRepr
    )
  }

  def materializeLabelledFlatInterface[T: WeakTypeTag, Repr: WeakTypeTag]: Tree = {
    materializeInterface[T, Repr](
      typeclassName = "LabelledFlatInterface",
      paramRepr = labelledParam,
      listRepr = flatParamListRepr,
      functionRepr = labelledFunction,
      concreteArg = flatConcreteArgRepr
    )
  }

}

trait LabelledNestedInterface[T] extends Serializable {
  type Repr
  def to(t: T): Repr
  def from(r: Repr): T
}
object LabelledNestedInterface {
  type Aux[T, Repr0] = LabelledNestedInterface[T] { type Repr = Repr0 }
  def apply[T](implicit iface: LabelledNestedInterface[T]): Aux[T, iface.Repr] = {
    iface
  }
  implicit def materialize[T, Repr]: LabelledNestedInterface.Aux[T, Repr] =
    macro InterfaceMacros.materializeLabelledNestedInterface[T, Repr]
}

trait LabelledFlatInterface[T] extends Serializable {
  type Repr
  def to(t: T): Repr
  def from(r: Repr): T
}
object LabelledFlatInterface {
  type Aux[T, Repr0] = LabelledFlatInterface[T] { type Repr = Repr0 }
  def apply[T](implicit iface: LabelledFlatInterface[T]): Aux[T, iface.Repr] = {
    iface
  }
  implicit def materialize[T, Repr]: LabelledFlatInterface.Aux[T, Repr] =
    macro InterfaceMacros.materializeLabelledFlatInterface[T, Repr]
}
