/*
 * Copyright (c) 2026 Miles Sabin
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

package shapeless.migration

import scala.meta._
import scalafix.v1._

/**
 * Prints SemanticDB types as source which denotes them at the definitions of a document, or `None` where it can't.
 *
 * Types are printed as they would be written (eg. `(A, B)`, `A => B`, `H :: T`), and symbols are named as simply as
 * possible: members of the classes and objects `enclosing` the definitions by their names, as are statically accessible
 * symbols where they
 * are members of an enclosing package or `scala`, or are imported, and otherwise by paths from such symbols.
 *
 * Shadowing isn't taken into account, so a symbol named by a name which a local definition shadows is printed wrongly.
 */
class TypePrinter(enclosing: Set[Symbol] = Set.empty)(implicit doc: SemanticDocument) {
  import TypePrinter._

  /** The type `tpe` as written in the definition of `owner`'s member */
  def print(tpe: SemanticType, owner: Symbol): Option[String] =
    render(tpe, owner, Map.empty).map(_.text)

  def render(tpe: SemanticType, owner: Symbol, wildcards: Map[Symbol, String]): Option[Rendered] = tpe match {
    case TypeRef(NoType, sym, Nil) if wildcards.contains(sym) =>
      Some(Rendered(wildcards(sym), Simple))
    case TypeRef(prefix @ SingleType(_, path), sym, args) if sym.isNone || sym.displayName.isEmpty =>
      // A type member of a path whose symbol SemanticDB doesn't record (eg. `data.Out` for `data: DataT.Aux[...]`)
      for {
        member <- refinementMember(path).filter(_.nonEmpty)
        rprefix <- render(prefix, owner, wildcards)
        rargs <- sequence(args.map(render(_, owner, wildcards)))
      } yield applied(sym, s"${rprefix.text.stripSuffix(".type")}.$member", rargs)
    case TypeRef(prefix, sym, args) =>
      for {
        name <- reference(prefix, sym)
        rargs <- sequence(args.map(render(_, owner, wildcards)))
      } yield applied(sym, name, rargs)
    case SingleType(prefix, sym) =>
      reference(prefix, sym).map(name => Rendered(s"$name.type", Simple))
    case ThisType(sym) =>
      Some(Rendered(if (sym == owner) "this.type" else s"${sym.displayName}.this.type", Simple))
    case ConstantType(constant) =>
      literal(constant).map(Rendered(_, Simple))
    case WithType(types) if types.length > 1 && types.exists(isAnyRef) =>
      render(WithType(types.filterNot(isAnyRef)), owner, wildcards)
    case WithType(List(tpe)) =>
      render(tpe, owner, wildcards)
    case WithType(types) =>
      sequence(types.map(render(_, owner, wildcards))).map(rs => Rendered(rs.map(compoundOperand).mkString(" with "), Compound))
    case IntersectionType(types) =>
      sequence(types.map(render(_, owner, wildcards))).map(rs => Rendered(rs.map(compoundOperand).mkString(" with "), Compound))
    case StructuralType(parent, declarations) if declarations.exists(isOverridingTerm) =>
      // Methods and values which override those of the parent (eg. those of an anonymous class implementing its
      // parent's) add nothing, unlike type members which refine those of the parent
      render(StructuralType(parent, declarations.filterNot(isOverridingTerm)), owner, wildcards)
    case StructuralType(parent, Nil) =>
      // The type of an anonymous class, eg. `new Foo[Int] {}`
      render(parent, owner, wildcards)
    case StructuralType(parent, declarations) =>
      for {
        rparent <- render(parent, owner, wildcards)
        rdecls <- sequence(declarations.map(declaration(_, owner, wildcards)))
      } yield Rendered(s"${compoundOperand(rparent)} { ${rdecls.mkString("; ")} }", Compound)
    case ExistentialType(underlying, declarations) =>
      sequence(declarations.map(info => bounds(info, owner, wildcards).map(b => info.symbol -> s"_$b")))
        .flatMap(ws => render(underlying, owner, wildcards ++ ws))
        .filter(r => declarations.forall(info => !r.text.contains(info.displayName)))
    case UniversalType(params, TypeRef(prefix, sym, args)) if args == params.map(p => TypeRef(NoType, p.symbol, Nil)) =>
      // The eta-expansion of a type constructor, eg. [+A] => IndexedSeq[A]
      reference(prefix, sym).map(Rendered(_, Simple))
    case _ =>
      None
  }

  /** The reference to `sym`, a member of `prefix` */
  def reference(prefix: SemanticType, sym: Symbol): Option[String] = prefix match {
    case NoType => Some(static(sym))
    case ThisType(_) => Some(name(sym))
    case SingleType(NoType, path) => Some(s"${static(path)}.${name(sym)}")
    case SingleType(ThisType(_), path) => Some(s"${name(path)}.${name(sym)}")
    case SingleType(outer, path) => reference(outer, path).map(p => s"$p.${name(sym)}")
    case TypeRef(_, _, _) => render(prefix, sym.owner, Map.empty).map(r => s"${simpleOperand(r)}#${name(sym)}")
    case _ => None
  }

  /** The path to the statically accessible symbol `sym` */
  def static(sym: Symbol): String =
    if (sym.isLocal || isTypeParameter(sym) || isParameter(sym) || !sym.owner.isGlobal || isClass(sym.owner) ||
        enclosing(sym.owner) ||
        aliases.contains(sym) || visible(sym) || sym.owner.isRootPackage || sym.owner.isEmptyPackage)
      name(sym)
    else
      s"${static(sym.owner)}.${name(sym)}"

  /** The name of `sym`, or that of the `scala` or `Predef` alias of it */
  def name(sym: Symbol): String = {
    val n = aliases.getOrElse(sym, sym.info.fold(sym.displayName)(_.displayName))
    if (isSymbolic(n) || n.headOption.exists(c => c.isLetter || c == '_')) n else s"`$n`"
  }

  /** Whether `sym` is visible by its name throughout the document */
  def visible(sym: Symbol): Boolean = {
    val owner = sym.owner
    alwaysVisible(owner) || packages(owner) || wildcardImports(owner) || isPackageObjectOf(owner, packages) ||
      isPackageObjectOf(owner, wildcardImports) ||
      namedImports.get(sym.displayName).exists(_.owner == owner)
  }

  /**
   * The single type member of the refinement which is the declared type of `path` (eg. `Out` for a
   * `data: DataT.Aux[F, T, R]`, where `DataT.Aux[F, T, R]` is `DataT[F, T] { type Out = R }`)
   */
  def refinementMember(path: Symbol): Option[String] = {
    def members(tpe: SemanticType): List[String] = tpe match {
      case StructuralType(_, decls) => decls.filter(_.isType).map(_.displayName)
      case TypeRef(_, alias, _) =>
        alias.info.map(_.signature).toList.flatMap {
          case TypeSignature(_, _, hi) => members(hi)
          case _ => Nil
        }
      case _ => Nil
    }
    val declared = path.info.map(_.signature).collect {
      case ValueSignature(tpe) => tpe
      case MethodSignature(_, Nil, tpe) => tpe
    }
    declared.map(members).collect { case List(member) => member }
  }

  /** Whether `owner` is the package object of one of `pkgs`, whose members are visible where those of the package are */
  def isPackageObjectOf(owner: Symbol, pkgs: Set[Symbol]): Boolean =
    owner.displayName == "package" && pkgs(owner.owner)

  lazy val packages: Set[Symbol] =
    doc.tree.collect { case pkg: Pkg => pkg.ref.symbol }.toSet

  lazy val importers: List[Importer] =
    doc.tree.collect { case i: Import => i.importers }.flatten

  lazy val wildcardImports: Set[Symbol] =
    importers.collect { case i if i.importees.exists(_.is[Importee.Wildcard]) => i.ref.symbol }.toSet

  lazy val namedImports: Map[String, Symbol] =
    importers.flatMap { i =>
      i.importees.collect {
        case Importee.Name(n) => n.value -> n.symbol
        case Importee.Rename(n, rename) => rename.value -> n.symbol
      }
    }.toMap

  def declaration(info: SymbolInformation, owner: Symbol, wildcards: Map[Symbol, String]): Option[String] =
    info.signature match {
      case TypeSignature(Nil, lo, hi) if lo == hi =>
        render(lo, owner, wildcards).map(r => s"type ${info.displayName} = ${r.text}")
      case TypeSignature(Nil, _, _) =>
        bounds(info, owner, wildcards).map(b => s"type ${info.displayName}$b")
      case ValueSignature(tpe) =>
        render(tpe, owner, wildcards).map(r => s"val ${info.displayName}: ${r.text}")
      case MethodSignature(Nil, Nil, tpe) =>
        render(tpe, owner, wildcards).map(r => s"${if (info.isVal) "val" else "def"} ${info.displayName}: ${r.text}")
      case _ =>
        None
    }

  def bounds(info: SymbolInformation, owner: Symbol, wildcards: Map[Symbol, String]): Option[String] =
    info.signature match {
      case TypeSignature(Nil, lo, hi) =>
        for {
          rlo <- bound(lo, NothingSymbol, ">:", owner, wildcards)
          rhi <- bound(hi, AnySymbol, "<:", owner, wildcards)
        } yield rlo + rhi
      case _ =>
        None
    }

  def bound(tpe: SemanticType, trivial: Symbol, relation: String, owner: Symbol, wildcards: Map[Symbol, String]): Option[String] =
    tpe match {
      case TypeRef(NoType, `trivial`, Nil) | NoType => Some("")
      case _ => render(tpe, owner, wildcards).map(r => s" $relation ${r.text}")
    }
}

object TypePrinter {
  sealed trait Kind
  case object Simple extends Kind
  case class Infix(op: String) extends Kind
  case object Function extends Kind
  case object Compound extends Kind

  case class Rendered(text: String, kind: Kind)

  val NothingSymbol = Symbol("scala/Nothing#")
  val AnySymbol = Symbol("scala/Any#")

  /** Owners whose members are visible by their names without imports */
  val alwaysVisible: Set[Symbol] = Set(Symbol("scala/"), Symbol("scala/package."), Symbol("scala/Predef."), Symbol("java/lang/"))

  /** The types aliased by the `scala` package and `Predef` (eg. `List` for `scala.collection.immutable.List`) */
  val aliases: Map[Symbol, String] = Map(
    "scala/collection/immutable/List#" -> "List",
    "scala/collection/immutable/Seq#" -> "Seq",
    "scala/collection/immutable/IndexedSeq#" -> "IndexedSeq",
    "scala/collection/immutable/Vector#" -> "Vector",
    "scala/collection/immutable/LazyList#" -> "LazyList",
    "scala/collection/immutable/Range#" -> "Range",
    "scala/collection/immutable/Map#" -> "Map",
    "scala/collection/immutable/Set#" -> "Set",
    "scala/collection/Iterable#" -> "Iterable",
    "scala/collection/Iterator#" -> "Iterator",
    "scala/collection/mutable/StringBuilder#" -> "StringBuilder",
    "scala/math/BigInt#" -> "BigInt",
    "scala/math/BigDecimal#" -> "BigDecimal",
    "scala/util/Either#" -> "Either",
    "scala/util/Left#" -> "Left",
    "scala/util/Right#" -> "Right",
    "java/lang/String#" -> "String"
  ).map { case (sym, alias) => Symbol(sym) -> alias }

  val TupleN = """scala/Tuple(\d+)#""".r
  val FunctionN = """scala/Function(\d+)#""".r

  def applied(sym: Symbol, name: String, args: List[Rendered]): Rendered = (sym.value, args) match {
    case (_, Nil) =>
      Rendered(name, Simple)
    case (TupleN(n), _) if n.toInt == args.length && args.length > 1 =>
      Rendered(args.map(_.text).mkString("(", ", ", ")"), Simple)
    case (FunctionN(n), _) if n.toInt + 1 == args.length =>
      val params = args.init match {
        case List(param) if param.kind != Function => param.text
        case params => params.map(_.text).mkString("(", ", ", ")")
      }
      Rendered(s"$params => ${args.last.text}", Function)
    case (_, List(left, right)) if isSymbolic(name) =>
      Rendered(s"${infixOperand(left, name, isLeft = true)} $name ${infixOperand(right, name, isLeft = false)}", Infix(name))
    case _ =>
      Rendered(args.map(_.text).mkString(s"$name[", ", ", "]"), Simple)
  }

  def infixOperand(operand: Rendered, op: String, isLeft: Boolean): String = operand.kind match {
    case Simple | Compound => operand.text
    case Infix(`op`) if isLeft != op.endsWith(":") => operand.text
    case _ => s"(${operand.text})"
  }

  def compoundOperand(operand: Rendered): String = operand.kind match {
    case Simple | Compound => operand.text
    case _ => s"(${operand.text})"
  }

  def simpleOperand(operand: Rendered): String = operand.kind match {
    case Simple => operand.text
    case _ => s"(${operand.text})"
  }

  def isSymbolic(name: String): Boolean = name.nonEmpty && name.forall(c => !c.isLetterOrDigit && c != '_')

  def isTypeParameter(sym: Symbol): Boolean = sym.value.endsWith("]")

  def isParameter(sym: Symbol): Boolean = sym.value.endsWith(")")

  /** Whether `sym` is a class or trait, whose members are referred to by their names within it */
  def isClass(sym: Symbol): Boolean = sym.value.endsWith("#")

  def isOverridingTerm(info: SymbolInformation): Boolean = !info.isType && info.overriddenSymbols.nonEmpty

  def isAnyRef(tpe: SemanticType): Boolean = tpe match {
    case TypeRef(_, sym, Nil) => sym.value == "scala/AnyRef#" || sym.value == "java/lang/Object#"
    case _ => false
  }

  def literal(constant: Constant): Option[String] = constant match {
    case UnitConstant => Some("Unit")
    case BooleanConstant(v) => Some(v.toString)
    case CharConstant(v) => Some(s"'$v'")
    case IntConstant(v) => Some(v.toString)
    case LongConstant(v) => Some(s"${v}L")
    case FloatConstant(v) => Some(s"${v}f")
    case DoubleConstant(v) => Some(v.toString)
    case StringConstant(v) => Some(Lit.String(v).syntax)
    case _ => None
  }

  def sequence[T](os: List[Option[T]]): Option[List[T]] =
    os.foldRight(Option(List.empty[T]))((o, acc) => for { t <- o; ts <- acc } yield t :: ts)
}
