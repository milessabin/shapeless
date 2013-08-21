/*
 * Copyright (c) 2011-13 Miles Sabin 
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

import sbt._

object Boilerplate {
  
  def gen(dir : File) = { 
    val tupler = dir / "shapeless" / "tupler.scala"
    IO.write(tupler, genTuplerInstances)
    
    val fntoproduct = dir / "shapeless" / "fntoproduct.scala"
    IO.write(fntoproduct, genFnToProductInstances)
    
    val fnfromproduct = dir / "shapeless" / "fnfromproduct.scala"
    IO.write(fnfromproduct, genFnFromProductInstances)
    
    val caseinst = dir / "shapeless" / "caseinst.scala"
    IO.write(caseinst, genCaseInst)

    val polyapply = dir / "shapeless" / "polyapply.scala"
    IO.write(polyapply, genPolyApply)

    val polyinst = dir / "shapeless" / "polyinst.scala"
    IO.write(polyinst, genPolyInst)

    val cases = dir / "shapeless" / "cases.scala"
    IO.write(cases, genCases)

    val polyntraits = dir / "shapeless" / "polyntraits.scala"
    IO.write(polyntraits, genPolyNTraits)

    val nats = dir / "shapeless" / "nats.scala"
    IO.write(nats, genNats)
    
    val tupletypeables = dir / "shapeless" / "tupletypeables.scala"
    IO.write(tupletypeables, genTupleTypeableInstances)

    val sizedbuilder = dir / "shapeless" / "sizedbuilder.scala"
    IO.write(sizedbuilder, genSizedBuilder)
    
    val hmapbuilder = dir / "shapeless" / "hmapbuilder.scala"
    IO.write(hmapbuilder, genHMapBuilder)
    
    Seq(
      tupler, fntoproduct, fnfromproduct, caseinst, polyapply,
      polyinst, cases, polyntraits, nats, tupletypeables, sizedbuilder,
      hmapbuilder
    )
  }

  def genHeader = {
    ("""|/*
        | * Copyright (c) 2011-13 Miles Sabin 
        | *
        | * Licensed under the Apache License, Version 2.0 (the "License");
        | * you may not use this file except in compliance with the License.
        | * You may obtain a copy of the License at
        | *
        | *     http://www.apache.org/licenses/LICENSE-2.0
        | *
        | * Unless required by applicable law or agreed to in writing, software
        | * distributed under the License is distributed on an "AS IS" BASIS,
        | * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
        | * See the License for the specific language governing permissions and
        | * limitations under the License.
        | */
        |
        |package shapeless
        |""").stripMargin
  }
  
  def genTuplerInstances = {
    def genInstance(arity : Int) = {
      val typeVars = (0 until arity) map (n => (n+'A').toChar)
      val typeArgs = typeVars.mkString("[", ", ", "]")
      val tupleType = if (arity == 1) "Tuple1[A]" else typeVars.mkString("(", ", ", ")")
      val hlistType = typeVars.mkString("", " :: ", " :: HNil")
      val hlistValue = ((1 to arity) map (n => "t._"+n)).mkString("", " :: ", " :: HNil")
      val pattern = ((0 until arity) map (n => (n+'a').toChar)).mkString("", " :: ", " :: HNil")
      val tupleValue = if (arity == 1) "Tuple1(a)" else ((0 until arity) map (n => (n+'a').toChar)).mkString("(", ", ", ")")
      
      ("""|
          |  implicit def hlistTupler"""+arity+typeArgs+""": Aux["""+hlistType+""", """+tupleType+"""] = new Tupler["""+hlistType+"""] {
          |    type Out = """+tupleType+"""
          |    def apply(l : """+hlistType+"""): Out = l match { case """+pattern+""" => """+tupleValue+""" }
          |  }
          |""").stripMargin
    }

    val instances = ((1 to 22) map genInstance).mkString
    
    genHeader+
    ("""|
        |package ops
        |
        |import hlist.Tupler
        |
        |trait TuplerInstances {
        |  type Aux[L <: HList, Out0] = Tupler[L] { type Out = Out0 }
        |"""+instances+"""}
        |""").stripMargin
  }
  
  def genFnToProductInstances = {
    def genInstance(arity : Int) = {
      val typeVars = (0 until arity) map (n => (n+'A').toChar)
      val typeArgs = (typeVars :+ "Res").mkString("[", ", ", "]")
      val fnType = typeVars.mkString("(", ", ", ")")+" => Res"
      val hlistType = (typeVars :+ "HNil").mkString(" :: ")
      val hlistFnType = "("+hlistType+") => Res"
      val pattern = ((0 until arity) map (n => (n+'a').toChar)).mkString("", " :: ", " :: HNil")
      val fnArgs = ((0 until arity) map (n => (n+'a').toChar)).mkString("(", ", ", ")")
      val fnBody = if (arity == 0) """fn()""" else """l match { case """+pattern+""" => fn"""+fnArgs+""" }""" 
      
      ("""|
          |  implicit def fnToProduct"""+arity+typeArgs+""": Aux["""+fnType+""", """+hlistFnType+"""] = new FnToProduct["""+fnType+"""] {
          |    type Out = """+hlistFnType+"""
          |    def apply(fn: """+fnType+"""): Out = (l : """+hlistType+""") => """+fnBody+"""
          |  }
          |""").stripMargin
    }

    val instances = ((0 to 22) map genInstance).mkString
    
    genHeader+
    ("""|
        |package ops
        |
        |import function.FnToProduct
        |
        |trait FnToProductInstances {
        |  type Aux[F, Out0] = FnToProduct[F] { type Out = Out0 }
        |"""+instances+"""}
        |""").stripMargin
  }
  
  def genFnFromProductInstances = {
    def genInstance(arity : Int) = {
      val typeVars = (0 until arity) map (n => (n+'A').toChar)
      val typeArgs = (typeVars :+ "Res").mkString("[", ", ", "]")
      val fnType = typeVars.mkString("(", ", ", ")")+" => Res"
      val hlistType = (typeVars :+ "HNil").mkString(" :: ")
      val hlistFnType = "("+hlistType+") => Res"
      val litArgs = ((0 until arity) map (n => (n+'a').toChar+" : "+(n+'A').toChar)).mkString("(", ", ", ")")
      val hlistFnArgs = (((0 until arity) map (n => (n+'a').toChar)) :+ "HNil").mkString("", " :: ", "")
      
      ("""|
          |  implicit def fnFromProduct"""+arity+typeArgs+""": Aux["""+hlistFnType+""", """+fnType+"""] = new FnFromProduct["""+hlistFnType+"""] {
          |    type Out = """+fnType+"""
          |    def apply(hf : """+hlistFnType+"""): Out = """+litArgs+""" => hf("""+hlistFnArgs+""")
          |  }
          |""").stripMargin
    }

    val instances = ((0 to 22) map genInstance).mkString
    
    genHeader+
    ("""|
        |package ops
        |
        |import function.FnFromProduct
        |
        |trait FnFromProductInstances {
        |  type Aux[F, Out0] = FnFromProduct[F] { type Out = Out0 }
        |"""+instances+"""}
        |""").stripMargin
  }
  
  def genCaseInst = {
    def genInst(arity : Int) = {
      val typeVars = (0 until arity) map (n => (n+'A').toChar)
      val typeArgs = typeVars.mkString(", ")
      val fnType = typeVars.mkString("(", ", ", ")")+" => Res"
      val hlistType = (typeVars :+ "HNil").mkString(" :: ")
      val fnArgs = ((0 until arity) map (n => (n+'a').toChar+" : "+(n+'A').toChar)).mkString("(", ", ", ")")
      val caseArgs = ((0 until arity) map (n => (n+'a').toChar)).mkString("(", " :: ", " :: HNil)")
      
      ("""|
          |  implicit def inst"""+arity+"""[Fn <: Poly, """+typeArgs+""", Res](cse : Case[Fn, """+hlistType+"""] { type Result = Res }) : """+fnType+""" = """+fnArgs+""" => cse.value"""+caseArgs+"""
          |""").stripMargin
    }

    val insts = ((1 to 22) map genInst).mkString
    
    genHeader+
    ("""|
        |trait CaseInst {
        |  import poly._
        |
        |"""+insts+"""
        |}
        |""").stripMargin
  }

  def genPolyApply = {
    def genApply(arity : Int) = {
      val typeVars = (0 until arity) map (n => (n+'A').toChar)
      val typeArgs = typeVars.mkString("[", ", ", "]")
      val hlistType = (typeVars :+ "HNil").mkString(" :: ")
      val fnArgs = ((0 until arity) map (n => (n+'a').toChar+" : "+(n+'A').toChar)).mkString("(", ", ", ")")
      val caseArgs = ((0 until arity) map (n => (n+'a').toChar)).mkString("(", " :: ", " :: HNil)")
      
      ("""|
          |  def apply"""+typeArgs+fnArgs+"""(implicit cse : Case[this.type, """+hlistType+"""]) : cse.Result = cse"""+caseArgs+"""
          |""").stripMargin
    }

    val applies = ((1 to 22) map genApply).mkString
    
    genHeader+
    ("""|
        |trait PolyApply {
        |  import poly._
        |
        |"""+applies+"""
        |}
        |""").stripMargin
  }

  def genPolyInst = {
    def genInst(arity : Int) = {
      val typeVars = (0 until arity) map (n => (n+'A').toChar)
      val typeArgs = typeVars.mkString(", ")
      val fnType = typeVars.mkString("(", ", ", ")")+" => cse.Result"
      val hlistType = (typeVars :+ "HNil").mkString(" :: ")
      val fnArgs = ((0 until arity) map (n => (n+'a').toChar+" : "+(n+'A').toChar)).mkString("(", ", ", ")")
      val caseArgs = ((0 until arity) map (n => (n+'a').toChar)).mkString("(", " :: ", " :: HNil)")
      
      ("""|
          |  implicit def inst"""+arity+"""["""+typeArgs+"""](fn : Poly)(implicit cse : fn.ProductCase["""+hlistType+"""]) : """+fnType+""" = """+fnArgs+""" => cse"""+caseArgs+"""
          |""").stripMargin
    }

    val insts = ((1 to 22) map genInst).mkString
    
    genHeader+
    ("""|
        |trait PolyInst {
        |"""+insts+"""
        |}
        |""").stripMargin
  }

  def genCases = {
    def genCase(arity : Int) = {
      val typeVars = (0 until arity) map (n => (n+'A').toChar)
      val typeArgs = typeVars.mkString(", ")
      val fnType = typeVars.mkString("(", ", ", ")")+" => Result0"
      val hlistType = (typeVars :+ "HNil").mkString(" :: ")
      val pattern = ((0 until arity) map (n => (n+'a').toChar)).mkString("", " :: ", " :: HNil")
      val fnArgs = ((0 until arity) map (n => (n+'a').toChar)).mkString("(", ", ", ")")
      val fnBody = """l match { case """+pattern+""" => fn"""+fnArgs+""" }""" 
      
      ("""|
          |  type Case"""+arity+"""[Fn, """+typeArgs+"""] = Case[Fn, """+hlistType+"""]
          |  object Case"""+arity+""" {
          |    type Aux[Fn, """+typeArgs+""", Result0] = Case[Fn, """+hlistType+"""] { type Result = Result0 }
          |
          |    def apply[Fn, """+typeArgs+""", Result0](fn : """+fnType+"""): Aux[Fn, """+typeArgs+""", Result0] =
          |      new Case[Fn, """+hlistType+"""] {
          |        type Result = Result0
          |         val value = (l : """+hlistType+""") => """+fnBody+"""
          |      }
          |  }
          |""").stripMargin
    }

    val cases = ((1 to 22) map genCase).mkString
    
    genHeader+
    ("""|
        |trait Cases {
        |  import poly._
        |
        |"""+cases+"""
        |}
        |""").stripMargin
  }

  def genPolyNTraits = {
    def genTrait(arity : Int) = {
      val typeVars = (0 until arity) map (n => (n+'A').toChar)
      val typeArgsList = typeVars.mkString(", ")
      val typeArgs = typeVars.mkString("[", ", ", "]")
      val fnType = typeVars.mkString("(", ", ", ")")+" => Res"
      val hlistType = (typeVars :+ "HNil").mkString(" :: ")
      val pattern = ((0 until arity) map (n => (n+'a').toChar)).mkString("", " :: ", " :: HNil")
      val fnArgs = ((0 until arity) map (n => (n+'a').toChar)).mkString("(", ", ", ")")
      val fnBody = if (arity == 0) """fn()""" else """l match { case """+pattern+""" => fn"""+fnArgs+""" }""" 
      
      ("""|
          |trait Poly"""+arity+""" extends Poly { outer =>
          |  type Case"""+typeArgs+""" = poly.Case[this.type, """+hlistType+"""]
          |  object Case {
          |    type Aux["""+typeArgsList+""", Result0] = poly.Case[outer.type, """+hlistType+"""] { type Result = Result0 }
          |  }
          |
          |  class CaseBuilder"""+typeArgs+""" {
          |    def apply[Res](fn: """+fnType+""") = new Case["""+typeArgsList+"""] {
          |      type Result = Res
          |      val value = (l : """+hlistType+""") => """+fnBody+"""
          |    }
          |  }
          |  
          |  def at"""+typeArgs+""" = new CaseBuilder"""+typeArgs+"""
          |}
          |""").stripMargin
    }

    val traits = ((1 to 22) map genTrait).mkString
    
    genHeader+
    ("""|
        |"""+traits+"""
        |""").stripMargin
  }
  
  def genNats = {
    def genNat(n : Int) = {
      ("""|
          |  type _"""+n+""" = Succ[_"""+(n-1)+"""]
          |  val _"""+n+""": _"""+n+""" = new _"""+n+"""
          |""").stripMargin
    }
    
    val nats = ((1 to 22) map genNat).mkString
    
    genHeader+
    ("""|
        |trait Nats {
        |"""+nats+"""}
        |""").stripMargin
  }
  
  def genTupleTypeableInstances = {
    def genInstance(arity : Int) = {
      val typeVars = (0 until arity) map (n => (n+'A').toChar)
      val typeArgs = typeVars.mkString("[", ", ", "]")
      val tupleType = if (arity == 1) "Tuple1[A]" else typeVars.mkString("(", ", ", ")")
      val wildcardTupleType = if (arity == 1) "Tuple1[_]" else "("+("_, "*(arity-1))+"_)"
      val implicitArgs = (typeVars map(a => "cast"+a+" : Typeable["+a+"]")).mkString("(implicit ", ", ", ")")
      val enumerators = ((0 until arity) map (n => "_ <- p._"+(n+1)+".cast["+(n+'A').toChar+"]")).mkString("(", "; ", ")")
      
      ("""|
          |  implicit def tuple"""+arity+"""Typeable"""+typeArgs+implicitArgs+""" = new Typeable["""+tupleType+"""] {
          |    def cast(t : Any) : Option["""+tupleType+"""] = {
          |      if(t == null) Some(t.asInstanceOf["""+tupleType+"""])
          |      else if(t.isInstanceOf["""+wildcardTupleType+"""]) {
          |        val p = t.asInstanceOf["""+wildcardTupleType+"""]
          |        for"""+enumerators+""" yield t.asInstanceOf["""+tupleType+"""]
          |      } else None
          |    }
          |  }
          |""").stripMargin
    }
    
    val instances = ((1 to 22) map genInstance).mkString
    
    genHeader+
    ("""|
        |trait TupleTypeableInstances {
        |  import syntax.typeable._
        |"""+instances+"""}
        |""").stripMargin
  }
  
  def genSizedBuilder = {
    def genInstance(arity : Int) = {
      val argVars = (0 until arity) map (n => (n+'a').toChar)
      val args = argVars.mkString("(", " : T, ", " : T)")
      val appendArgs = argVars.mkString("(", ", ", ")")

      ("""|
          |  def apply[T]"""+args+"""(implicit cbf : CanBuildFrom[Nothing, T, CC[T]]) = 
          |    wrap[CC[T], _"""+arity+"""]((cbf() += """+appendArgs+""").result)
          |""").stripMargin
    }

    val instances = ((1 to 22) map genInstance).mkString
    
    genHeader+
    ("""|
        |class SizedBuilder[CC[_]] {
        |  import scala.collection.generic.CanBuildFrom
        |  import nat._
        |  import Sized.wrap
        |"""+instances+"""}
        |""").stripMargin
  }
  
  def genHMapBuilder = {
    def genInstance(arity : Int) = {
      val typeArgs = ((0 until arity) map (n => "K"+n+", V"+n)).mkString("[", ", ", "]")
      val args = ((0 until arity) map (n => "e"+n+" : (K"+n+", V"+n+")")).mkString("(", ", ", ")")
      val witnesses = ((0 until arity) map (n => "ev"+n+" : R[K"+n+", V"+n+"]")).mkString("(implicit ", ", ", ")")
      val mapArgs = ((0 until arity) map (n => "e"+n)).mkString("(", ", ", ")")
      
      ("""|
          |  def apply"""+typeArgs+"""
          |    """+args+"""
          |    """+witnesses+"""
          |    = new HMap[R](Map"""+mapArgs+""")
          |""").stripMargin
    }
    
    val instances = ((1 to 10) map genInstance).mkString

    genHeader+
    ("""|
        |class HMapBuilder[R[_, _]] {
        |"""+instances+"""}
        |""").stripMargin
    
  }
}
