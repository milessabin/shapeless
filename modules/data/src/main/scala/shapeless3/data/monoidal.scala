/*
 * Copyright (c) 2020 Miles Sabin
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

package shapeless3.data

import scala.compiletime._

trait Monoidal {
  type to[_] <: Tuple
  type from[_ <: Tuple]

  type compose[f[_], g[_]] = [t] =>> g[f[t]]

  type length[m] = Monoidal.length[to[m]]
  type reverse[m] = from[Monoidal.reverse[to[m]]]
  type flatten[mm] = from[Monoidal.flatten[Monoidal.map[to[mm]][to]]]
  type map[m] = [f[_]] =>> from[Monoidal.map[to[m]][f]]
  type flatMap[m] = [f[_]] =>> from[Monoidal.flatten[Monoidal.map[to[m]][compose[f, to]]]]
  type select[m, n <: Int] = from[Monoidal.select[to[m], n]]
}

trait Cartesian extends Monoidal {
  type reversePrepend[m, n] = from[Monoidal.reversePrepend[to[m], to[n]]]
  type concat[m, n] = from[Monoidal.concat[to[m], to[n]]]
  type zipWith[m, n] = [f[_, _]] =>> from[Monoidal.zipWith[to[m], to[n]][f]]
  type at[m, n <: Int] = Monoidal.at[to[m], n]
}

trait Cocartesian extends Monoidal {
  type reverseExtend[m, n] = from[Monoidal.reversePrepend[to[m], to[n]]]
  type extend[m, n] = from[Monoidal.concat[to[m], to[n]]]
}

object Monoidal {
  type length[m <: Tuple] = m match {
    case EmptyTuple => 0
    case hd *: tl => S[length[tl]]
  }

  type reverse[m <: Tuple] = reverseAcc[m, EmptyTuple]
  type reverseAcc[m <: Tuple, acc <: Tuple] <: Tuple = m match {
    case hd *: tl => reverseAcc[tl, hd *: acc]
    case EmptyTuple => acc
  }

  type map[m <: Tuple] = [f[_]] =>> map0[m, f]
  type map0[m <: Tuple, f[_]] <: Tuple = m match {
    case EmptyTuple => EmptyTuple
    case hd *: tl => f[hd] *: map0[tl, f]
  }

  type flatten[mm <: Tuple] = flattenAcc[mm, EmptyTuple]
  type flattenAcc[mm <: Tuple, acc <: Tuple] <: Tuple = mm match {
    case hd *: tl => flattenAcc[tl, reversePrepend[hd, acc]]
    case EmptyTuple => reverse[acc]
  }

  type reversePrepend[m <: Tuple, n <: Tuple] <: Tuple = m match {
    case hd *: tl => reversePrepend[tl, hd *: n]
    case EmptyTuple => n
  }

  type concat[m <: Tuple, n <: Tuple] = reversePrepend[reverse[m], n]

  type zipWith[m <: Tuple, n <: Tuple] = [f[_, _]] =>> zipWithAcc[m, n, EmptyTuple, f]
  type zipWithAcc[m <: Tuple, n <: Tuple, acc <: Tuple, f[_, _]] <: Tuple = (m, n) match {
    case (hm *: tm, hn *: tn) => zipWithAcc[tm, tn, f[hm, hn] *: acc, f]
    case (EmptyTuple, EmptyTuple) => reverse[acc]
  }

  type at[m <: Tuple, n <: Int] = (m, n) match {
    case (hd *: _, 0) => hd
    case (_ *: tl, S[p]) => at[tl, p]
  }

  type select[m <: Tuple, n <: Int] = selectAcc[m, n, EmptyTuple]
  type selectAcc[m <: Tuple, n <: Int, acc <: Tuple] <: Tuple = (m, n) match {
    case (hd *: tl, 0) => hd *: reversePrepend[acc, tl]
    case (hd *: tl, S[p]) => selectAcc[tl, p, hd *: acc]
  }
}

trait UnboundedMonoidal[T0[_, _], U0] extends Monoidal{
  type to[t] <: Tuple = t match {
    case T0[hd, tl] => hd *: to[tl]
    case U0 => EmptyTuple
  }

  type from[t <: Tuple] = t match {
    case hd *: tl => T0[hd, from[tl]]
    case EmptyTuple => U0
  }
}

trait BoundedMonoidal[B, T0[_, _ <: B] <: B, U0 <: B] extends Monoidal {
  type to[t] <: Tuple = t match {
    case T0[hd, tl] => hd *: to[tl]
    case U0 => EmptyTuple
  }

  type from[t <: Tuple] = t match {
    case hd *: tl => T0[hd, from[tl]]
    case EmptyTuple => U0
  }
}

trait UnitlessMonoidal[T0[_, _]] extends Monoidal {
  type to[t] <: Tuple = t match {
    case T0[hd, tl] => hd *: to[tl]
    case _ => t *: EmptyTuple
  }

  type from[t <: Tuple] = t match {
    case hd *: tl => from0[hd, tl]
  }
  type from0[p, t <: Tuple] = t match {
    case hd *: tl => T0[p, from0[hd, tl]]
    case EmptyTuple => p
  }
}

trait DirectMonoidal[T0[_ <: Tuple]] extends Monoidal {
  type to[t] <: Tuple = t match {
    case T0[l] => l
  }
  type from[t <: Tuple] = T0[t]
}

object tuples extends Monoidal with Cartesian {
  type to[t] = t&Tuple
  type from[t <: Tuple] = t
}

object pairs extends UnboundedMonoidal[Tuple2, Unit] with Cartesian

object eithers extends Cocartesian {
  // Nothing needs special handling ... perhaps we should drop this
  // encoding of coproducts altogether?
  //
  // The equivalent of CNil would be Nothing and no match type applied
  // to Nothing will reduce, so bare CNil is more or less impossible to
  // express. That being so, the unitless version is probably a better
  // choice.
  class Wrap[T]

  type to[t] <: Tuple = Wrap[t] match {
    case Wrap[Either[hd, tl]] => hd *: to[tl]
    case Wrap[Nothing] => EmptyTuple
  }

  type from[t <: Tuple] = t match {
    case hd *: tl => Either[hd, from[tl]]
    case EmptyTuple => Nothing
  }
}

object pairs2 extends UnitlessMonoidal[Tuple2] with Cartesian

object eithers2 extends UnitlessMonoidal[Either] with Cocartesian

// This is fine in the same source file/compilation run, but fails when
// separately compiled as a test
object MonoidalTest {
  trait Cp[L]

  object coproduct extends DirectMonoidal[Cp] with Cocartesian

  type Field[K, V]

  type invOption[t] = t match {
    case Option[u] => u
  }

  type pairp[t] = (t, (t, Unit))
  type pairt[t] = (t, t)
  type pairp2[t] = (t, t)

  type orInt[t] = Either[t, Either[Int, Nothing]]
  type orInt2[t] = Either[t, Int]
  type orIntc[t] = Cp[(t, Int)]

  type rotate[t] = t match {
    case Int => String
    case String => Boolean
    case Boolean => Int
  }

  type p = (Int, (String, (Boolean, Unit)))
  type pb = (Double, (Char, Unit))
  type pc = ("i", ("s", ("b", Unit)))
  type pd = ((Int, (String, Unit)), ((Boolean, Unit), (Unit, ((Double, (Char, Unit)), Unit))))
  summon[pairs.length[p] =:= 3]
  summon[pairs.reverse[p] =:= (Boolean, (String, (Int, Unit)))]
  summon[pairs.map[p][Option] =:= (Option[Int], (Option[String], (Option[Boolean], Unit)))]
  summon[pairs.map[p][rotate] =:= (String, (Boolean, (Int, Unit)))]
  summon[pairs.map[(Option[Int], (Option[String], (Option[Boolean], Unit)))][invOption] =:= p]
  summon[pairs.reversePrepend[p, pb] =:= (Boolean, (String, (Int, (Double, (Char, Unit)))))]
  summon[pairs.concat[p, pb] =:= (Int, (String, (Boolean, (Double, (Char, Unit)))))]
  summon[pairs.zipWith[pc, p][Field] =:= (Field["i", Int], (Field["s", String], (Field["b", Boolean], Unit)))]
  summon[pairs.flatten[pd] =:= (Int, (String, (Boolean, (Double, (Char, Unit)))))]
  summon[pairs.flatMap[pb][pairp] =:= (Double, (Double, (Char, (Char, Unit))))]
  summon[pairs.at[p, 1] =:= String]
  summon[pairs.select[p, 1] =:= (String, (Int, (Boolean, Unit)))]

  type p2 = (Int, (String, Boolean))
  type p2b = (Double, Char)
  type p2c = ("i", ("s", "b"))
  type p2d = ((Int, (String, Boolean)), (Double, Char))
  summon[pairs2.length[p2] =:= 3]
  summon[pairs2.reverse[p2] =:= (Boolean, (String, Int))]
  summon[pairs2.map[p2][Option] =:= (Option[Int], (Option[String], Option[Boolean]))]
  summon[pairs2.map[p2][rotate] =:= (String, (Boolean, Int))]
  summon[pairs2.map[(Option[Int], (Option[String], Option[Boolean]))][invOption] =:= p2]
  summon[pairs2.concat[p2, p2b] =:= (Int, (String, (Boolean, (Double, Char))))]
  summon[pairs2.zipWith[p2c, p2][Field] =:= (Field["i", Int], (Field["s", String], Field["b", Boolean]))]
  summon[pairs2.flatten[p2d] =:= (Int, (String, (Boolean, (Double, Char))))]
  summon[pairs2.flatMap[p2b][pairp2] =:= (Double, (Double, (Char, Char)))]
  summon[pairs2.at[p2, 1] =:= String]
  summon[pairs2.select[p2, 1] =:= (String, (Int, Boolean))]

  type t = (Int, String, Boolean)
  type tb = (Double, Char)
  type tc = ("i", "s", "b")
  type td = ((Int, String), Tuple1[Boolean], EmptyTuple, (Double, Char))
  summon[tuples.length[t] =:= 3]
  summon[tuples.reverse[t] =:= (Boolean, String, Int)]
  summon[tuples.map[t][Option] =:= (Option[Int], Option[String], Option[Boolean])]
  summon[tuples.map[t][rotate] =:= (String, Boolean, Int)]
  summon[tuples.map[(Option[Int], Option[String], Option[Boolean])][invOption] =:= t]
  summon[tuples.concat[t, tb] =:= (Int, String, Boolean, Double, Char)]
  summon[tuples.zipWith[tc, t][Field] =:= (Field["i", Int], Field["s", String], Field["b", Boolean])]
  summon[tuples.flatten[td] =:= (Int, String, Boolean, Double, Char)]
  summon[tuples.flatMap[tb][pairt] =:= (Double, Double, Char, Char)]
  summon[tuples.at[t, 1] =:= String]
  summon[tuples.select[t, 1] =:= (String, Int, Boolean)]

  type e = Either[Int, Either[String, Either[Boolean, Nothing]]]
  type eb = Either[Double, Either[Char, Nothing]]
  type ed = Either[Either[Int, Either[String, Nothing]], Either[Either[Boolean, Nothing], Either[Either[Double, Either[Char, Nothing]], Nothing]]]
  summon[eithers.length[e] =:= 3]
  summon[eithers.reverse[e] =:= Either[Boolean, Either[String, Either[Int, Nothing]]]]
  summon[eithers.map[e][Option] =:= Either[Option[Int], Either[Option[String], Either[Option[Boolean], Nothing]]]]
  summon[eithers.map[e][rotate] =:= Either[String, Either[Boolean, Either[Int, Nothing]]]]
  summon[eithers.map[Either[Option[Int], Either[Option[String], Either[Option[Boolean], Nothing]]]][invOption] =:= e]
  summon[eithers.extend[e, eb] =:= Either[Int, Either[String, Either[Boolean, Either[Double, Either[Char, Nothing]]]]]]
  summon[eithers.flatten[ed] =:= Either[Int, Either[String, Either[Boolean, Either[Double, Either[Char, Nothing]]]]]]
  summon[eithers.flatMap[eb][orInt] =:= Either[Double, Either[Int, Either[Char, Either[Int, Nothing]]]]]
  summon[eithers.select[e, 1] =:= Either[String, Either[Int, Either[Boolean, Nothing]]]]

  type e2 = Either[Int, Either[String, Boolean]]
  type e2b = Either[Double, Char]
  type e2d = Either[Either[Int, Either[String, Boolean]], Either[Double, Char]]
  summon[eithers2.length[e2] =:= 3]
  summon[eithers2.reverse[e2] =:= Either[Boolean, Either[String, Int]]]
  summon[eithers2.map[e2][Option] =:= Either[Option[Int], Either[Option[String], Option[Boolean]]]]
  summon[eithers2.map[e2][rotate] =:= Either[String, Either[Boolean, Int]]]
  summon[eithers2.map[Either[Option[Int], Either[Option[String], Option[Boolean]]]][invOption] =:= e2]
  summon[eithers2.extend[e2, e2b] =:= Either[Int, Either[String, Either[Boolean, Either[Double, Char]]]]]
  summon[eithers2.flatten[e2d] =:= Either[Int, Either[String, Either[Boolean, Either[Double, Char]]]]]
  summon[eithers2.flatMap[e2b][orInt2] =:= Either[Double, Either[Int, Either[Char, Int]]]]
  summon[eithers2.select[e2, 1] =:= Either[String, Either[Int, Boolean]]]

  type c = Cp[(Int, String, Boolean)]
  type cb = Cp[(Double, Char)]
  type cd = Cp[(Cp[(Int, String)], Cp[Tuple1[Boolean]], Cp[EmptyTuple], Cp[(Double, Char)])]
  summon[coproduct.length[c] =:= 3]
  summon[coproduct.reverse[c] =:= Cp[(Boolean, String, Int)]]
  summon[coproduct.map[c][Option] =:= Cp[(Option[Int], Option[String], Option[Boolean])]]
  summon[coproduct.map[c][rotate] =:= Cp[(String, Boolean, Int)]]
  summon[coproduct.map[Cp[(Option[Int], Option[String], Option[Boolean])]][invOption] =:= c]
  summon[coproduct.extend[c, cb] =:= Cp[(Int, String, Boolean, Double, Char)]]
  summon[coproduct.flatten[cd] =:= Cp[(Int, String, Boolean, Double, Char)]]
  summon[coproduct.flatMap[cb][orIntc] =:= Cp[(Double, Int, Char, Int)]]
  summon[coproduct.select[c, 1] =:= Cp[(String, Int, Boolean)]]
}
