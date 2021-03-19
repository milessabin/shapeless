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
