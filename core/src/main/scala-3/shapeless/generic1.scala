/*
 * Copyright (c) 2015-18 Miles Sabin
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

trait Generic1ScalaCompat {

  implicit def mkGeneric10[T[_], U[_], FR[_[_], _[_]]]: Generic1[T, [t[_]] =>> FR[t, U]] = ???

  implicit def mkGeneric11[T[_], U[_], FR[_[_], _[_]]]: Generic1[T, [t[_]] =>> FR[U, t]] = ???
}

trait Generic10ScalaCompat {
  implicit def apply[T[_], FR[_[_]]]: Generic1[T, FR] = ???
}

trait IsHCons1ScalaCompat {

  implicit def mkIsHCons10[L[_], FH[_[_], _[_]], U[_], FT[_[_]]]: IsHCons1[L, [t[_]] =>> FH[t, U], FT] = ???

  implicit def mkIsHCons11[L[_], FH[_[_], _[_]], U[_], FT[_[_]]]: IsHCons1[L, [t[_]] =>> FH[U, t], FT] = ???

  implicit def mkIsHCons12[L[_], FH[_[_]], FT[_[_], _[_]], U[_]]: IsHCons1[L, FH, [t[_]] =>> FT[t, U]] = ???

  implicit def mkIsHCons13[L[_], FH[_[_]], FT[_[_], _[_]], U[_]]: IsHCons1[L, FH, [t[_]] =>> FT[U, t]] = ???
}

trait IsHCons10ScalaCompat {
  implicit def apply[L[_], FH[_[_]], FT[_[_]]]: IsHCons1[L, FH, FT] = ???
}

trait IsCCons1ScalaCompat {

  implicit def mkIsCCons10[L[_], FH[_[_], _[_]], U[_], FT[_[_]]]: IsCCons1[L, [t[_]] =>> FH[t, U], FT] = ???

  implicit def mkIsCCons11[L[_], FH[_[_], _[_]], U[_], FT[_[_]]]: IsCCons1[L, [t[_]] =>> FT[U, t], FT] = ???

  implicit def mkIsCCons12[L[_], FH[_[_]], FT[_[_], _[_]], U[_]]: IsCCons1[L, FH, [t[_]] =>> FT[t, U]] = ???

  implicit def mkIsCCons13[L[_], FH[_[_]], FT[_[_], _[_]], U[_]]: IsCCons1[L, FH, [t[_]] =>> FT[U, t]] = ???
}

trait IsCCons10ScalaCompat {
  implicit def apply[L[_], FH[_[_]], FT[_[_]]]: IsCCons1[L, FH, FT] = ???
}

trait Split1ScalaCompat {

  implicit def mkSplit10[L[_], FO[_[_], _[_]], U[_], FI[_[_]]]: Split1[L, [t[_]] =>> FO[t, U], FI] = ???

  implicit def mkSplit11[L[_], FO[_[_], _[_]], U[_], FI[_[_]]]: Split1[L, [t[_]] =>> FO[U, t], FI] = ???

  implicit def mkSplit12[L[_], FO[_[_]], FI[_[_], _[_]], U[_]]: Split1[L, FO, [t[_]] =>> FI[t, U]] = ???

  implicit def mkSplit13[L[_], FO[_[_]], FI[_[_], _[_]], U[_]]: Split1[L, FO, [t[_]] =>> FI[U, t]] = ???
}

trait Split10ScalaCompat {
  implicit def apply[L[_], FO[_[_]], FI[_[_]]]: Split1[L, FO, FI] = ???
}
