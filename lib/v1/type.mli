(* Copyright (C) 2022  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the LGPL-3.0 Linking Exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * and the LGPL-3.0 Linking Exception along with this library.  If not, see
 * <http://www.gnu.org/licenses/> and <https://spdx.org>, respectively.
 *)

type haystack = ..

type (_, _) tag =
  | Anon : (unit, 'a) tag
  | Label : string -> (string, 'a) tag
  | Annot : ('l, 'a) tag * 'a -> ('l, 'a) tag

type ('p, 'a) name = ..
type field_annot = ..
type field_tag = (string, field_annot) tag
type constructor_annot = ..
type constructor_tag = (string, constructor_annot) tag

val label : (string, _) tag -> string

type (_, _) param =
  | Z : ('a -> 'p, 'a) param
  | S : ('p, 'a) param -> ('b -> 'p, 'a) param

type _ atom =
  | Int : int atom
  | Char : char atom
  | Bytes : bytes atom
  | String : string atom
  | Float : float atom
  | Bool : bool atom
  | Int32 : int32 atom
  | Int64 : int64 atom
  | Nativeint : nativeint atom

[@@@warning "-30"]

type (_, _) t =
  | Param : ('p, 'a) param -> ('p, 'a) t
  | App : ('a -> 'p, 'b) t * ('p, 'a) t -> ('p, 'b) t
  | Atom : 'a atom -> ('p, 'a) t
  | Nominal : ('p, 'a) name * ('p, 'a) descr -> ('p, 'a) t

and (_, _) descr =
  | Fix : ('a -> 'p, 'a) descr -> ('p, 'a) descr
  | Tuple : 'i * (unit, 'p, 'a, 'i) product -> ('p, 'a) descr
  | Record : 'i * (field_tag, 'p, 'a, 'i) product -> ('p, 'a) descr
  | Variant : 'e * (constructor_tag, 'p, 'a, 'e) sum -> ('p, 'a) descr

and (_, _, _, _) product =
  | (::) :
      ('tag * ('p, 'b) t * ('a -> 'b)) * ('tag, 'p, 'a, 'i) product ->
      ('tag, 'p, 'a, 'b -> 'i) product
  | [] :
      ('tag, 'p, 'a, 'a) product

and (_, _, _, _) sum =
  | (::) :
      ('tag * ('p, 'b) t * ('b -> 'a)) * ('tag, 'p, 'a, 'e) sum ->
      ('tag, 'p, 'a, ('b -> haystack) -> 'e) sum
  | [] :
      ('tag, 'p, 'a, 'a -> haystack) sum

type nonrec 'a list = 'a list = [] | (::) of 'a * 'a list

[@@@warning "+30"]

val ($) : ('a -> 'p, 'b) t -> ('p, 'a) t -> ('p, 'b) t

val int : ('p, int) t
val char : ('p, char) t
val bytes : ('p, bytes) t
val string : ('p, string) t
val float : ('p, float) t
val bool : ('p, bool) t
val int32 : ('p, int32) t
val int64 : ('p, int64) t
val nativeint : ('p, nativeint) t

type (_, _) name +=
  | Unit : ('p, unit) name
  | T2 : ('a1 -> 'a2 -> 'p, 'a1 * 'a2) name
  | T3 : ('a1 -> 'a2 -> 'a3 -> 'p, 'a1 * 'a2 * 'a3) name
  | T4 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'p, 'a1 * 'a2 * 'a3 * 'a4) name
  | T5 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'p,
          'a1 * 'a2 * 'a3 * 'a4 * 'a5) name
  | T6 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'p,
          'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6) name
  | Option : ('a -> 'p, 'a option) name
  | Result : ('a -> 'b -> 'p, ('a, 'b) result) name
  | List : ('a -> 'p, 'a list) name

val unit : ('p, unit) t
val t2 : ('a1 -> 'a2 -> 'p, 'a1 * 'a2) t
val t3 : ('a1 -> 'a2 -> 'a3 -> 'p, 'a1 * 'a2 * 'a3) t
val t4 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'p, 'a1 * 'a2 * 'a3 * 'a4) t
val t5 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'p, 'a1 * 'a2 * 'a3 * 'a4 * 'a5) t
val t6 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'p,
          'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6) t

val option : ('a -> 'p, 'a option) t
val result : ('a -> 'b -> 'p, ('a, 'b) result) t
val list : ('a -> 'p, 'a list) t
