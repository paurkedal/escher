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

type ('p, 'a) annot = ..
type field_annot = ..
type field_tag = (string, field_annot) tag
type constructor_annot = ..
type constructor_tag = (string, constructor_annot) tag

let rec label : (string, _) tag -> string = function
 | Label l -> l
 | Annot (tag, _) -> label tag

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
  | Fix : ('a -> 'p, 'a) t -> ('p, 'a) t
  | Atom : 'a atom -> ('p, 'a) t
  | Tuple : 'i * (unit, 'p, 'a, 'i) product -> ('p, 'a) t
  | Record : 'i * (field_tag, 'p, 'a, 'i) product -> ('p, 'a) t
  | Variant : 'e * (constructor_tag, 'p, 'a, 'e) sum -> ('p, 'a) t
  | Annot : ('p, 'a) annot * ('p, 'a) t -> ('p, 'a) t

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

let int = Atom Int
let char = Atom Char
let bytes = Atom Bytes
let string = Atom String
let float = Atom Float
let bool = Atom Bool
let int32 = Atom Int32
let int64 = Atom Int64
let nativeint = Atom Nativeint

type (_, _) annot +=
  | Is_unit : ('p, unit) annot
  | Is_t2 : ('a1 -> 'a2 -> 'p, 'a1 * 'a2) annot
  | Is_t3 : ('a1 -> 'a2 -> 'a3 -> 'p, 'a1 * 'a2 * 'a3) annot
  | Is_t4 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'p, 'a1 * 'a2 * 'a3 * 'a4) annot
  | Is_t5 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'p,
             'a1 * 'a2 * 'a3 * 'a4 * 'a5) annot
  | Is_t6 : ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'p,
             'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6) annot
  | Is_option : ('a -> 'p, 'a option) annot
  | Is_result : ('a -> 'b -> 'p, ('a, 'b) result) annot
  | Is_list : ('a -> 'p, 'a list) annot

let unit = Annot (Is_unit, Tuple ((), []))

let t2 =
  Annot (Is_t2, Tuple ((fun x1 x2 -> (x1, x2)), [
    (), Param Z, (fun (x, _) -> x);
    (), Param (S Z), (fun (_, x) -> x);
  ]))

let t3 =
  Annot (Is_t3, Tuple ((fun x1 x2 x3 -> (x1, x2, x3)), [
    (), Param Z, (fun (x, _, _) -> x);
    (), Param (S Z), (fun (_, x, _) -> x);
    (), Param (S (S Z)), (fun (_, _, x) -> x);
  ]))

let t4 =
  Annot (Is_t4, Tuple ((fun x1 x2 x3 x4 -> (x1, x2, x3, x4)), [
    (), Param Z, (fun (x, _, _, _) -> x);
    (), Param (S Z), (fun (_, x, _, _) -> x);
    (), Param (S (S Z)), (fun (_, _, x, _) -> x);
    (), Param (S (S (S Z))), (fun (_, _, _, x) -> x);
  ]))

let t5 =
  Annot (Is_t5, Tuple ((fun x1 x2 x3 x4 x5 -> (x1, x2, x3, x4, x5)), [
    (), Param Z, (fun (x, _, _, _, _) -> x);
    (), Param (S Z), (fun (_, x, _, _, _) -> x);
    (), Param (S (S Z)), (fun (_, _, x, _, _) -> x);
    (), Param (S (S (S Z))), (fun (_, _, _, x, _) -> x);
    (), Param (S (S (S (S Z)))), (fun (_, _, _, _, x) -> x);
  ]))

let t6 =
  Annot (Is_t6, Tuple ((fun x1 x2 x3 x4 x5 x6 -> (x1, x2, x3, x4, x5, x6)), [
    (), Param Z, (fun (x, _, _, _, _, _) -> x);
    (), Param (S Z), (fun (_, x, _, _, _, _) -> x);
    (), Param (S (S Z)), (fun (_, _, x, _, _, _) -> x);
    (), Param (S (S (S Z))), (fun (_, _, _, x, _, _) -> x);
    (), Param (S (S (S (S Z)))), (fun (_, _, _, _, x, _) -> x);
    (), Param (S (S (S (S (S Z))))), (fun (_, _, _, _, _, x) -> x);
  ]))

let option =
  let elim none some = function None -> none () | Some x -> some x in
  Annot (Is_option, Variant (elim, [
    Label "None", unit, (fun () -> None);
    Label "Some", Param Z, (fun x -> Some x);
  ]))

let result =
  let elim ok error = function Ok x -> ok x | Error x -> error x in
  Annot (Is_result, Variant (elim, [
    Label "Ok", Param Z, (fun x -> Ok x);
    Label "Error", Param (S Z), (fun x -> Error x);
  ]))

let list =
  let elim nil cons = function [] -> nil () | x :: xs -> cons (x, xs) in
  Annot (Is_list, Fix (Variant (elim, [
    Label "[]", unit, (fun () -> []);
    Label "(::)", App (App (t2, Param (S (S Z))), Param Z),
      (fun (x, xs) -> x :: xs);
  ])))