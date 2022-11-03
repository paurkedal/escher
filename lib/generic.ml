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

open Escher_v1

let (%) f g x = f (g x)

module Index = struct

  type (_, _) t =
    | Z : ('c, 'c) t
    | S : ('c, 'd) t -> ('a -> 'c, 'd) t

  let rec succ : type a c d. (c, a -> d) t -> (c, d) t = function
   | Z -> S Z
   | S i -> S (succ i)

end

module Make_needle (Value : sig type t end) = struct
  type Type.haystack += V of Value.t
  let get = function V x -> x | _ -> failwith __FUNCTION__
end
module Unit_needle = Make_needle (struct type t = unit end)
module Int_needle = Make_needle (struct type t = int end)

module Sum = struct

  let delta
    : type a b p d g e.
      lt: g -> eq: (b -> g) -> gt: g ->
      (e, (b -> Type.haystack) -> d) Index.t ->
      (_, p, a, e) Type.sum -> e -> a -> g =
    fun ~lt ~eq ~gt ->
    let module Needle = Make_needle (struct type t = g end) in
    let rec loop_after
      : type e. (_, p, a, e) Type.sum -> e -> a -> Type.haystack =
      (function
       | [] -> Fun.id
       | (_, _, _) :: rest ->
          let f = loop_after rest in
          fun elim -> f (elim (fun _ -> Needle.V gt)))
    in
    let rec loop
      : type e.
        (e, (b -> Type.haystack) -> d) Index.t -> (_, p, a, e) Type.sum ->
        e -> a -> Type.haystack =
      fun index sum ->
      (match sum, index with
       | [], _ -> Fun.id
       | (_, _, _) :: rest, Index.Z ->
          let f = loop_after rest in
          fun elim -> f (elim (fun y -> Needle.V (eq y)))
       | (_, _, _) :: rest, Index.S index ->
          let f = loop index rest in
          fun elim -> f (elim (fun _ -> Needle.V lt)))
    in
    fun index sum ->
      let f = loop index sum in
      fun elim ->
        let g = f elim in
        fun x -> Needle.get (g x)

  type ('p, 'env, 'g) diagonal_fun = {
    call: 'a. ('p, 'a) Type.t -> 'env -> 'a -> 'a -> 'g;
  }

  let diagonal
    : type a p d g.
      (_, p, a, d) Type.sum -> d ->
      lt: g -> eq: (p, 'env, g) diagonal_fun -> gt: g ->
      'env -> a -> a -> g =
    fun sum0 elim0 ~lt ~eq ~gt ->
    let module Needle = Make_needle (struct type t = a -> g end) in
    let rec loop
      : type e. (d, e) Index.t -> (_, p, a, e) Type.sum ->
        'env -> e -> a -> a -> g =
      fun param -> function
       | [] -> fun _ elim x1 -> Needle.get (elim x1)
       | (_, t, _) :: rest ->
          let equal_case = eq.call t in
          let equal_rest = loop (Index.succ param) rest in
          fun env ->
            let equal_case = equal_case env in
            let equal_rest = equal_rest env in
            let inner =
              delta ~lt:(fun _ -> lt) ~eq:equal_case ~gt:(fun _ -> gt)
                param sum0 elim0
            in
            fun elim ->
              equal_rest (elim (fun y1 -> Needle.V (fun x2 -> inner x2 y1)))
    in
    let f = loop Index.Z sum0 in
    fun env -> f env elim0

end

(* equal *)

let equal_atom : type a. a Type.atom -> a -> a -> bool = function
 | Int -> Int.equal
 | Char -> Char.equal
 | Bytes -> Bytes.equal
 | String -> String.equal
 | Float -> Float.equal
 | Bool -> Bool.equal
 | Int32 -> Int32.equal
 | Int64 -> Int64.equal
 | Nativeint -> Nativeint.equal

module E_equal = Environment.Make (struct type 'a t = 'a -> 'a -> bool end)

let rec equal : type p a. (p, a) Type.t -> p E_equal.t -> a -> a -> bool =
  function
   | Param param -> E_equal.find (fun f x y -> Lazy.force f x y) param
   | App (t, u) ->
      let equal_t, equal_u = equal t, equal u in
      fun env -> equal_t (equal_u env :: env)
   | Fix t -> E_equal.fix (equal t)
   | Atom t -> fun _ -> equal_atom t
   | Tuple (_, prod) -> equal_product prod
   | Record (_, prod) -> equal_product prod
   | Variant (elim, sum) -> equal_sum sum elim
   | Annot (_, t) -> equal t

and equal_product
  : type tag a p i.
    (tag, p, a, i) Type.product -> p E_equal.t -> a -> a -> bool =
  function
   | [] -> fun _ _ _ -> true
   | (_, t, project) :: rest ->
      fun env ->
        let equal_proj = equal t env in
        let equal_rest = equal_product rest env in
        fun x y ->
          equal_proj (project x) (project y) && equal_rest x y

and equal_sum
  : type a p d. (_, p, a, d) Type.sum -> d -> p E_equal.t -> a -> a -> bool =
  fun sum elim -> Sum.diagonal sum ~lt:false ~eq:{call = equal} ~gt:false elim

(* compare *)

let compare_atom : type a. a Type.atom -> a -> a -> int = function
 | Int -> Int.compare
 | Char -> Char.compare
 | Bytes -> Bytes.compare
 | String -> String.compare
 | Float -> Float.compare
 | Bool -> Bool.compare
 | Int32 -> Int32.compare
 | Int64 -> Int64.compare
 | Nativeint -> Nativeint.compare

module E_compare = Environment.Make (struct type 'a t = 'a -> 'a -> int end)

let rec compare
  : type a p. (p, a) Type.t -> p E_compare.t -> a -> a -> int =
  function
   | Param param -> E_compare.find (fun f x y -> Lazy.force f x y) param
   | App (t, u) ->
      let compare_t, compare_u = compare t, compare u in
      fun env -> compare_t (compare_u env :: env)
   | Fix t -> E_compare.fix (compare t)
   | Atom t -> fun _ -> compare_atom t
   | Tuple (_, prod) -> compare_product prod
   | Record (_, prod) -> compare_product prod
   | Variant (elim, sum) -> compare_sum sum elim
   | Annot (_, t) -> compare t

and compare_product
  : type tag a p i.
    (tag, p, a, i) Type.product -> p E_compare.t -> a -> a -> int =
  function
   | [] -> fun _ _ _ -> 0
   | (_, t, project) :: rest ->
      let compare_proj = compare t in
      let compare_rest = compare_product rest in
      fun env ->
        let compare_proj = compare_proj env in
        let compare_rest = compare_rest env in
        fun x y ->
          let ord = compare_proj (project x) (project y) in
          if ord <> 0 then ord else compare_rest x y

and compare_sum
  : type a p e. (_, p, a, e) Type.sum -> e -> p E_compare.t -> a -> a -> int =
  fun sum elim -> Sum.diagonal sum ~lt:1 ~eq:{call = compare} ~gt:(-1) elim

(* iter *)

module E_iter = Environment.Make (struct type 'a t = 'a -> unit end)

let rec iter
  : type a p. (p, a) Type.t -> p E_iter.t -> a -> unit =
  function
   | Param param -> E_iter.find (fun f x -> Lazy.force f x) param
   | App (t, u) ->
      let iter_t, iter_u = iter t, iter u in
      fun env -> iter_t (iter_u env :: env)
   | Fix t -> E_iter.fix (iter t)
   | Atom _ -> fun _ _ -> ()
   | Tuple (_, prod) -> iter_product prod
   | Record (_, prod) -> iter_product prod
   | Variant (elim, sum) ->
      let f = iter_sum sum elim in
      fun env ->
        let f = f env in
        fun x -> Unit_needle.get (f x)
   | Annot (_, t) -> iter t

and iter_product
  : type tag a p i. (tag, p, a, i) Type.product -> p E_iter.t -> a -> unit =
  function
   | [] -> fun _ _ -> ()
   | (_, t, project) :: rest ->
      let iter_proj = iter t in
      let iter_rest = iter_product rest in
      fun env ->
        let iter_proj = iter_proj env in
        let iter_rest = iter_rest env in
        fun x -> iter_proj (project x); iter_rest x

and iter_sum
  : type a p e. (_, p, a, e) Type.sum -> e -> p E_iter.t -> a -> Type.haystack =
  function
   | [] -> fun elim _ -> elim
   | (_, t, _) :: rest ->
      let iter_case = iter t in
      let iter_rest = iter_sum rest in
      fun elim env ->
        let iter_case = iter_case env in
        iter_rest (elim (fun x -> Unit_needle.V (iter_case x))) env

(* fold *)

module E_fold =
  Fold_environment.Make (struct type ('a, 'b) t = 'a -> 'b -> 'b end)

let rec fold
  : type a b p. (p, a) Type.t -> (p, b) E_fold.t -> a -> b -> b =
  function
   | Param param -> E_fold.find (fun f x -> Lazy.force f x) param
   | App (t, u) ->
      let fold_t, fold_u = fold t, fold u in
      fun env -> fold_t (fold_u env :: env)
   | Fix t -> E_fold.fix (fold t)
   | Atom _ -> fun _ _ -> Fun.id
   | Tuple (_, prod) -> fold_product prod
   | Record (_, prod) -> fold_product prod
   | Variant (elim, sum) -> fold_sum sum elim
   | Annot (_, t) -> fold t

and fold_product
  : type tag a b p i.
    (tag, p, a, i) Type.product -> (p, b) E_fold.t -> a -> b -> b =
  function
   | [] -> fun _ _ -> Fun.id
   | (_, t, project) :: rest ->
      let fold_proj = fold t in
      let fold_rest = fold_product rest in
      fun env ->
        let fold_proj = fold_proj env in
        let fold_rest = fold_rest env in
        fun x acc -> acc |> fold_proj (project x) |> fold_rest x

and fold_sum
  : type a b p e.
    (_, p, a, e) Type.sum -> e -> (p, b) E_fold.t -> a -> b -> b =
  fun sum elim ->
  let module Needle = Make_needle (struct type t = b -> b end) in
  let rec loop
    : type e.
      (_, p, a, e) Type.sum -> e -> (p, b) E_fold.t -> a -> Type.haystack =
    function
     | [] -> fun elim _ -> elim
     | (_, t, _) :: rest ->
        let fold_case = fold t in
        let fold_rest = loop rest in
        fun elim env ->
          let fold_case = fold_case env in
          fold_rest (elim (fun x -> Needle.V (fold_case x))) env
  in
  let f = loop sum elim in
  fun env ->
    let f = f env in
    fun x -> Needle.get (f x)

(* pp *)

let pp_atom : type a. a Type.atom -> a Fmt.t = function
 | Int -> Fmt.int
 | Char -> Fmt.(quote ~mark:"'" char)
 | Bytes -> Fmt.(quote (using (Bytes.to_string % Bytes.escaped) string))
 | String -> Fmt.(quote (using String.escaped string))
 | Float -> Fmt.float
 | Bool -> Fmt.bool
 | Int32 -> Fmt.int32
 | Int64 -> Fmt.int64
 | Nativeint -> Fmt.nativeint

module E_pp = Environment.Make (struct type 'a t = 'a Fmt.t end)
module Pp_needle = Make_needle (struct type t = Format.formatter -> unit end)

(* FIXME: list *)
let rec pp : type a p. (p, a) Type.t -> p E_pp.t -> a Fmt.t =
  function
   | Param param ->
      E_pp.find (fun f ppf x -> Lazy.force f ppf x) param
   | App (t, u) ->
      let pp_u = pp u in
      let pp_t = pp t in
      fun env -> pp_t (pp_u env :: env)
   | Fix t -> E_pp.fix (pp t)
   | Atom t -> fun _ -> pp_atom t
   | Tuple (_, prod) ->
      let rec loop
        : type i. (_, _, a, i) Type.product -> p E_pp.t -> a Fmt.t =
      function
       | [] ->
          fun _ -> Fmt.nop
       | [(), t, project] ->
          let pp_proj = pp t in
          fun env -> Fmt.(using project (pp_proj env))
       | ((), t, project) :: rest ->
          let pp_proj = pp t in
          let pp_rest = loop rest in
          fun env -> Fmt.(using project (pp_proj env) ++ comma ++ pp_rest env)
      in
      let pp_prod = loop prod in
      fun env ->
        Fmt.(parens (pp_prod env))
   | Record (_, prod) ->
      let rec loop
        : type i. (_, _, a, i) Type.product -> p E_pp.t -> a Fmt.t =
      function
       | [] -> fun _ -> Fmt.nop
       | (tag, t, project) :: rest ->
          let label = Type.label tag in
          let pp_proj = pp t in
          let pp_rest = loop rest in
          fun env ->
            Fmt.(const string label ++ sp ++ const string "=" ++ sp
                 ++ using project (pp_proj env) ++ semi ++ pp_rest env)
      in
      let pp_prod = loop prod in
      fun env ->
        Fmt.(braces (pp_prod env))
   | Variant (elim, sum) ->
      let rec loop
        : type e. (_, _, a, e) Type.sum -> e -> p E_pp.t -> a -> Type.haystack =
        function
         | [] ->
            fun elim _ -> elim
         | (tag, Tuple (_, []), _) :: rest ->
            let pp_label = Fun.flip Fmt.(const string (Type.label tag)) in
            let pp_case y = Pp_needle.V (pp_label y) in
            let pp_rest = loop rest in
            fun elim env -> pp_rest (elim pp_case) env
         | (tag, t, _) :: rest ->
            let label = Type.label tag in
            let pp_t = pp t in
            let pp_rest = loop rest in
            fun elim env ->
              let pp_t = pp_t env in
              let pp_case =
                Fun.flip Fmt.(box ~indent:2 (const string label ++ sp ++ pp_t))
              in
              pp_rest (elim (fun y -> Pp_needle.V (pp_case y))) env
      in
      let pp_sum = loop sum elim in
      fun env ->
        let pp_sum = pp_sum env in
        fun ppf x -> Pp_needle.get (pp_sum x) ppf
   | Annot (Type.Is_list, _) ->
      fun env ->
        Fmt.(brackets (list ~sep:semi (pp (Param Z) env)))
   | Annot (_, t) -> pp t
