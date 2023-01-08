(* Copyright (C) 2023  Petter A. Urkedal <paurkedal@gmail.com>
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
module String_map = Map.Make (String)

type t = [
  | `Null
  | `Bool of bool
  | `Int of int
  | `Float of float
  | `String of string
  | `Assoc of (string * t) list
  | `List of t list
]

module Make_needle (Value : sig type t end) = struct
  type Type.haystack += V of Value.t
  let get = function V x -> x | _ -> failwith __FUNCTION__
end

(* encode *)

module E_encode = Escher.Environment.Make (struct
  type nonrec 'a t = 'a -> t
end)

module Json_needle = Make_needle (struct type nonrec t = t end)

let encode_atom : type a. a Type.atom -> a -> t =
  function
   | Int -> fun x -> `Int x
   | Char -> fun x -> `String (String.make 1 x)
   | Bytes -> fun x -> `String (Bytes.to_string x)
   | String -> fun x -> `String x
   | Float -> fun x -> `Float x
   | Bool -> fun x -> `Bool x
   | Int32 -> fun x -> `Float (Int32.to_float x)
   | Int64 -> fun x -> `Float (Int64.to_float x)
   | Nativeint -> fun x -> `Float (Nativeint.to_float x)

let rec encode : type p a. (p, a) Type.t -> p E_encode.t -> a -> t =
  function
   | Param param ->
      E_encode.find (fun f x -> Lazy.force f x) param
   | App (t, u) ->
      let encode_t, encode_u = encode t, encode u in
      fun env -> encode_t (encode_u env :: env)
   | Iso (_, g, t) ->
      fun env ->
        let enc = encode t env in
        fun x -> enc (g x)
   | Atom t ->
      let encode_t = encode_atom t in
      fun _ -> encode_t
   | Nominal (Type.Option, _) ->
      fun env ->
        let enc = encode (Param Z) env in
        (function None -> `Null | Some x -> enc x)
   | Nominal (Type.List, _) ->
      fun env ->
        let enc_elt = encode (Param Z) env in
        fun xs -> `List (List.map enc_elt xs)
   | Nominal (Type.Assoc, _) ->
      fun env ->
        let enc_value = encode (Param Z) env in
        let enc_elt (k, v) = (k, enc_value v) in
        fun kvs -> `Assoc (List.map enc_elt kvs)
   | Nominal (_, t) ->
      encode_descr t

and encode_descr : type p a. (p, a) Type.descr -> p E_encode.t -> a -> t =
  function
   | Fix t -> E_encode.fix (encode_descr t)
   | Tuple (_, prod) ->
      fun env ->
        let enc = encode_tuple prod env in
        fun x -> `List (enc x [])
   | Record (_, prod) ->
      fun env ->
        let enc = encode_record prod env in
        fun x -> `Assoc (enc x [])
   | Variant (elim, sum) ->
      fun env ->
        let enc = encode_variant sum elim env in
        fun x -> Json_needle.get (enc x)

and encode_tuple
  : type p a i. (_, p, a, i) Type.product -> p E_encode.t -> a -> _ -> _ =
  function
   | [] -> fun _ _ acc -> acc
   | ((), t, project) :: rest ->
      fun env ->
        let enc_t = encode t env in
        let enc_rest = encode_tuple rest env in
        fun x acc -> enc_rest x (enc_t (project x) :: acc)

and encode_record
  : type p a i. (_, p, a, i) Type.product -> p E_encode.t -> a -> _ -> _ =
  function
   | [] -> fun _ _ acc -> acc
   | (tag, t, project) :: rest ->
      fun env ->
        let label = Type.label tag in
        let enc_t = encode t env in
        let enc_rest = encode_record rest env in
        fun x acc -> enc_rest x ((label, enc_t (project x)) :: acc)

and encode_variant
  : type p a e.
    (_, p, a, e) Type.sum -> e -> p E_encode.t -> a -> Type.haystack =
  function
   | [] -> fun elim _ -> elim
   | (tag, t, _) :: rest ->
      fun elim env ->
        let label = Type.label tag in
        let enc_t = encode t env in
        encode_variant rest
          (elim (fun x -> Json_needle.V (`List [`String label; enc_t x]))) env

(* decode *)

type decode_error = {
  msg: string;
  (* TODO: path: [`Index of int | `Label of string] list; *)
}
exception Decode_error of decode_error

let pp_decode_error = Fmt.using (fun {msg} -> msg) Fmt.string

let fail_decode msg = raise (Decode_error {msg})

module E_decode = Escher.Environment.Make (struct
  type nonrec 'a t = t -> 'a
end)

(* TODO: Check overflow for int, int32, int64, and nativeint. *)

let decode_int = function
 | `Int x -> x
 | `Float x when fst (modf x) = 0.0 -> int_of_float x
 | _ -> fail_decode "Expected a number convertible to int."

let decode_char = function
 | `String x when String.length x = 1 -> x.[0]
 | _ -> fail_decode "Expected a single byte string."

let decode_bytes = function
 | `String x -> Bytes.of_string x
 | _ -> fail_decode "Expected a string."

let decode_string = function
 | `String x -> x
 | _ -> fail_decode "Expected a string."

let decode_float = function
 | `Int x -> float_of_int x
 | `Float x -> x
 | _ -> fail_decode "Expected a number."

let decode_bool = function
 | `Bool x -> x
 | _ -> fail_decode "Expected a boolean."

let decode_int32 = function
 | `Int x -> Int32.of_int x
 | `Float x when fst (modf x) = 0.0 -> Int32.of_float x
 | _ -> fail_decode "Expected a number convertible to int32"

let decode_int64 = function
 | `Int x -> Int64.of_int x
 | `Float x when fst (modf x) = 0.0 -> Int64.of_float x
 | _ -> fail_decode "Expected a number convertible to int64"

let decode_nativeint = function
 | `Int x -> Nativeint.of_int x
 | `Float x when fst (modf x) = 0.0 -> Nativeint.of_float x
 | _ -> fail_decode "Expected a number convertible to nativeint"

let decode_option f = function
 | `Null -> None
 | x -> Some (f x)

let decode_list f = function
 | `List xs -> List.map f xs
 | _ -> fail_decode "Expected a list."

let decode_assoc f = function
 | `Assoc kvs -> List.map (fun (k, v) -> (k, f v)) kvs
 | _ -> fail_decode "Expected an object."

let decode_atom : type a. a Type.atom -> t -> a = function
 | Int -> decode_int
 | Char -> decode_char
 | Bytes -> decode_bytes
 | String -> decode_string
 | Float -> decode_float
 | Bool -> decode_bool
 | Int32 -> decode_int32
 | Int64 -> decode_int64
 | Nativeint -> decode_nativeint

module C_decode = struct
  type t = {
    ignore_excessive_keys: bool;
  }
  let create ~ignore_excessive_keys () = {ignore_excessive_keys}
  let default = {ignore_excessive_keys = true}
end

let rec decode
  : type p a.
    ?config: C_decode.t -> (p, a) Type.t -> p E_decode.t -> t -> a =
  fun ?(config = C_decode.default) ->
  function
   | Param param -> E_decode.find (fun f x -> Lazy.force f x) param
   | App (t, u) ->
      let decode_t, decode_u = decode ~config t, decode ~config u in
      fun env -> decode_t (decode_u env :: env)
   | Iso (f, _, t) ->
      fun env ->
        let dec = decode ~config t env in
        fun x -> f (dec x)
   | Atom t ->
      let dec = decode_atom t in
      fun _ -> dec
   | Nominal (Type.Option, _) ->
      fun env ->
        let dec = decode ~config (Param Z) env in
        decode_option dec
   | Nominal (Type.List, _) ->
      fun env ->
        let dec = decode ~config (Param Z) env in
        decode_list dec
   | Nominal (Type.Assoc, _) ->
      fun env ->
        let dec_value = decode ~config (Param Z) env in
        decode_assoc dec_value
   | Nominal (_, t) ->
      decode_descr ~config t

and decode_descr
  : type p a.
    config: C_decode.t -> (p, a) Type.descr -> p E_decode.t -> t -> a =
  fun ~config ->
  function
   | Fix t ->
      E_decode.fix (decode_descr ~config t)
   | Tuple (intro, prod) ->
      fun env ->
        let dec = decode_tuple ~config prod env in
        (function
         | `List xs -> dec xs intro
         | _ -> fail_decode "Cannot decode tuple.")
   | Record (intro, prod) ->
      fun env ->
        let dec = decode_record ~config prod env in
        (function
         | `Assoc kvs ->
            let kvs =
              List.fold_left
                (fun acc (k, v) -> String_map.add k v acc)
                String_map.empty kvs
            in
            dec kvs intro
         | _ -> fail_decode "Cannot decode record.")
   | Variant (_, sum) ->
      fun env ->
        let dec = decode_variant ~config sum env in
        (function
         | `List [`String label; value] ->
            dec label value
         | _ -> fail_decode "Cannot decode variant.")

and decode_tuple
  : type p a i.
    config: C_decode.t ->
    (_, p, a, i) Type.product -> p E_decode.t -> t list -> i -> a =
  fun ~config ->
  function
   | [] ->
      fun _ xs intro ->
        if xs <> [] then fail_decode "Too many elements for tuple.";
        intro
   | ((), t, _) :: rest ->
      fun env ->
        let dec_t = decode ~config t env in
        let dec_rest = decode_tuple ~config rest env in
        (function
         | [] -> fun _ -> fail_decode "Too few elements for tuple."
         | x :: xs -> fun intro -> dec_rest xs (intro (dec_t x)))

and decode_record
  : type p a i.
    config: C_decode.t ->
    (_, p, a, i) Type.product -> p E_decode.t -> t String_map.t -> i -> a =
  fun ~config ->
  function
   | [] ->
      fun _ kvs intro ->
        if not (config.ignore_excessive_keys || String_map.is_empty kvs) then
          fail_decode "Excessive keys for record.";
        intro
   | (tag, t, _) :: rest ->
      fun env ->
        let label = Type.label tag in
        let dec_t = decode ~config t env in
        let dec_rest = decode_record ~config rest env in
        fun kvs intro ->
          let x = try String_map.find label kvs with Not_found -> `Null in
          dec_rest kvs (intro (dec_t x))

and decode_variant
  : type p a e.
    config: C_decode.t ->
    (_, p, a, e) Type.sum -> p E_decode.t -> string -> t -> a =
  fun ~config ->
  function
   | [] ->
      fun _ l _ -> fail_decode ("Failed to decode variant case " ^ l ^ ".")
   | (tag, t, inject) :: rest ->
      fun env ->
        let label = Type.label tag in
        let dec_t = decode ~config t env in
        let dec_rest = decode_variant ~config rest env in
        fun l v ->
          if label = l then inject (dec_t v) else
          dec_rest l v
