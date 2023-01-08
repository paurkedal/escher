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

type t = [
  | `Null
  | `Bool of bool
  | `Int of int
  | `Float of float
  | `String of string
  | `Assoc of (string * t) list
  | `List of t list
]

(** {2 Encoding} *)

module E_encode : Escher.Environment.S with type 'a elt := 'a -> t

val encode : ('p, 'a) Type.t -> 'p E_encode.t -> 'a -> t

(** {2 Decoding} *)

type decode_error

val pp_decode_error : decode_error Fmt.t

exception Decode_error of decode_error

module C_decode : sig
  type t
  val create : ignore_excessive_keys: bool -> unit -> t
end

module E_decode : Escher.Environment.S with type 'a elt := t -> 'a

val decode : ?config: C_decode.t -> ('p, 'a) Type.t -> 'p E_decode.t -> t -> 'a
