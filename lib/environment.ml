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

module type A = sig

  type 'a elt

  type _ t =
    | [] : unit t
    | (::) : 'a elt * 'p t -> ('a -> 'p) t
    | Private_cons_lazy : 'a elt Lazy.t * 'p t -> ('a -> 'p) t
        [@alert private_ "This constructor is private."]

end

module type S = sig
  include A

  val find :
    ('a elt Lazy.t -> 'a elt) ->
    ('p, 'a) Type.param -> 'p t -> 'a elt

  val fix : (('a -> 'p) t -> 'a elt) -> 'p t -> 'a elt

end

module Make (Elt : sig type 'a t end) = struct

  type 'a elt = 'a Elt.t

  type 'p t =
    | [] : unit t
    | (::) : 'a Elt.t * 'p t -> ('a -> 'p) t
    | Private_cons_lazy : 'a Elt.t Lazy.t * 'p t -> ('a -> 'p) t

  let rec find
    : type a p.
      (a elt Lazy.t -> a elt) -> (p, a) Type.param -> p t -> a elt =
    fun force p env ->
    (match p, env with
     | Z, f :: _ -> f
     | Z, Private_cons_lazy (f, _) -> force f
     | S p, (_ :: env | Private_cons_lazy (_, env)) -> find force p env
     | _, [] -> .)

  let fix f =
    fun env ->
      let rec g = lazy (f (Private_cons_lazy (g, env))) in
      Lazy.force g

end
