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

(** Environment providing callbacks for type parameters. *)

open Escher_v1

(** Abstract environment interface. *)
module type A = sig

  type 'a elt

  type _ t =
    | [] : unit t
    | (::) : 'a elt * 'p t -> ('a -> 'p) t
    | Private_cons_lazy : 'a elt Lazy.t * 'p t -> ('a -> 'p) t
        [@alert private_ "This constructor is private."]
        (**)
  (** [[p1; ...; pN]] is an environment which binds [N] type parameter slots to
      the values [p1], ..., [pN].

      The additional {!Private_cons_lazy} constructor is only to be used
      internally to implement the fixed-point combinator. *)

end

(** Full environment interface for algorithm implementation. *)
module type S = sig
  include A

  (** {2 Utility functions for constructing algorithms} *)

  val find :
    ('a elt Lazy.t -> 'a elt) ->
    ('p, 'a) Type.param -> 'p t -> 'a elt
  (** [find forcer param env] returns the binding of [param] in [env] with the
      help of [forcer] which, utilizing the structure of the value, must
      postpone forcing the argument until the surrounding {!fix}, which created
      the binding, returns.  Typically, this is done by commuting {!Lazy.force}
      into an η-expansion of the value, e.g. [fun f x y -> Lazy.force f x y]. *)

  val fix : (('a -> 'p) t -> 'a elt) -> 'p t -> 'a elt
  (** Informally, [fix f env] is the [z] which solves the equation
      [z = f (z :: env)].  In practice {!Private_cons_lazy} is used internally
      to break the recursion.  The laziness is forced by the first argument of
      {!find} after [fix f env] returns. *)

end

module Make : functor (Elt : sig type 'a t end) -> S with type 'a elt = 'a Elt.t
