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

(** Generic algorithms.

    To each algorithm, a congruent environment module is provided for resolving
    type parameters or providing callbacks.  Since OCaml can locate constructors
    by type, the list constructors used to form the environment can be passed
    unqualified.  E.g. the following three definitions are valid and gives the
    same algoritms:
    {[
      open Type
      let eq1 = Escher.Generic.equal result [Int.equal; String.equal]
      let eq2 = Escher.Generic.equal (App (result, int)) [String.equal]
      let eq3 = Escher.Generic.equal (App (App (result, int), string)) []
    ]} *)

open Escher_v1

(** {2 equal} *)

module E_equal : Environment.A with type 'a elt := 'a -> 'a -> bool

val equal : ('p, 'a) Type.t -> 'p E_equal.t -> 'a -> 'a -> bool
(** [equal t env] infers an equality predicate for [t] using [env] to resolve
    equality predicates for type parameters. *)

(** {2 compare} *)

module E_compare : Environment.A with type 'a elt := 'a -> 'a -> int

val compare : ('p, 'a) Type.t -> 'p E_compare.t -> 'a -> 'a -> int
(** [compare t env] infers a comparison function for [t] using [env] to resolve
    comparison functions for type parameters. *)

(** {2 iter} *)

module E_iter : Environment.A with type 'a elt := 'a -> unit

val iter : ('p, 'a) Type.t -> 'p E_iter.t -> 'a -> unit
(** [iter t env] infers a function which, given an instance of [t], calls the
    corresponding function of [env] for each occurrence of a value of a type
    parameter in ['p].  The iteration is carried out in order of occurence as
    prescribed by [t], which might differ from the OCaml type.  If [t] has no
    type parameters, this function has no effect. *)

(** {2 fold} *)

module E_fold : Fold_environment.A with type ('a, 'b) elt := 'a -> 'b -> 'b

val fold : ('p, 'a) Type.t -> ('p, 'b) E_fold.t -> 'a -> 'b -> 'b
(** [fold t env] infers a function which, given a value of [t], carries out an
    accumulating iteration over occurences of values belonging to a type
    parameter in ['p].  This is not the catamorphism (aka generalized fold), but
    a simple composition of [f x] for each occurrence [x] and with [f] the
    appropriate element of [env]. *)

(** {2 pp} *)

module E_pp : Environment.A with type 'a elt := 'a Fmt.t

val pp : ('p, 'a) Type.t -> 'p E_pp.t -> 'a Fmt.t
(** [pp t env] infers a pretty-printer for [t] using [env] to resolve pretty
    printers for type parameters. *)
