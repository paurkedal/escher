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

open Escher
open Escher_v1

let test_fold () =
  let f x acc = assert (x = acc); x + acc in
  let z = Generic.fold Type.list [f] [1; 2; 4] 1 in
  Alcotest.(check int) "same" 8 z

let pp_unit = Generic.pp Type.unit []
let show_unit = Fmt.to_to_string pp_unit
let pp_int_list = Generic.pp Type.(list $ int) []
let show_int_list = Fmt.to_to_string pp_int_list

let test_pp () =
  Alcotest.(check string) "same" "()" (show_unit ());
  Alcotest.(check string) "same" "[]" (show_int_list []);
  Alcotest.(check string) "same" "[1]" (show_int_list [1]);
  Alcotest.(check string) "same" "[1; 2]" (show_int_list [1; 2])

let test_cases = [
  Alcotest.test_case "fold" `Quick test_fold;
  Alcotest.test_case "pp" `Quick test_pp;
]
