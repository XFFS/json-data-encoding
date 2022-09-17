(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, Inc. <contact@nomadic-labs.com>          *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

let large_lists_5kish = List.init 20 (fun i -> List.init (5000 + i) Fun.id)

let large_lists =
  List.init 10 (fun i ->
      let len = (i + 1) * 1000 in
      List.init len Fun.id)

let genf =
  let open Crowbar in
  choose
    [
      const Fun.id;
      const succ;
      const pred;
      const (Fun.const 0);
      const (Stdlib.max 1024);
      const (Stdlib.min 1024);
    ]

let genl =
  let open Crowbar in
  let large_list_gen =
    List.mapi
      (fun i l ->
        with_printer
          (fun fmt _ -> Format.fprintf fmt "large_list_%d" (5003 + i))
          (const l))
      large_lists_5kish
    @ List.mapi
        (fun i l ->
          with_printer
            (fun fmt _ -> Format.fprintf fmt "large_list_%dk" (i + 1))
            (const l))
        large_lists
  in
  choose (list int :: large_list_gen)

let test f l = Crowbar.check_eq (Stdlib.List.map f l) (List_map.map_pure f l)

let () = Crowbar.add_test ~name:"tail-rec list map" [genf; genl] test

let genfi =
  let open Crowbar in
  choose
    [
      const ( + );
      const ( * );
      const Stdlib.max;
      const Stdlib.min;
      const (fun _ _ -> 0);
      const (fun i v -> Stdlib.max i (Stdlib.min 1024 v));
    ]

let testi f l = Crowbar.check_eq (Stdlib.List.mapi f l) (List_map.mapi_pure f l)

let () = Crowbar.add_test ~name:"tail-rec list mapi" [genfi; genl] testi

let testa l1 l2 =
  Crowbar.check_eq (Stdlib.List.append l1 l2) (List_map.append l1 l2)

let () = Crowbar.add_test ~name:"tail-rec list append" [genl; genl] testa
