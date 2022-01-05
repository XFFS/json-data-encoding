(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, Inc. <contact@nomadic-labs.com>          *)
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

let e =
  let open Json_encoding in
  seq unit

let witness = ref 0

let v () =
  incr witness ;
  Seq.Cons
    ( (),
      fun () ->
        incr witness ;
        Seq.Cons
          ( (),
            fun () ->
              incr witness ;
              Seq.Cons
                ( (),
                  fun () ->
                    incr witness ;
                    Seq.Cons
                      ( (),
                        fun () ->
                          incr witness ;
                          Seq.Nil ) ) ) )

let () =
  Printf.printf "Testing lazily seq streaming\n%!" ;
  let lexeme_seq = Json_encoding.construct_seq e v in
  let rec consume last s =
    match s () with
    | Seq.Cons (_, s) ->
        (* checking that the delta is never more than one: it checks that we
           traverse [v] one element at a time. *)
        assert (!witness - last <= 1) ;
        consume !witness s
    | Seq.Nil -> assert (!witness = last)
  in
  assert (!witness = 0) ;
  consume 0 lexeme_seq ;
  assert (!witness = 5) ;
  Printf.printf "Success for lazily seq streaming\n%!"
