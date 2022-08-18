(*

Copyright (c) 2020, Anton Bachin

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

*)

(* https://discuss.ocaml.org/t/a-new-list-map-that-is-both-stack-safe-and-fast/865 *)

(* Non-tail-recursive prefix for the new fast map implementation. This is not
   really necessary, as it is possible to use the tail-recursive suffix
   function, and get almost the same performance. However, there *is* a slight
   improvement, and this makes it fully competitive with Containers and
   Base :) *)
let rec plain_unrolled_prefix_5 map_suffix count f l =
  match l with
  | [] -> []
  | [x1] ->
      let y1 = f x1 in
      [y1]
  | [x1; x2] ->
      let y2 = f x2 in
      let y1 = f x1 in
      [y1; y2]
  | [x1; x2; x3] ->
      let y3 = f x3 in
      let y2 = f x2 in
      let y1 = f x1 in
      [y1; y2; y3]
  | [x1; x2; x3; x4] ->
      let y4 = f x4 in
      let y3 = f x3 in
      let y2 = f x2 in
      let y1 = f x1 in
      [y1; y2; y3; y4]
  | x1 :: x2 :: x3 :: x4 :: x5 :: tail ->
      let tail =
        if count <= 0 then map_suffix f tail
        else plain_unrolled_prefix_5 map_suffix (count - 1) f tail
      in
      let y5 = f x5 in
      let y4 = f x4 in
      let y3 = f x3 in
      let y2 = f x2 in
      let y1 = f x1 in
      y1 :: y2 :: y3 :: y4 :: y5 :: tail

let rec plain_unrolled_prefix_5i map_suffix count i f l =
  match l with
  | [] -> []
  | [x1] ->
      let y1 = f i x1 in
      [y1]
  | [x1; x2] ->
      let y2 = f (i + 1) x2 in
      let y1 = f i x1 in
      [y1; y2]
  | [x1; x2; x3] ->
      let y3 = f (i + 2) x3 in
      let y2 = f (i + 1) x2 in
      let y1 = f i x1 in
      [y1; y2; y3]
  | [x1; x2; x3; x4] ->
      let y4 = f (i + 3) x4 in
      let y3 = f (i + 2) x3 in
      let y2 = f (i + 1) x2 in
      let y1 = f i x1 in
      [y1; y2; y3; y4]
  | x1 :: x2 :: x3 :: x4 :: x5 :: tail ->
      let tail =
        if count <= 0 then map_suffix i f tail
        else plain_unrolled_prefix_5i map_suffix (count - 1) (i + 5) f tail
      in
      let y5 = f (i + 4) x5 in
      let y4 = f (i + 3) x4 in
      let y3 = f (i + 2) x3 in
      let y2 = f (i + 1) x2 in
      let y1 = f i x1 in
      y1 :: y2 :: y3 :: y4 :: y5 :: tail

(* Fast tail-recursive map for the list suffix. *)
let chunked_tail_recursive_map_12 f l =
  let rec split chunks l =
    match l with
    | _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: tail ->
        (split [@ocaml.tailcall]) (l :: chunks) tail
    | _ -> l :: chunks
  in

  let map_head_chunk chunk =
    match[@ocaml.warning "-8"] chunk with
    | [] -> []
    | [x1] ->
        let y1 = f x1 in
        [y1]
    | [x1; x2] ->
        let y2 = f x2 in
        let y1 = f x1 in
        [y1; y2]
    | [x1; x2; x3] ->
        let y3 = f x3 in
        let y2 = f x2 in
        let y1 = f x1 in
        [y1; y2; y3]
    | [x1; x2; x3; x4] ->
        let y4 = f x4 in
        let y3 = f x3 in
        let y2 = f x2 in
        let y1 = f x1 in
        [y1; y2; y3; y4]
    | [x1; x2; x3; x4; x5] ->
        let y5 = f x5 in
        let y4 = f x4 in
        let y3 = f x3 in
        let y2 = f x2 in
        let y1 = f x1 in
        [y1; y2; y3; y4; y5]
    | [x1; x2; x3; x4; x5; x6] ->
        let y6 = f x6 in
        let y5 = f x5 in
        let y4 = f x4 in
        let y3 = f x3 in
        let y2 = f x2 in
        let y1 = f x1 in
        [y1; y2; y3; y4; y5; y6]
    | [x1; x2; x3; x4; x5; x6; x7] ->
        let y7 = f x7 in
        let y6 = f x6 in
        let y5 = f x5 in
        let y4 = f x4 in
        let y3 = f x3 in
        let y2 = f x2 in
        let y1 = f x1 in
        [y1; y2; y3; y4; y5; y6; y7]
    | [x1; x2; x3; x4; x5; x6; x7; x8] ->
        let y8 = f x8 in
        let y7 = f x7 in
        let y6 = f x6 in
        let y5 = f x5 in
        let y4 = f x4 in
        let y3 = f x3 in
        let y2 = f x2 in
        let y1 = f x1 in
        [y1; y2; y3; y4; y5; y6; y7; y8]
    | [x1; x2; x3; x4; x5; x6; x7; x8; x9] ->
        let y9 = f x9 in
        let y8 = f x8 in
        let y7 = f x7 in
        let y6 = f x6 in
        let y5 = f x5 in
        let y4 = f x4 in
        let y3 = f x3 in
        let y2 = f x2 in
        let y1 = f x1 in
        [y1; y2; y3; y4; y5; y6; y7; y8; y9]
    | [x1; x2; x3; x4; x5; x6; x7; x8; x9; x10] ->
        let y10 = f x10 in
        let y9 = f x9 in
        let y8 = f x8 in
        let y7 = f x7 in
        let y6 = f x6 in
        let y5 = f x5 in
        let y4 = f x4 in
        let y3 = f x3 in
        let y2 = f x2 in
        let y1 = f x1 in
        [y1; y2; y3; y4; y5; y6; y7; y8; y9; y10]
    | [x1; x2; x3; x4; x5; x6; x7; x8; x9; x10; x11] ->
        let y11 = f x11 in
        let y10 = f x10 in
        let y9 = f x9 in
        let y8 = f x8 in
        let y7 = f x7 in
        let y6 = f x6 in
        let y5 = f x5 in
        let y4 = f x4 in
        let y3 = f x3 in
        let y2 = f x2 in
        let y1 = f x1 in
        [y1; y2; y3; y4; y5; y6; y7; y8; y9; y10; y11]
    | [x1; x2; x3; x4; x5; x6; x7; x8; x9; x10; x11; x12] ->
        let y12 = f x12 in
        let y11 = f x11 in
        let y10 = f x10 in
        let y9 = f x9 in
        let y8 = f x8 in
        let y7 = f x7 in
        let y6 = f x6 in
        let y5 = f x5 in
        let y4 = f x4 in
        let y3 = f x3 in
        let y2 = f x2 in
        let y1 = f x1 in
        [y1; y2; y3; y4; y5; y6; y7; y8; y9; y10; y11; y12]
  in

  let map_tail_chunk suffix chunk =
    match[@ocaml.warning "-8"] chunk with
    | x1
      :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: x9 :: x10 :: x11 :: x12 :: _
      ->
        let y12 = f x12 in
        let y11 = f x11 in
        let y10 = f x10 in
        let y9 = f x9 in
        let y8 = f x8 in
        let y7 = f x7 in
        let y6 = f x6 in
        let y5 = f x5 in
        let y4 = f x4 in
        let y3 = f x3 in
        let y2 = f x2 in
        let y1 = f x1 in
        y1 :: y2 :: y3 :: y4 :: y5 :: y6 :: y7 :: y8 :: y9 :: y10 :: y11 :: y12
        :: suffix
  in

  let rec map_all_tail_chunks suffix chunks =
    match chunks with
    | [] -> suffix
    | chunk :: more ->
        (map_all_tail_chunks [@ocaml.tailcall])
          (map_tail_chunk suffix chunk)
          more
  in

  let chunks = split [] l in

  match chunks with
  | [] -> []
  | first :: rest -> map_all_tail_chunks (map_head_chunk first) rest

let chunked_tail_recursive_mapi_12 i f l =
  let rec split i chunks l =
    match l with
    | _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: tail ->
        (split (i + 12) [@ocaml.tailcall]) ((i, l) :: chunks) tail
    | _ -> (i, l) :: chunks
  in

  let map_head_chunk i chunk =
    match[@ocaml.warning "-8"] chunk with
    | [] -> []
    | [x1] ->
        let y1 = f i x1 in
        [y1]
    | [x1; x2] ->
        let y2 = f (i + 1) x2 in
        let y1 = f i x1 in
        [y1; y2]
    | [x1; x2; x3] ->
        let y3 = f (i + 2) x3 in
        let y2 = f (i + 1) x2 in
        let y1 = f i x1 in
        [y1; y2; y3]
    | [x1; x2; x3; x4] ->
        let y4 = f (i + 3) x4 in
        let y3 = f (i + 2) x3 in
        let y2 = f (i + 1) x2 in
        let y1 = f i x1 in
        [y1; y2; y3; y4]
    | [x1; x2; x3; x4; x5] ->
        let y5 = f (i + 4) x5 in
        let y4 = f (i + 3) x4 in
        let y3 = f (i + 2) x3 in
        let y2 = f (i + 1) x2 in
        let y1 = f i x1 in
        [y1; y2; y3; y4; y5]
    | [x1; x2; x3; x4; x5; x6] ->
        let y6 = f (i + 5) x6 in
        let y5 = f (i + 4) x5 in
        let y4 = f (i + 3) x4 in
        let y3 = f (i + 2) x3 in
        let y2 = f (i + 1) x2 in
        let y1 = f i x1 in
        [y1; y2; y3; y4; y5; y6]
    | [x1; x2; x3; x4; x5; x6; x7] ->
        let y7 = f (i + 6) x7 in
        let y6 = f (i + 5) x6 in
        let y5 = f (i + 4) x5 in
        let y4 = f (i + 3) x4 in
        let y3 = f (i + 2) x3 in
        let y2 = f (i + 1) x2 in
        let y1 = f i x1 in
        [y1; y2; y3; y4; y5; y6; y7]
    | [x1; x2; x3; x4; x5; x6; x7; x8] ->
        let y8 = f (i + 7) x8 in
        let y7 = f (i + 6) x7 in
        let y6 = f (i + 5) x6 in
        let y5 = f (i + 4) x5 in
        let y4 = f (i + 3) x4 in
        let y3 = f (i + 2) x3 in
        let y2 = f (i + 1) x2 in
        let y1 = f i x1 in
        [y1; y2; y3; y4; y5; y6; y7; y8]
    | [x1; x2; x3; x4; x5; x6; x7; x8; x9] ->
        let y9 = f (i + 8) x9 in
        let y8 = f (i + 7) x8 in
        let y7 = f (i + 6) x7 in
        let y6 = f (i + 5) x6 in
        let y5 = f (i + 4) x5 in
        let y4 = f (i + 3) x4 in
        let y3 = f (i + 2) x3 in
        let y2 = f (i + 1) x2 in
        let y1 = f i x1 in
        [y1; y2; y3; y4; y5; y6; y7; y8; y9]
    | [x1; x2; x3; x4; x5; x6; x7; x8; x9; x10] ->
        let y10 = f i x10 in
        let y9 = f (i + 8) x9 in
        let y8 = f (i + 7) x8 in
        let y7 = f (i + 6) x7 in
        let y6 = f (i + 5) x6 in
        let y5 = f (i + 4) x5 in
        let y4 = f (i + 3) x4 in
        let y3 = f (i + 2) x3 in
        let y2 = f (i + 1) x2 in
        let y1 = f i x1 in
        [y1; y2; y3; y4; y5; y6; y7; y8; y9; y10]
    | [x1; x2; x3; x4; x5; x6; x7; x8; x9; x10; x11] ->
        let y11 = f (i + 10) x11 in
        let y10 = f (i + 9) x10 in
        let y9 = f (i + 8) x9 in
        let y8 = f (i + 7) x8 in
        let y7 = f (i + 6) x7 in
        let y6 = f (i + 5) x6 in
        let y5 = f (i + 4) x5 in
        let y4 = f (i + 3) x4 in
        let y3 = f (i + 2) x3 in
        let y2 = f (i + 1) x2 in
        let y1 = f i x1 in
        [y1; y2; y3; y4; y5; y6; y7; y8; y9; y10; y11]
    | [x1; x2; x3; x4; x5; x6; x7; x8; x9; x10; x11; x12] ->
        let y12 = f (i + 11) x12 in
        let y11 = f (i + 10) x11 in
        let y10 = f (i + 9) x10 in
        let y9 = f (i + 8) x9 in
        let y8 = f (i + 7) x8 in
        let y7 = f (i + 6) x7 in
        let y6 = f (i + 5) x6 in
        let y5 = f (i + 4) x5 in
        let y4 = f (i + 3) x4 in
        let y3 = f (i + 2) x3 in
        let y2 = f (i + 1) x2 in
        let y1 = f i x1 in
        [y1; y2; y3; y4; y5; y6; y7; y8; y9; y10; y11; y12]
  in

  let map_tail_chunk suffix (i, chunk) =
    match[@ocaml.warning "-8"] chunk with
    | x1
      :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: x9 :: x10 :: x11 :: x12 :: _
      ->
        let y12 = f (i + 11) x12 in
        let y11 = f (i + 10) x11 in
        let y10 = f (i + 9) x10 in
        let y9 = f (i + 8) x9 in
        let y8 = f (i + 7) x8 in
        let y7 = f (i + 6) x7 in
        let y6 = f (i + 5) x6 in
        let y5 = f (i + 4) x5 in
        let y4 = f (i + 3) x4 in
        let y3 = f (i + 2) x3 in
        let y2 = f (i + 1) x2 in
        let y1 = f i x1 in
        y1 :: y2 :: y3 :: y4 :: y5 :: y6 :: y7 :: y8 :: y9 :: y10 :: y11 :: y12
        :: suffix
  in

  let rec map_all_tail_chunks suffix chunks =
    match chunks with
    | [] -> suffix
    | chunk :: more ->
        (map_all_tail_chunks [@ocaml.tailcall])
          (map_tail_chunk suffix chunk)
          more
  in

  let chunks = split i [] l in

  match chunks with
  | [] -> []
  | (i, first) :: rest -> map_all_tail_chunks (map_head_chunk i first) rest

(* Combines the 5x unrolled non-tail-recursive map for a prefix of 5000
   elements, followed by the tail-recursive new fast map for the remainder. *)
let faster_map f l =
  plain_unrolled_prefix_5 chunked_tail_recursive_map_12 1000 f l

let faster_mapi f l =
  plain_unrolled_prefix_5i chunked_tail_recursive_mapi_12 1000 0 f l

(* wrapper that is actually exported *)
let map_pure f l = faster_map f l

let mapi_pure f l = faster_mapi f l

let rec append rev_acc xs ys =
  match xs with
  | [] -> List.rev_append rev_acc ys
  | [x1] -> List.rev_append rev_acc (x1 :: ys)
  | [x1; x2] -> List.rev_append rev_acc (x1 :: x2 :: ys)
  | [x1; x2; x3] -> List.rev_append rev_acc (x1 :: x2 :: x3 :: ys)
  | [x1; x2; x3; x4] -> List.rev_append rev_acc (x1 :: x2 :: x3 :: x4 :: ys)
  | [x1; x2; x3; x4; x5] ->
      List.rev_append rev_acc (x1 :: x2 :: x3 :: x4 :: x5 :: ys)
  | [x1; x2; x3; x4; x5; x6] ->
      List.rev_append rev_acc (x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: ys)
  | [x1; x2; x3; x4; x5; x6; x7] ->
      List.rev_append rev_acc (x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: ys)
  | [x1; x2; x3; x4; x5; x6; x7; x8] ->
      List.rev_append
        rev_acc
        (x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: ys)
  | [x1; x2; x3; x4; x5; x6; x7; x8; x9] ->
      List.rev_append
        rev_acc
        (x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: x9 :: ys)
  | [x1; x2; x3; x4; x5; x6; x7; x8; x9; x10] ->
      List.rev_append
        rev_acc
        (x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: x9 :: x10 :: ys)
  | [x1; x2; x3; x4; x5; x6; x7; x8; x9; x10; x11] ->
      List.rev_append
        rev_acc
        (x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: x9 :: x10 :: x11 :: ys)
  | [x1; x2; x3; x4; x5; x6; x7; x8; x9; x10; x11; x12] ->
      List.rev_append
        rev_acc
        (x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: x9 :: x10 :: x11 :: x12
       :: ys)
  | x1
    :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: x9 :: x10 :: x11 :: x12 :: xs
    ->
      append
        (x12 :: x11 :: x10 :: x9 :: x8 :: x7 :: x6 :: x5 :: x4 :: x3 :: x2 :: x1
       :: rev_acc)
        xs
        ys

let append xs ys =
  match xs with
  | [] -> ys
  | [x1] -> x1 :: ys
  | [x1; x2] -> x1 :: x2 :: ys
  | [x1; x2; x3] -> x1 :: x2 :: x3 :: ys
  | [x1; x2; x3; x4] -> x1 :: x2 :: x3 :: x4 :: ys
  | [x1; x2; x3; x4; x5] -> x1 :: x2 :: x3 :: x4 :: x5 :: ys
  | [x1; x2; x3; x4; x5; x6] -> x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: ys
  | [x1; x2; x3; x4; x5; x6; x7] -> x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: ys
  | [x1; x2; x3; x4; x5; x6; x7; x8] ->
      x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: ys
  | [x1; x2; x3; x4; x5; x6; x7; x8; x9] ->
      x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: x9 :: ys
  | [x1; x2; x3; x4; x5; x6; x7; x8; x9; x10] ->
      x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: x9 :: x10 :: ys
  | [x1; x2; x3; x4; x5; x6; x7; x8; x9; x10; x11] ->
      x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: x9 :: x10 :: x11 :: ys
  | [x1; x2; x3; x4; x5; x6; x7; x8; x9; x10; x11; x12] ->
      x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: x9 :: x10 :: x11 :: x12
      :: ys
  | x1
    :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: x9 :: x10 :: x11 :: x12 :: xs
    ->
      append [x12; x11; x10; x9; x8; x7; x6; x5; x4; x3; x2; x1] xs ys
