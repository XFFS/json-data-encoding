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
        if count <= 0 then map_suffix (i + 5) f tail
        else plain_unrolled_prefix_5i map_suffix (count - 1) (i + 5) f tail
      in
      let y5 = f (i + 4) x5 in
      let y4 = f (i + 3) x4 in
      let y3 = f (i + 2) x3 in
      let y2 = f (i + 1) x2 in
      let y1 = f i x1 in
      y1 :: y2 :: y3 :: y4 :: y5 :: tail

(* Fast tail-recursive map for the list suffix. *)
let chunked_tail_recursive_map_12 =
  let rec split chunks l =
    match l with
    | _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: tail ->
        (split [@ocaml.tailcall]) (l :: chunks) tail
    | _ -> l :: chunks
  in

  let map_chunk f suffix chunk =
    match chunk with
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
    | l -> List.map f l
  in

  let rec map_chunks f suffix chunks =
    match chunks with
    | [] -> suffix
    | chunk :: more ->
        (map_chunks [@ocaml.tailcall]) f (map_chunk f suffix chunk) more
  in
  fun f l ->
    let chunks = split [] l in
    map_chunks f [] chunks

let chunked_tail_recursive_mapi_12 =
  let rec split i chunks l =
    match l with
    | _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: tail ->
        (split (i + 12) [@ocaml.tailcall]) ((i, l) :: chunks) tail
    | _ -> (i, l) :: chunks
  in
  let map_chunk f suffix (i, chunk) =
    match chunk with
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
    | l -> List.mapi (fun j x -> f (i + j) x) l
  in

  let rec map_chunks f suffix chunks =
    match chunks with
    | [] -> suffix
    | chunk :: more ->
        (map_chunks [@ocaml.tailcall]) f (map_chunk f suffix chunk) more
  in

  fun i f l ->
    let chunks = split i [] l in
    map_chunks f [] chunks

let limit =
  match Sys.backend_type with
  | Other "js_of_ocaml" -> 50
  | Other _ | Native | Bytecode -> 1000

(* Combines the 5x unrolled non-tail-recursive map for a prefix of 5000
   elements, followed by the tail-recursive new fast map for the remainder. *)
let faster_map f l =
  plain_unrolled_prefix_5 chunked_tail_recursive_map_12 limit f l

let faster_mapi f l =
  plain_unrolled_prefix_5i chunked_tail_recursive_mapi_12 limit 0 f l

(* wrapper that is actually exported *)
let map_pure f l = faster_map f l

let mapi_pure f l = faster_mapi f l

let rec append5 count xs ys =
  match xs with
  | [] -> ys
  | [x1] -> x1 :: ys
  | [x1; x2] -> x1 :: x2 :: ys
  | [x1; x2; x3] -> x1 :: x2 :: x3 :: ys
  | [x1; x2; x3; x4] -> x1 :: x2 :: x3 :: x4 :: ys
  | x1 :: x2 :: x3 :: x4 :: x5 :: tl ->
      x1 :: x2 :: x3 :: x4 :: x5
      ::
      (if count > limit then List.rev_append (List.rev tl) ys
      else append5 (count + 1) tl ys)

let append xs ys = match ys with [] -> xs | _ -> append5 limit xs ys
