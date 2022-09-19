(*
Copyright (c) 2016-2022 Jane Street Group, LLC <opensource@janestreet.com>
Copyright (c) 2022-2022 Nomadic Labs <contact@nomadic-labs.com>
*)

let tc_limit =
  match Sys.backend_type with
  | Other "js_of_ocaml" -> 50
  | Other _ | Native | Bytecode -> 1000

let tail_append l1 l2 = List.rev_append (List.rev l1) l2

let rec count_append l1 l2 count =
  match l1 with
  | [] -> l2
  | [x1] -> x1 :: l2
  | [x1; x2] -> x1 :: x2 :: l2
  | [x1; x2; x3] -> x1 :: x2 :: x3 :: l2
  | [x1; x2; x3; x4] -> x1 :: x2 :: x3 :: x4 :: l2
  | x1 :: x2 :: x3 :: x4 :: x5 :: tl ->
      x1 :: x2 :: x3 :: x4 :: x5
      ::
      (if count > tc_limit then tail_append tl l2
      else count_append tl l2 (count + 1))

let append l1 l2 = match l2 with [] -> l1 | _ -> count_append l1 l2 0

let tail_map xs ~f =
  let rec rise ys = function
    | [] -> ys
    | (y0, y1, y2, y3, y4, y5, y6, y7, y8) :: bs ->
        rise (y0 :: y1 :: y2 :: y3 :: y4 :: y5 :: y6 :: y7 :: y8 :: ys) bs
  in
  let rec dive bs = function
    | x0 :: x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: xs ->
        let y0 = f x0 in
        let y1 = f x1 in
        let y2 = f x2 in
        let y3 = f x3 in
        let y4 = f x4 in
        let y5 = f x5 in
        let y6 = f x6 in
        let y7 = f x7 in
        let y8 = f x8 in
        dive ((y0, y1, y2, y3, y4, y5, y6, y7, y8) :: bs) xs
    | xs -> rise (List.map f xs) bs
  in
  dive [] xs

let rec count_map ~f l count =
  match l with
  | [] -> []
  | [x1] ->
      let f1 = f x1 in
      [f1]
  | [x1; x2] ->
      let f1 = f x1 in
      let f2 = f x2 in
      [f1; f2]
  | [x1; x2; x3] ->
      let f1 = f x1 in
      let f2 = f x2 in
      let f3 = f x3 in
      [f1; f2; f3]
  | [x1; x2; x3; x4] ->
      let f1 = f x1 in
      let f2 = f x2 in
      let f3 = f x3 in
      let f4 = f x4 in
      [f1; f2; f3; f4]
  | x1 :: x2 :: x3 :: x4 :: x5 :: tl ->
      let f1 = f x1 in
      let f2 = f x2 in
      let f3 = f x3 in
      let f4 = f x4 in
      let f5 = f x5 in
      f1 :: f2 :: f3 :: f4 :: f5
      ::
      (if count > tc_limit then tail_map ~f tl else count_map ~f tl (count + 1))

let tail_mapi xs ~f i =
  let rec rise ys = function
    | [] -> ys
    | (y0, y1, y2, y3, y4, y5, y6, y7, y8) :: bs ->
        rise (y0 :: y1 :: y2 :: y3 :: y4 :: y5 :: y6 :: y7 :: y8 :: ys) bs
  in
  let rec dive i bs = function
    | x0 :: x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: xs ->
        let y0 = f (i + 0) x0 in
        let y1 = f (i + 1) x1 in
        let y2 = f (i + 2) x2 in
        let y3 = f (i + 3) x3 in
        let y4 = f (i + 4) x4 in
        let y5 = f (i + 5) x5 in
        let y6 = f (i + 6) x6 in
        let y7 = f (i + 7) x7 in
        let y8 = f (i + 8) x8 in
        dive (i + 9) ((y0, y1, y2, y3, y4, y5, y6, y7, y8) :: bs) xs
    | xs -> rise (List.mapi (fun j x -> f (i + j) x) xs) bs
  in
  dive i [] xs

let rec count_mapi ~f l count i =
  match l with
  | [] -> []
  | [x1] ->
      let f1 = f i x1 in
      [f1]
  | [x1; x2] ->
      let f1 = f (i + 0) x1 in
      let f2 = f (i + 1) x2 in
      [f1; f2]
  | [x1; x2; x3] ->
      let f1 = f (i + 0) x1 in
      let f2 = f (i + 1) x2 in
      let f3 = f (i + 2) x3 in
      [f1; f2; f3]
  | [x1; x2; x3; x4] ->
      let f1 = f (i + 0) x1 in
      let f2 = f (i + 1) x2 in
      let f3 = f (i + 2) x3 in
      let f4 = f (i + 3) x4 in
      [f1; f2; f3; f4]
  | x1 :: x2 :: x3 :: x4 :: x5 :: tl ->
      let f1 = f (i + 0) x1 in
      let f2 = f (i + 1) x2 in
      let f3 = f (i + 2) x3 in
      let f4 = f (i + 3) x4 in
      let f5 = f (i + 4) x5 in
      f1 :: f2 :: f3 :: f4 :: f5
      ::
      (if count > tc_limit then tail_mapi ~f tl (i + 5)
      else count_mapi ~f tl (count + 1) (i + 5))

let map f l = count_map ~f l 0

let mapi f l = count_mapi ~f l 0 0
