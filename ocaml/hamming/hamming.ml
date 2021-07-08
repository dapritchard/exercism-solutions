open Base

type nucleotide = A | C | G | T

let compare_nucl x y =
  match x,y with
  | A, A -> 0
  | C, C -> 0
  | G, G -> 0
  | T, T -> 0
  | _, _ -> 1

let hamming_distance l1 l2 =

  let diff_nucl = List.map2 l1 l2 ~f:compare_nucl in
  match l1, l2, diff_nucl with

  (* Lists of equal length *)
  | _, _, List.Or_unequal_lengths.Ok x -> Ok (List.fold x ~init:0 ~f:(+))

  (* Lists of nonequal length *)
  | [],  _, _ -> Error "left strand must not be empty"
  | _,  [], _ -> Error "right strand must not be empty"
  | _,   _, _ -> Error "left and right strands must be of equal length"
