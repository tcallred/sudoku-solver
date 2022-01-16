(* Sudoku Solver by Taylor Allred *)

module Coord = struct
  type t = int * int

  let compare (x1, y1) (x2, y2) = x2 - x1 + (y2 - y1)
end

module Board = Map.Make (Coord)

type sudoku = { board : int Board.t; dims : int * int }

let rec range_ab a b = if a >= b then [] else a :: range_ab (a + 1) b
let range = range_ab 0

let make_board w h =
  let dims = (w, h) in
  let board = Board.empty in
  { board; dims }

let make_filled_board w h =
  let dims = (w, h) in
  let board =
    range (w * h)
    |> List.fold_left
         (fun board i ->
           range (w * h)
           |> List.fold_left
                (fun board j -> Board.add (i, j) ((i + j) mod 10) board)
                board)
         Board.empty
  in
  { board; dims }

let print_board { board; dims = w, h } =
  let open Printf in
  range (w * h)
  |> List.iter (fun i ->
         if i mod h == 0 then print_endline "\n" else print_endline "";
         range (w * h)
         |> List.iter (fun j ->
                match Board.find_opt (i, j) board with
                | Some elt -> printf (if j mod w = 0 then " %d" else "%d") elt
                | None -> printf (if j mod w = 0 then " %s" else "%s") "#"));
  print_endline "\n"
