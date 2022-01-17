(* Sudoku Solver by Taylor Allred *)
module Coord = struct
  type t = int * int

  let compare = compare
end

module Board = Map.Make (Coord)

type sudoku = { board : int Board.t; dims : int * int }

let rec range_ab a b = if a >= b then [] else a :: range_ab (a + 1) b
let range = range_ab 0

let list_comp l1 l2 =
  let ( let* ) x f = List.map f x |> List.concat in
  let ( let+ ) x f = List.map f x in
  let* x = l1 in
  let+ y = l2 in
  (x, y)

let make_empty_board w h =
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
                (fun board j -> Board.add (i, j) (((i + j) mod 9) + 1) board)
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

let get_row row { board; dims = w, h } =
  List.map (fun col -> Board.find_opt (row, col) board) (range (w * h))
  |> List.filter Option.is_some
  |> List.map (fun elt -> Option.value elt ~default:0)

let get_col col { board; dims = w, h } =
  List.map (fun row -> Board.find_opt (row, col) board) (range (w * h))
  |> List.filter Option.is_some
  |> List.map (fun elt -> Option.value elt ~default:0)

let cube_coords (row, col) w h =
  let cube_start = (row - (row mod h), col - (col mod w)) in
  let srow, scol = cube_start in
  list_comp (range_ab srow (srow + h)) (range_ab scol (scol + w))

let get_cube coord { board; dims = w, h } =
  List.map (fun c -> Board.find_opt c board) (cube_coords coord w h)
  |> List.filter Option.is_some
  |> List.map (fun elt -> Option.value elt ~default:0)

let gen_board w h =
  let dims = (w, h) in
  let board =
    range (w * h)
    |> List.fold_left
         (fun board i ->
           range (w * h)
           |> List.fold_left
                (fun board j ->
                  if Random.bool () then
                    let chosen = Random.int (w * h) + 1 in
                    let row = get_row i { board; dims } in
                    let col = get_col j { board; dims } in
                    let cube = get_cube (i, j) { board; dims } in
                    if
                      (not (List.mem chosen row))
                      && (not (List.mem chosen col))
                      && not (List.mem chosen cube)
                    then Board.add (i, j) chosen board
                    else board
                  else board)
                board)
         Board.empty
  in
  { board; dims }
