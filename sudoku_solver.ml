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
  |> List.filter_map Fun.id

let get_col col { board; dims = w, h } =
  List.map (fun row -> Board.find_opt (row, col) board) (range (w * h))
  |> List.filter_map Fun.id

let cube_coords (row, col) w h =
  let start_row, start_col = (row - (row mod h), col - (col mod w)) in
  list_comp
    (range_ab start_row (start_row + h))
    (range_ab start_col (start_col + w))

let get_cube coord { board; dims = w, h } =
  List.map (fun c -> Board.find_opt c board) (cube_coords coord w h)
  |> List.filter_map Fun.id

let valid_num_placement n (i, j) { board; dims } =
  let row = get_row i { board; dims } in
  let col = get_col j { board; dims } in
  let cube = get_cube (i, j) { board; dims } in
  (not (List.mem n row)) && (not (List.mem n col)) && not (List.mem n cube)

let board_is_solved { board; dims = w, h } =
  List.length (Board.bindings board) = w * h * (w * h)

let next_coord (row, col) (w, h) =
  if col = (w * h) - 1 then (row + 1, 0) else (row, col + 1)

let n_choices w h = range_ab 1 ((w * h) + 1)

let rec solve coord { board; dims } =
  let w, h = dims in
  let row, _ = coord in
  if board_is_solved { board; dims } || row >= w * h then { board; dims }
  else if Board.mem coord board then
    solve (next_coord coord dims) { board; dims }
  else
    List.fold_left
      (fun { board; dims } n ->
        if valid_num_placement n coord { board; dims } then
          let next =
            solve (next_coord coord dims)
              { board = Board.add coord n board; dims }
          in
          if board_is_solved next then next else { board; dims }
        else { board; dims })
      { board; dims } (n_choices w h)

let solve_board { board; dims } = solve (0, 0) { board; dims }

exception Timeout

let delayed_fun f x timeout =
  let _ =
    Sys.set_signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise Timeout))
  in
  ignore (Unix.alarm timeout);
  try
    let r = f x in
    ignore (Unix.alarm 0);
    r
  with e ->
    ignore (Unix.alarm 0);
    raise e

let solvable (s : sudoku) =
  try
    let _ = delayed_fun solve_board s 500 in
    true
  with _ -> false

let gen_board w h =
  let dims = (w, h) in
  let board =
    range (w * h)
    |> List.fold_left
         (fun board i ->
           range (w * h)
           |> List.fold_left
                (fun board j ->
                  let chosen = Random.int (w * h) + 1 in
                  if
                    Random.bool () && Random.bool ()
                    && valid_num_placement chosen (i, j) { board; dims }
                    && solvable { board = Board.add (i, j) chosen board; dims }
                  then Board.add (i, j) chosen board
                  else board)
                board)
         Board.empty
  in
  { board; dims }
