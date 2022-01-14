(* Sudoku Solver by Taylor Allred *)

type sudoku =
  { board: int option array array;
    dims : int * int;
  }

let make_board w h = 
  let dims = (w, h)
  in let board = Array.make_matrix (w * h) (w * h) None
  in {
    board = board;
    dims = dims;
  } 

let make_filled_board w h = 
  let dims = (w, h)
  in let board = Array.make_matrix (w * h) (w * h) (Some 0)
  in 
  for i = 0 to (w * h) - 1 do
    for j = 0 to (w * h) - 1 do
      board.(i).(j) <- Some ((i + j) mod 10);
    done;
  done;
  { board = board;
    dims = dims;
  } 

let print_board {board = board; dims = (w, h);} =
  let open Printf in
  begin
    Array.iteri (
      fun i row -> 
        begin 
          if i mod h == 0 then print_endline "\n" else print_endline "";
          Array.iteri (
            fun j elt ->
              match elt with
              | Some elt' -> printf (if j mod w = 0 then " %d" else "%d") elt'
              | None -> printf (if j mod w = 0 then " %s" else "%s") "#"
          )
            row;
        end
    )
      board;
    print_endline "\n";
  end

