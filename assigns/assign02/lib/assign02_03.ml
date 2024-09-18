(* Define the directions and path types *)
type dir = 
  | North
  | South
  | East
  | West

type path = dir list

(* Function to compute the Euclidean distance between start and end of the path *)
let dist (p: path): float =
  (* Helper function to accumulate movement along x and y axes *)
  let rec walk p (x, y) =
    match p with
    | [] -> (x, y)
    | North :: t -> walk t (x, y + 1)
    | South :: t -> walk t (x, y - 1)
    | East  :: t -> walk t (x + 1, y)
    | West  :: t -> walk t (x - 1, y)
  in
  let (final_x, final_y) = walk p (0, 0) in
  sqrt (float_of_int (final_x * final_x + final_y * final_y))