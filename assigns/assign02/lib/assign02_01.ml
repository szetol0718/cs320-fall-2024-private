type piece = 
  | X
  | O

type pos = 
  | Piece of piece
  | Blank

type board = (pos * pos * pos) * (pos * pos * pos) * (pos * pos * pos)

type row_index = 
  | Top
  | Middle
  | Bottom

type col_index = 
  | Left
  | Middle
  | Right

type pos_index = row_index * col_index

(* Function to get the position from the board based on row and column index *)
let get_pos (b: board) (index: pos_index): pos = 
  let (r1, r2, r3) = b in
  let get_row r = 
    match r with
    | Left -> fst (fst r)
    | Middle -> snd (fst r)
    | Right -> snd r in
  match index with
  | (Top, c) -> get_row (r1, c)
  | (Middle, c) -> get_row (r2, c)
  | (Bottom, c) -> get_row (r3, c)

(* Helper to check if three positions have the same piece *)
let three_in_a_row p1 p2 p3 =
  match (p1, p2, p3) with
  | (Piece X, Piece X, Piece X) -> true
  | (Piece O, Piece O, Piece O) -> true
  | _ -> false

(* Function to check if there is a winner on the board *)
let winner (b: board): bool =
  let (r1, r2, r3) = b in
  let check_row (a, b, c) = three_in_a_row a b c in
  let check_col (a1, a2, a3) (b1, b2, b3) (c1, c2, c3) = 
    three_in_a_row a1 a2 a3 
    || three_in_a_row b1 b2 b3 
    || three_in_a_row c1 c2 c3 
  in
  let check_diagonal () = three_in_a_row (fst r1) (snd r1) (snd r1) 





