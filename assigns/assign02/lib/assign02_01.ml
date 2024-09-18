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

(*helper to get the position with row index*)
let get_row (p1, p2, p3) col =
  match col with
  | Left -> p1
  | Middle -> p2
  | Right -> p3

(* Function to get the position from the board based on row and column index *)
let get_pos (b: board) ((ri, ci): pos_index): pos = 
  let (r1, r2, r3) = b in
  match ri with
  | Top -> get_row r1 ci
  | Middle -> get_row r2 ci
  | Bottom -> get_row r3 ci

(* Helper to check if three positions have the same piece *)
let three_in_a_row p1 p2 p3 =
  match (p1, p2, p3) with
  | (Piece X, Piece X, Piece X) -> true
  | (Piece O, Piece O, Piece O) -> true
  | _ -> false

(* Function to check if there is a winner on the board *)
  let winner (b: board): bool =
    let (r1, r2, r3) = b in
    let (p11, p12, p13) = r1 in
    let (p21, p22, p23) = r2 in
    let (p31, p32, p33) = r3 in
  
    (* Check rows, columns, and diagonals *)
    three_in_a_row p11 p12 p13 || (* Row 1 *)
    three_in_a_row p21 p22 p23 || (* Row 2 *)
    three_in_a_row p31 p32 p33 || (* Row 3 *)
    three_in_a_row p11 p21 p31 || (* Column 1 *)
    three_in_a_row p12 p22 p32 || (* Column 2 *)
    three_in_a_row p13 p23 p33 || (* Column 3 *)
    three_in_a_row p11 p22 p33 || (* Diagonal 1 *)
    three_in_a_row p13 p22 p31   (* Diagonal 2 *)