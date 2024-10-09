(* Define the type 'a test *)
type 'a test =
  | TestCase of 'a
  | TestList of 'a test list

(* Implement fold_left *)
let rec fold_left op base = function
  | TestCase x -> op base x
  | TestList lst -> List.fold_left (fold_left op) base lst
