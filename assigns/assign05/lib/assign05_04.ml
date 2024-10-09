(* Define the set_info type for FuncSet *)
type set_info = {
  ind : int -> bool;
  mn : int;
  mx : int;
}

(* Module ListSet: represents finite sets of integers as sorted lists of distinct integers *)
module ListSet = struct
  (* Type for ListSet is int list *)
  type t = int list

  (* Return the empty set *)
  let empty = []

  (* Return a singleton set containing the element n *)
  let singleton n = [n]

  (* Check if element x is in the set s *)
  let mem x s = List.mem x s

  (* Return the cardinality of the set s *)
  let card s = List.length s

  (* Union of two sets, represented as sorted lists of distinct integers *)
  let rec union s1 s2 =
    match s1, s2 with
    | [], s | s, [] -> s
    | x1::xs1, x2::xs2 ->
        if x1 < x2 then x1 :: union xs1 s2
        else if x1 > x2 then x2 :: union s1 xs2
        else x1 :: union xs1 xs2
end

(* Module FuncSet: represents finite sets of integers using set_info record *)
module FuncSet = struct
  (* Type for FuncSet is set_info *)
  type t = set_info

  (* Return the empty set *)
  let empty = { ind = (fun _ -> false); mn = 1; mx = 0 }

  (* Return a singleton set containing the element n *)
  let singleton n = { ind = (fun x -> x = n); mn = n; mx = n }

  (* Check if element x is in the set *)
  let mem x s = s.ind x

  (* Return the cardinality of the set s *)
  let card s =
    let rec aux count x =
      if x > s.mx then count
      else if s.ind x then aux (count + 1) (x + 1)
      else aux count (x + 1)
    in
    aux 0 s.mn

  (* Union of two sets, using indicator function and updating bounds *)
  let union s1 s2 =
    {
      ind = (fun x -> s1.ind x || s2.ind x);
      mn = min s1.mn s2.mn;
      mx = max s1.mx s2.mx;
    }
end
