open Assign01_02
open Assign01_03

let to_string s = 
  let rec decode s i acc =
    if s = 1 then List.rev acc
    else 
      let ex = nth s i in 
      decode (s/(nth_prime i) ** ex) (i+1) ( ex:: acc)
    in
    let elements = decode s 0 [] in
    "[" ^ (Sting.concat ";" (List.map string_of_int elements)) ^ "]" 
