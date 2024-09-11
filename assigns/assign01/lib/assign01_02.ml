open Assign01_01
let is_prime n =
  let rec check d =
    if pow d 2 > n then true
    else if n mod d = 0 then false
    else check (d + 1)
  in
  n > 1 && check 2

let nth_prime n =
    let rec find_prime count current =
      if count = n && is_prime current then current
      else if is_prime current then find_prime (count + 1) (current + 1)
      else find_prime count (current + 1)
    in
    find_prime 0 2