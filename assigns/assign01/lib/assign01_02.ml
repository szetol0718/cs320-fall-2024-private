let is_prime n =
  let rec check d =
    d * d > n || (n mod d <> 0 && check (d + 1))
  in
  check 2

let nth_prime n =
    let rec find_prime count current =
      if count = n then current
      else if is_prime current then find_prime (count + 1) (current + 1)
      else find_prime count (current + 1)
    in
    find_prime 0 2