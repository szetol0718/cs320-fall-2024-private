let is_prime n =
  let go i =
    if i = n then
      true
    else if n mod i = 0 then
      false
    else
      go (i + 1)
  in
  if   n < 2
  then false
  else go 2

let nth_prime n =
    let rec find_prime count current =
      if count = n then current
      else if is_prime current then find_prime (count + 1) (current + 1)
      else find_prime count (current + 1)
    in
    find_prime 0 