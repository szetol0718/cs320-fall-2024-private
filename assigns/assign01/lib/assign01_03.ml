open Assign01_02

let nth s i =
  let prime = nth_prime i in
  let rec count_exponent s count =
    if s mod prime = 0 then count_exponent (s / prime) (count + 1)
    else count
  in
  count_exponent s 0
