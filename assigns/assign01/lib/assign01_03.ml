open Assign01_02

let nth s i =
  let rec help s prime count = 
    if (s mod i) <> 0 then count
    else help (s/prime) prime (count+1)
  in
let prime = nth_prime i in 
help s prime 
