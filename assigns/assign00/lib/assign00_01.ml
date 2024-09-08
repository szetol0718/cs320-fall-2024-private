let sqrt n =
    let rec k n_temp = 
        if k*k > n_temp then k
        else 
            k = k + 1
    in
    if n = 0 then 0
    else k n