module SumOfMultiples

let isMultiple x y = y <> 0 && x % y = 0

let sum numbers limit =
    (0, [ 1 .. limit - 1 ])
    ||> List.fold (fun acc current ->
        if numbers |> List.exists (isMultiple current)
        then acc + current
        else acc)