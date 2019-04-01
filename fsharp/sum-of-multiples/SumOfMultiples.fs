module SumOfMultiples

let isMultiple x y = y <> 0 && x % y = 0

let sum numbers limit =
    [ 1 .. limit - 1 ]
    |> List.filter (fun current -> numbers |> List.exists (isMultiple current))
    |> List.sum
