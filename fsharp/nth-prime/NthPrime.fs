module NthPrime

let isPrime n =
    seq { 2 .. n |> float |> sqrt |> int }
    |> Seq.exists (fun x -> n % x = 0)
    |> not

let prime nth : int option =
    if nth = 0
    then None
    else
        Seq.initInfinite id
        |> Seq.skip 1
        |> Seq.choose (fun i -> if isPrime i then Some i else None)
        |> Seq.item nth
        |> Some