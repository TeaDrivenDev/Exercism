module Raindrops

let findPrimeFactor n =
    [ 2 .. n/2 ] |> List.tryFind ((%) n >> (=) 0)

let (|Low|NoPrimeFactors|PrimeFactor|) n =
    if n <= 2
    then Low
    else
        match findPrimeFactor n with
        | Some f -> PrimeFactor (f, n / f)
        | None -> NoPrimeFactors

let rec primeFactors n =
    match n with
    | Low | NoPrimeFactors -> [ n ]
    | PrimeFactor (f, remainder) -> f :: primeFactors remainder

let toPling factors =
    let set = set factors

    [3, "Pling"; 5, "Plang"; 7, "Plong"]
    |> List.filter (fst >> set.Contains)
    |> function
        | [] -> None
        | factors ->
            factors
            |> List.map snd
            |> String.concat ""
            |> Some

let convert number =
    primeFactors number
    |> toPling
    |> Option.defaultWith (fun () -> string number)