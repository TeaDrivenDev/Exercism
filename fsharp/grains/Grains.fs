module Grains

open System.Numerics

let asSnd fst snd = fst, snd

let square n =
    if n <= 0 || n > 64
    then Error  "square must be between 1 and 64"
    else Ok <| uint64 (BigInteger.Pow(BigInteger(2), n - 1))

let total =
    [ 1..64 ]
    |> List.map square
    |> asSnd (Ok 0UL)
    ||> List.fold (fun acc current ->
        current
        |> Result.bind (fun r ->
            acc |> Result.map (fun a -> a + uint64 r)))
