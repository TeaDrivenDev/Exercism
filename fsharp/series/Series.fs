module Series

open System

let slices (input : string) n =
    if n < 1 || input.Length < n
    then None
    else
        input
        |> Seq.windowed n
        |> Seq.map (Seq.toArray >> String)
        |> Seq.toList
        |> Some
