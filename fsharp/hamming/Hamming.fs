module Hamming

let distance (a : string) (b : string) =
    if a.Length = b.Length
    then
        (a, b)
        ||> Seq.zip
        |> Seq.fold (fun acc (a, b) -> acc + (if a = b then 0 else 1)) 0
        |> Some
    else None
