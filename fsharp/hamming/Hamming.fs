module Hamming

let distance (a : string) (b : string) =
    if a.Length = b.Length
    then
        (a, b)
        ||> Seq.zip
        |> Seq.sumBy (fun (a, b) -> if a = b then 0 else 1)
        |> Some
    else None
