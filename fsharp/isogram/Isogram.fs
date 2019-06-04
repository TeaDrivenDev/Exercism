module Isogram

let isIsogram (str : string) =
    str.ToLower()
    |> Seq.filter System.Char.IsLetter
    |> Seq.groupBy id
    |> Seq.exists (fun (_, letters) -> Seq.length letters > 1)
    |> not