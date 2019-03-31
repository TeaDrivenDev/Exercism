module Acronym

let abbreviate (phrase : string) =
    phrase.Split([| ' '; '-' |], System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.collect (fun word ->
        if word = word.ToUpper()
        then [| |]
        else
            word.[1..]
            |> Seq.filter System.Char.IsUpper
            |> Seq.toArray
        |> Array.append [| word.[0] |])
    |> System.String
    |> fun s -> s.ToUpper()