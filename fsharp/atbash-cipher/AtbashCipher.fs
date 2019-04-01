module AtbashCipher

open System.Text.RegularExpressions

let cipher =
    let atoz, digits = ['a'..'z'], ['0'..'9']

    (atoz @ digits, List.rev atoz @ digits)
    ||> List.zip
    |> Map.ofList

let encode (words : string) =
    Regex.Replace(words.ToLower(), @"[^\d\w]", "")
    |> Seq.map (fun c -> cipher.[c])
    |> Seq.chunkBySize 5
    |> Seq.map System.String
    |> String.concat " "

let decode (code : string) =
    (encode code).Replace(" ", "")