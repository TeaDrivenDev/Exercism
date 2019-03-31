module Pangram

open System.Text.RegularExpressions

let alphabet = ['a'..'z'] |> set

let isPangram (input : string) =
    let input =
        Regex.Replace(input.ToLower(), "[\W]", "")
        |> set

    alphabet - input |> Set.isEmpty