module PigLatin

let trueVowels = [ "a"; "e"; "i"; "o"; "u" ]
let vowels = trueVowels @ [ "yt"; "xr" ]

let trueConsonants =
    Set.ofList ([ 'a' .. 'z' ] |> List.map string)
    - Set.ofList trueVowels
    |> Set.toList

let specialConsonants = [ "ch"; "qu"; "sch"; "thr"; "th"; "rh" ]
let xquConsonants = trueConsonants |> List.map (fun c -> c + "qu")
let consonants = specialConsonants @ xquConsonants @ trueConsonants

let (|StartsWith|_|) (values : string list) (word : string) =
    values
    |> List.tryFind word.StartsWith
    |> Option.map (fun c -> c, word.[c.Length..])

let translate (phrase : string) =
    phrase.Split [| ' ' |]
    |> Array.map (fun word ->
        match word with
        | StartsWith vowels _ -> word + "ay"
        | StartsWith consonants (c, rest) -> rest + c + "ay"
        | _ -> failwith (sprintf "Invalid start of word: %c" word.[0]))
    |> String.concat " "
