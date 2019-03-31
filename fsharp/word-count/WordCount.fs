module WordCount

open System.Text.RegularExpressions

let replacePunctuation (s : string) =
    Regex.Replace(s, @"[^\w']", " ")

let replaceQuotingApostrophes (s : string) =
    Regex.Replace(s, @"^'|'$", "")

let isWord (s : string) =
    Regex.IsMatch(s, @"[\w]")

let toLower (s : string) = s.ToLower()

let countWords (phrase : string) =
    (phrase |> replacePunctuation).Split [| ' ' |]
    |> Array.filter isWord
    |> Array.groupBy (replaceQuotingApostrophes >> toLower)
    |> Array.map (fun (word, words) -> word, words.Length)
    |> Map.ofArray