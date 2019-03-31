module Anagram

let sorted word = word |> Seq.sortBy id |> Seq.toList

let (|ToLower|) (word : string) = word.ToLower()

let findAnagrams candidates (ToLower word) =
    let sortedWord = sorted word

    candidates
    |> Seq.filter (fun (ToLower c) -> sorted c = sortedWord && c <> word)
    |> Seq.toList