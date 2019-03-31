module Etl

open System
open System.Collections.Generic

let asFst second first = first, second

let (|KeyValuePair|) (kvp : KeyValuePair<_, _>) = kvp.Key, kvp.Value

let transform (old : Map<_, _>) =
    old
    |> Seq.collect (fun (KeyValuePair (points, letters)) ->
        letters |> List.map (Char.ToLower >> asFst points))
    |> Map.ofSeq