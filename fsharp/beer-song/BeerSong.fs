module BeerSong

open System

let bottles count =
    if count = 1
    then "1 bottle"
    else (if count = 0
          then "no more"
          else string count
          |> sprintf "%s bottles")

let capitalize (s : string) =
    if String.IsNullOrWhiteSpace s
    then s
    else (Char.ToUpper s.[0] |> string)
         + (if s.Length > 1 then s.Substring 1 else "")

let defaultFirst count =
    let bottles = bottles count

    sprintf "%s of beer on the wall, %s of beer." (capitalize bottles) bottles

let defaultSecond count =
    let one = if count > 0 then "one" else "it"

    sprintf "Take %s down and pass it around, %s of beer on the wall." one (bottles count)

let oneFirst = "1 bottle of beer on the wall, 1 bottle of beer."
let noneSecond = "Go to the store and buy some more, 99 bottles of beer on the wall."

let verse current =
    match current with
    | 0 -> [ defaultFirst 0; noneSecond ]
    | 1 -> [ oneFirst; defaultSecond 0 ]
    | _ -> [ defaultFirst current; defaultSecond (current - 1) ]

let recite start count =
    let rec pad verses =
        match verses with
        | verse :: [] -> verse
        | verse :: tail -> verse @ [ "" ] @ pad tail
        | [] -> []

    List.init count (fun index -> start - index)
    |> List.map verse
    |> pad
