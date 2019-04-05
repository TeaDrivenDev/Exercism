module BeerSong

open System

let bottles count =
    if count = 1
    then "1 bottle"
    else
        match count with
        | 0 -> "no more"
        | _ -> string count
        |> sprintf "%s bottles"

let capitalize (s : string) =
    if String.IsNullOrWhiteSpace s
    then s
    else (Char.ToUpper s.[0] |> string) + s.Substring 1

let defaultFirst count =
    let bottles = bottles count

    sprintf "%s of beer on the wall, %s of beer." (capitalize bottles) bottles

let defaultSecond count =
    let one = if count > 0 then "one" else "it"

    sprintf "Take %s down and pass it around, %s of beer on the wall." one (bottles count)

let noneSecond = "Go to the store and buy some more, 99 bottles of beer on the wall."

let verse current =
    match current with
    | 0 -> [ defaultFirst 0; noneSecond ]
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
