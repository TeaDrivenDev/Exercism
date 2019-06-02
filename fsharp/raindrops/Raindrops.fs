module Raindrops

let sounds =
    [
        3, "Pling"
        5, "Plang"
        7, "Plong"
    ]
    |> Map.ofList

let sound number =
    sounds |> Map.tryFind number |> Option.defaultValue ""

let convert number =
    [ 1 .. number ]
    |> List.choose (fun x ->
        if number % x = 0 then sound x |> Some else None)
    |> String.concat ""
    |> function
        | "" -> string number
        | s -> s
