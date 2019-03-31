module Proverb

let final word = sprintf "And all for the want of a %s." word

let line first second = sprintf "For want of a %s the %s was lost." first second

let recite words =
    match words with
    | [] -> []
    | [ word ] -> [ final word ]
    | head :: _ ->
        let pairs = words |> List.windowed 2 |> List.collect List.pairwise

        [
            yield! pairs |> List.map (fun pair -> pair ||> line)

            yield final head
        ]
