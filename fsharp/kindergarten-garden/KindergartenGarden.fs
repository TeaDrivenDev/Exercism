module KindergartenGarden

type Plant =
    | Clover
    | Grass
    | Radishes
    | Violets

let plants (diagram : string) (student : string) =
    let garden =
        let [| row1; row2 |] =
            diagram.Split([| '\n' |])
            |> Array.map (Seq.chunkBySize 2)

        (row1, row2)
        ||> Seq.zip
        |> Seq.map (fun (first, second) ->
            Array.append first second
            |> Array.toList
            |> List.map (function
                | 'C' -> Clover
                | 'G' -> Grass
                | 'R' -> Radishes
                | 'V' -> Violets
                | _ -> failwith "Unknown plant"))
        |> Seq.toList

    garden |> List.item (int student.[0] - int 'A')
