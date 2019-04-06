module OcrNumbers

let numbers =
    [
        [ " _ "
          "| |"
          "|_|"
          "   " ], 0
        [ "   "
          "  |"
          "  |"
          "   " ], 1
        [ " _ "
          " _|"
          "|_ "
          "   " ], 2
        [ " _ "
          " _|"
          " _|"
          "   " ], 3
        [ "   "
          "|_|"
          "  |"
          "   " ], 4
        [ " _ "
          "|_ "
          " _|"
          "   " ], 5
        [ " _ "
          "|_ "
          "|_|"
          "   " ], 6
        [ " _ "
          "  |"
          "  |"
          "   " ], 7
        [ " _ "
          "|_|"
          "|_|"
          "   " ], 8
        [ " _ "
          "|_|"
          " _|"
          "   " ], 9
    ]
    |> Map.ofList

let recognizeDigit input =
    numbers.TryFind input
    |> Option.map string
    |> Option.defaultValue "?"

let chunkLine (input : string list) : string list list =
    let [ line1; line2; line3; line4 ] =
        input
        |> List.map
            (Seq.chunkBySize 3
             >> Seq.map (Array.map string >> String.concat "")
             >> Seq.toList)

    ((List.zip line1 line2), (List.zip line3 line4))
    ||> List.zip
    |> List.map (fun ((l1, l2), (l3, l4)) -> [ l1; l2; l3; l4])

let convert (input : string list) =
    if List.length input % 4 <> 0
       || input |> List.exists (fun s -> s.Length % 3 <> 0)
    then None
    else
        input
        |> List.chunkBySize 4
        |> List.map (chunkLine
                     >> List.map recognizeDigit
                     >> String.concat "")
        |> String.concat ","
        |> Some
