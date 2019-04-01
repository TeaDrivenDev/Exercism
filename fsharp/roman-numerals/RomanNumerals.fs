module RomanNumerals

let glyphs =
    [ 1, "I"; 5, "V"; 10, "X"; 50, "L"; 100, "C"; 500, "D"; 1000, "M" ]
    |> Map.ofList

let rec translate power digit =
    match digit with
    | 0 -> ""
    | 1 | 2 | 3 -> String.replicate digit glyphs.[power]
    | 4 -> translate power 1 + translate power 5
    | 5 -> glyphs.[power * 5]
    | 6 | 7 | 8 -> translate power 5 + translate power (digit - 5)
    | 9 -> translate power 1 + translate (power * 10) 1
    | _ -> failwith "Not a single digit"

let roman arabic =
    let rec collect power remaining =
        let digit = remaining % 10

        translate power digit
        :: if (remaining > 0)
           then collect (power * 10) (remaining / 10)
           else []

    collect 1 arabic
    |> List.rev
    |> String.concat ""