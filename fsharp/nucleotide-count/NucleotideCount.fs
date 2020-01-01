module NucleotideCount

type NucleotideCounts = { A: int; C: int; G: int; T: int }

let validNucleotideNames = [ 'A'; 'C'; 'G'; 'T' ] |> Set.ofList

let rec count acc items =
    match items with
    | head :: tail ->
        match head with
        | 'A' -> count { acc with A = acc.A + 1 } tail
        | 'C' -> count { acc with C = acc.C + 1 } tail
        | 'G' -> count { acc with G = acc.G + 1 } tail
        | 'T' -> count { acc with T = acc.T + 1 } tail
        | _ -> None
    | [] -> Some acc

let nucleotideCounts (strand: string): Option<Map<char, int>> =
    strand
    |> Seq.toList
    |> count { A = 0; C = 0; G = 0; T = 0 }
    |> Option.map (fun acc ->
        [ 'A', acc.A; 'C', acc.C; 'G', acc.G; 'T', acc.T]
        |> Map.ofList)
