module RnaTranscription

let map = [ 'G', 'C'; 'C', 'G'; 'T', 'A'; 'A', 'U' ] |> Map.ofList

let toRna dna =
    dna
    |> Seq.map (fun c -> map.[c])
    |> System.String.Concat