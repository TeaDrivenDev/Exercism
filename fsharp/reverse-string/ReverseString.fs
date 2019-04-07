module ReverseString

let reverse (input: string): string =
    ("", input)
    ||> Seq.fold (fun acc current -> string current + acc)
