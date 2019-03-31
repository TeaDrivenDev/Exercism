module Bob

let containsLetters (input : string) =
    input |> Seq.exists System.Char.IsLetter

let (|Statement|ForcefulQuestion|Shouting|Question|Silence|) (input : string) =
    if System.String.IsNullOrWhiteSpace input then Silence
    elif input = input.ToUpper() && containsLetters input
    then if input.EndsWith "?" then ForcefulQuestion else Shouting
    elif input.EndsWith "?" then Question
    else Statement

let response (input : string) =
    match input.Trim() with
    | Statement -> "Whatever."
    | Question -> "Sure."
    | Shouting -> "Whoa, chill out!"
    | ForcefulQuestion -> "Calm down, I know what I'm doing!"
    | Silence -> "Fine. Be that way!"
