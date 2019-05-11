module PhoneNumber

open System
open System.Text.RegularExpressions

let checkCharacters input =
    if input |> String.exists (fun c -> [ '!'; ':'; '@' ] |> List.contains c)
    then Error "punctuations not permitted"
    elif input |> String.exists Char.IsLetter
    then Error "alphanumerics not permitted"
    else Ok input

let checkLength input =
    match String.length input with
    | 9 -> Error "incorrect number of digits"
    | 10 | 11 -> Ok input
    | _ -> Error "more than 11 digits"

let checkCountryCode input =
    if String.length input = 11 && not <| input.StartsWith "1"
    then Error "11 digits must start with 1"
    else Regex.Match(input, @"\d{10}$").Value |> Ok

let checkSubCodes input =
    let m = Regex.Match(input, @"(?<area>\d{3})(?<exchange>\d{3})\d{4}$")

    let area = m.Groups.["area"].Value
    let exchange = m.Groups.["exchange"].Value

    if area.StartsWith '0'
    then Error "area code cannot start with zero"
    elif area.StartsWith '1'
    then Error "area code cannot start with one"
    elif exchange.StartsWith '0'
    then Error "exchange code cannot start with zero"
    elif exchange.StartsWith '1'
    then Error "exchange code cannot start with one"
    else Ok input

let clean input : Result<uint64, string> =
    input
    |> checkCharacters
    |> Result.map (String.filter Char.IsDigit)
    |> Result.bind checkLength
    |> Result.bind checkCountryCode
    |> Result.bind checkSubCodes
    |> Result.map UInt64.Parse
