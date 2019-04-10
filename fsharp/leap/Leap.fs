module Leap

let (|IsDivisibleBy|_|) d n =
    if n % d = 0 then Some () else None

let (|NotDivisibleBy|_|) d n =
    if n % d <> 0 then Some () else None

let leapYear (year: int): bool =
    match year with
    | IsDivisibleBy 400 | IsDivisibleBy 4 & NotDivisibleBy 100 -> true
    | _ -> false