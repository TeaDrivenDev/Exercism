module Clock

open System

type Clock = { Hours : int; Minutes : int }

let normalizeComponent ``base`` value =
    if value >= 0
    then value % ``base``, value / ``base``
    else
        let carry = Math.Abs(value / ``base``) + 1
        let normalizedValue = value + carry * ``base``

        if normalizedValue = ``base``
        then 0, carry - 1
        else normalizedValue, carry

let normalizeTime hours minutes =
    let newMinutes, carry =
        minutes
        |> normalizeComponent 60
    let newHours, _ =
        (hours + carry * Math.Sign minutes)
        |> normalizeComponent 24

    newHours, newMinutes

let add minutesToAdd clock =
    let hours, minutes =
        normalizeTime clock.Hours (clock.Minutes + minutesToAdd)

    { Hours = hours; Minutes = minutes }

let subtract minutesToSubtract clock = add (-minutesToSubtract) clock

let create hours minutes =
    { Hours = 0; Minutes = 0 }
    |> add (hours * 60 + minutes)

let display clock = sprintf "%02i:%02i" clock.Hours clock.Minutes
