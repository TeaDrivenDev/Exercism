module AllYourBase

open System

let rebase digits inputBase outputBase =
    let value =
        digits
        |> List.rev
        |> List.mapi (fun index digit ->
            float inputBase ** float index * float digit)
        |> List.sum

    let xx =
        [ Math.Log(value, float outputBase) |> Math.Floor |> int .. 0 ]

Math.Log(16., 10.)