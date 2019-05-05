module AllYourBase

let isValidBase baseValue = baseValue >= 2

let isValidDigit baseValue digit = digit >= 0 && digit < baseValue

let convertFrom inputBase digits =
    digits
    |> List.rev
    |> List.mapi (fun index n -> 
        int (float inputBase ** float index) * n)
    |> List.sum

let convertTo outputBase value =
    let rec convertLoop outputBase digits remaining =
        if remaining > 0
        then
            let here = remaining % outputBase
            here :: convertLoop outputBase digits (remaining / outputBase)
        else digits

    if value = 0
    then [ 0 ]
    else convertLoop outputBase [] value
    |> List.rev

let rebase digits inputBase outputBase =
    if [ inputBase; outputBase ] |> List.forall isValidBase
        && digits |> List.forall (isValidDigit inputBase)
    then
        digits
        |> convertFrom inputBase
        |> convertTo outputBase
        |> Some
    else None

