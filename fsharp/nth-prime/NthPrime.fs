module NthPrime

let isPrime existingPrimes n =
    existingPrimes
    |> List.filter (fun x -> x <= (n |> float |> sqrt |> int))
    |> List.exists (fun x -> n % x = 0)
    |> not

let rec findPrimes targetCount existingPrimes next =
    if List.length existingPrimes = targetCount
    then existingPrimes
    else
        let existingPrimes =
            if next |> isPrime existingPrimes
            then existingPrimes @ [ next ]
            else existingPrimes

        findPrimes targetCount existingPrimes (next + 1)

let prime nth : int option =
    if nth = 0
    then None
    else
        findPrimes nth [ 2 ] 3
        |> List.last
        |> Some
