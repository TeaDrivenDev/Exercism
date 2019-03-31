module DifferenceOfSquares

let square x = x * x

let squareOfSum x = [ 1 .. x ] |> List.sum |> square

let sumOfSquares x = [ 1 .. x ] |> List.sumBy square

let differenceOfSquares x = squareOfSum x - sumOfSquares x