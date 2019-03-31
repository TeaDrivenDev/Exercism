module Leap

let inline (<&&>) f g x = f x && g x
let inline (<||>) f g x = f x || g x

let isDivisibleBy divisor dividend = dividend % divisor = 0

let leapYear =
    isDivisibleBy 4
    <&&> (not << isDivisibleBy 100 <||> isDivisibleBy 400)