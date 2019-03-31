module Triangle

let canExist sides =
    sides
    |> List.sort
    |> fun [ x; y; z ] -> x + y > z

let equilateral ([ a; b; c ] as sides) =
    canExist sides && (a = b && a = c && a <> 0.)

let isosceles ([ a; b; c ] as sides) =
    canExist sides && (a = b || a = c || b = c)

let scalene ([ a; b; c ] as sides) =
    canExist sides && (a <> b && a <> c && b <> c)
