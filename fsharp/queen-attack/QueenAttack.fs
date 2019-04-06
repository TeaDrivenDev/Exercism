module QueenAttack

let create (row : int, column : int) =
    row >= 0 && column >= 0 && row < 8 && column < 8

let canAttack ((row1, column1) as queen1) ((row2, column2) as queen2) =
    create queen1 && create queen2
    && (row1 = row2
        || column1 = column2
        || row1 + column1 = row2 + column2
        || row1 - column1 = row2 - column2)