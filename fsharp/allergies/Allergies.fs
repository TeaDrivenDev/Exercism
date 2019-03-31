module Allergies

[<System.Flags>]
type Allergen =
    | Eggs = 1
    | Peanuts = 2
    | Shellfish = 4
    | Strawberries = 8
    | Tomatoes = 16
    | Chocolate = 32
    | Pollen = 64
    | Cats = 128

let allergicTo (score : int) (allergen : Allergen) =
    (allergen &&& enum<Allergen> score) = allergen

let list score =
    System.Enum.GetValues(typeof<Allergen>)
    |> Seq.cast<Allergen>
    |> Seq.filter (allergicTo score)
    |> Seq.toList
