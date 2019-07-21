module TreeBuildingTypes

type Record = { RecordId: int; ParentId: int }

type Tree =
    | Branch of int * Tree list
    | Leaf of int
