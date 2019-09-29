module TreeBuildingFastest

open TreeBuildingTypes

// Previously ScottHutchFP: https://exercism.io/tracks/fsharp/exercises/tree-building/solutions/8214df6fb8d6405c8f58f8d18645375a
// Now my own

let recordId t =
    match t with
    | Branch (id, c) -> id
    | Leaf id -> id

let isBranch t =
    match t with
    | Branch (id, c) -> true
    | Leaf id -> false

let children t =
    match t with
    | Branch (id, c) -> c
    | Leaf id -> []

let rec traverse previousId records =
    match records with
    | head :: tail ->
        if head.RecordId <> 0
           && (head.ParentId > head.RecordId
               || head.ParentId = head.RecordId)
        then failwith "Nodes with invalid parents"
        else
            if head.RecordId <> previousId + 1
            then failwith "Non-continuous list"
            else
                if head.RecordId = 0
                then -1, head.RecordId
                else head.ParentId, head.RecordId
                :: traverse head.RecordId tail
    | [] -> []

let buildTree records =
    let records = List.sortBy (fun x -> x.RecordId) records

    match records with
    | [] -> failwith "Empty input"
    | head :: tail ->
        if head.ParentId <> 0 || head.RecordId <> 0
        then failwith "Root node is invalid"
        else
            let leaves = traverse 0 tail

            let rec buildSubtree key =
                leaves
                |> List.filter (fst >> (=) key)
                |> function
                    | [] -> Leaf key
                    | children ->
                        Branch (key, children |> List.map (snd >> buildSubtree))

            let root = buildSubtree 0
            root
