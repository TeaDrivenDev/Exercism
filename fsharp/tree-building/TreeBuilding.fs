module TreeBuilding

type Record = { RecordId: int; ParentId: int }
type Tree =
    | Branch of int * Tree list
    | Leaf of int

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

    if List.isEmpty records then failwith "Empty input"
    else
        let root = records.[0]
        if root.ParentId <> 0 || root.RecordId <> 0
        then failwith "Root node is invalid"
        else
            let leaves = traverse -1 records

            let mapByParentId =
                leaves
                |> List.groupBy fst
                |> List.map (fun (parentId, children) ->
                    parentId, List.map snd children)
                |> Map.ofList

            let rec helper key =
                mapByParentId
                |> Map.tryFind key
                |> Option.map (fun node ->
                    Branch (key, List.map (fun i -> helper i) node))
                |> Option.defaultValue (Leaf key)

            let root = helper 0
            root
