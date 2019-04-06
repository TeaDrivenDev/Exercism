module RobotSimulator

type Direction = North | East | South | West
type Position = int * int
type Robot = { direction: Direction; position: Position }

let advance { direction = direction; position = (x, y) } =
    match direction with
    | North -> x, y + 1
    | East -> x + 1, y
    | South -> x, y - 1
    | West -> x - 1, y

let rotate oldDirection turn =
    [ North; East; South; West; North ]
    |> List.pairwise
    |> List.pick (fun pair ->
        match turn, pair with
        | 'L', (l, r) when r = oldDirection -> Some l
        | 'R', (l, r) when l = oldDirection -> Some r
        | _ -> None)

let create direction position =
    { direction = direction; position = position }

let move instructions robot =
    let processInstruction robot instruction =
        match instruction with
        | 'A' -> { robot with position = advance robot }
        | 'L' | 'R' ->
            { robot with
                direction = rotate robot.direction instruction }
        | _ -> failwith "Invalid instruction"

    (robot, instructions)
    ||> Seq.fold (fun acc instruction ->
        processInstruction acc instruction)
