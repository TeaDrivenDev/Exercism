module RobotName

let random = System.Random()

type Robot = { Name : string }

let createRandomName () =
    [
        yield! [ 1..2 ]
               |> List.map (fun _ -> random.Next(65, 90) |> char |> string)
        yield! [ 1..3 ]
               |> List.map (fun _ -> random.Next(0, 9) |> string)
    ]
    |> String.concat ""

let mutable existingNames = Set.empty<string>

let createUniqueRandomName () =
    let rec innerLoop existingNames =
        let name = createRandomName ()

        if existingNames |> Set.contains name
        then innerLoop existingNames
        else name, existingNames |> Set.add name

    let name, newExistingNames = innerLoop existingNames
    existingNames <- newExistingNames

    name

let name robot = robot.Name

let mkRobot () = { Name = createUniqueRandomName () }

let reset robot =
    let newName = createUniqueRandomName ()

    existingNames <- existingNames |> Set.remove robot.Name

    { robot with Name = newName }