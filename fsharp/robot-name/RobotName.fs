module RobotName

let random = System.Random()

let createRandomName () =
    [
        yield! [ 1..2 ]
               |> List.map (fun _ -> random.Next(65, 90) |> char |> string)
        yield! [ 1..3 ]
               |> List.map (fun _ -> random.Next(0, 9) |> string)
    ]
    |> String.concat ""

type Robot = { Name : string }

let name robot = robot.Name

let mkRobot () = { Name = createRandomName() }

let reset robot = { robot with Name = createRandomName () }