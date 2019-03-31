open System
open System.IO
open System.Text.RegularExpressions

type Project =
    {
        Name : string
        Path : string
        Guid : Guid
        Content : string
    }

type Line =
    | Data of string
    | Project of Project
    | EndProject
    | SolutionItems of string
    | ProjectConfiguration of string

let (|RegexMatch|_|) (regex : Regex) line =
    let m = regex.Match line
    if m.Success then Some m else None

// from https://stackoverflow.com/a/14772471/236507
let projectStringRegex =
    Regex "Project\\(\"\\{[\\w-]*\\}\"\\) = \"(?<name>[\\w _]*.*)\", \"(?<path>.*)\", \"\\{(?<guid>[\\w-]*)\\}\""

let configurationStringRegex = Regex "^\\{(?<guid>[\\w-]*)\\}\\..*"

let readLine (line : string) =
    match line.Trim() with
    | RegexMatch projectStringRegex m ->
        match m.Groups.["path"].Value with
        | "Solution Items" -> SolutionItems line
        | _ ->
            {
                Name = m.Groups.["name"].Value
                Path = m.Groups.["path"].Value
                Guid = Guid.Parse m.Groups.["guid"].Value
                Content = line
            }
            |> Project
    | RegexMatch configurationStringRegex _ -> ProjectConfiguration line
    | "EndProject" -> EndProject
    | _ -> Data line

// from https://weblog.west-wind.com/posts/2010/Dec/20/Finding-a-Relative-Path-in-NET
let getRelativePath (basePath : string) (fullPath : string) =
    let basePath =
        if not <| basePath.EndsWith("\\")
        then basePath + "\\"
        else basePath

    let baseUri = Uri basePath
    let fullUri = Uri fullPath

    let relativeUri = baseUri.MakeRelativeUri fullUri

    relativeUri.ToString().Replace("/", "\\")

let getProjectFiles baseDirectory =
    Directory.GetFiles(baseDirectory, "*.fsproj", SearchOption.AllDirectories)
    |> Array.map (fun file -> Path.GetFileNameWithoutExtension file, getRelativePath baseDirectory file)
    |> Array.toList

let projectsIn lines =
    lines
    |> List.choose (function
        | Project project -> Some project
        | _ -> None)
    |> List.map (fun project -> project.Path)

let readSolutionFile filePath =
    File.ReadAllLines filePath
    |> Array.map readLine
    |> Array.toList

let createProject (name, path) =
    let guid = Guid.NewGuid()

    {
        Name = name
        Path = path
        Guid = guid
        Content =
            sprintf "Project(\"{6EC3EE1D-3C4E-46DD-8F32-0CC8E7565705}\") = \"%s\", \"%s\", \"%s\"" name path (guid.ToString("B").ToUpper())
    }

let createNewSolutionLines lines (newProjects : Project list) =
    let rec recurse previousLine lines =
        match lines with
        | (SolutionItems content) as line :: tail ->
            (newProjects
            |> List.collect (fun project -> [ project.Content; "EndProject" ]))
            @ content :: recurse line tail
        | (Data data) as line :: tail ->
            match previousLine with
            | ProjectConfiguration _ ->
                (newProjects
                |> List.collect (fun project ->
                    let guid = project.Guid.ToString("B").ToUpper()

                    [
                        ".Debug|Any CPU.ActiveCfg = Debug|Any CPU"
                        ".Debug|Any CPU.Build.0 = Debug|Any CPU"
                        ".Release|Any CPU.ActiveCfg = Release|Any CPU"
                        ".Release|Any CPU.Build.0 = Release|Any CPU"
                    ]
                    |> List.map (sprintf "\t\t%s%s" guid)))
                @ recurse line tail
            | line -> data :: recurse line tail
        | (Project project) as line :: tail ->
            project.Content :: recurse line tail
        | EndProject as line :: tail ->
            "EndProject" :: recurse line tail
        | (ProjectConfiguration configuration) as line :: tail ->
            configuration :: recurse line tail
        | [] -> []

    recurse (Data "") lines

let update solutionFilePath =
    let basePath = Path.GetDirectoryName solutionFilePath

    let lines = readSolutionFile solutionFilePath

    let projects = projectsIn lines
    let projectFiles = getProjectFiles basePath

    let newProjects =
        projectFiles
        |> List.filter (fun (_, path) -> not <| List.exists ((=) path) projects)
        |> List.map createProject

    match newProjects with
    | [] -> ()
    | _ ->
        let newLines =
            createNewSolutionLines lines newProjects
            |> List.filter (not << String.IsNullOrWhiteSpace)

        File.WriteAllLines(solutionFilePath, newLines)

// ----------------------------------------

//let projectString = "Project(\"{6EC3EE1D-3C4E-46DD-8F32-0CC8E7565705}\") = \"Transpose\", \"transpose\\Transpose.fsproj\", \"{BECAEA10-A36B-478B-A4DA-D51798B359BD}\""

//let configurationString = "{BECAEA10-A36B-478B-A4DA-D51798B359BD}.Debug|Any CPU.ActiveCfg = Debug|Any CPU"

//let solutionItemsString = "Project(\"{2150E333-8FDC-42A3-9474-1A3956D46DE8}\") = \"Solution Items\", \"Solution Items\", \"{CF060CC8-C54D-4962-B3A5-B6CC069B5F18}\""

let solutionFilePath = Path.Combine(__SOURCE_DIRECTORY__, "ExercismFsharp.sln")

update solutionFilePath
