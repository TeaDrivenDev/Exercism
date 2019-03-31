module GradeSchool

type School =
    {
        Roster : (string * int) list
    }

let empty = { Roster = [] }

let roster school =
    school.Roster |> List.map fst

let grade gr school =
    school.Roster
    |> List.filter (snd >> (=) gr)
    |> List.map fst

let add name grade school =
    {
        Roster =
            (name, grade) :: school.Roster
            |> List.groupBy snd
            |> List.sortBy fst
            |> List.collect (snd >> List.sortBy fst)
    }