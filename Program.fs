open System

let runDay day =
    match day with
    | 1 ->
        printfn "Day 01:"
        printfn "  Part 1: %s" (Days.Day01.part1 ())
        printfn "  Part 2: %s" (Days.Day01.part2 ())
    | 2 ->
        printfn "Day 02:"
        printfn "  Part 1: %s" (Days.Day02.part1 ())
        printfn "  Part 2: %s" (Days.Day02.part2 ())
    | 3 -> Days.Day03.day03.Answer()
    | 4 -> Days.Day04.day04.Answer()
    | _ ->
        printfn "Day %d not implemented" day

[<EntryPoint>]
let main args =
    match args with
    | [| dayStr |] ->
        match Int32.TryParse(dayStr) with
        | true, day -> runDay day
        | _ -> printfn "Usage: dotnet run <day>"
    | _ ->
        printfn "Advent of Code 2025"
        printfn "Usage: dotnet run <day>"
        printfn ""
        printfn "Available days: 1"
    0
