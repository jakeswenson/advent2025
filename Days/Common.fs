namespace Common

open System
open System.Collections.Generic

module Seq =
  let inspect f  = Seq.map (fun x -> f x; x)



type ProblemPart =
  | PartOne of (unit -> string)
  | PartTwo of (unit -> string)

type ProblemBuilder(day: int, part1: string option, part2: string option) =
  member x.PartOne = part1 |> Option.defaultValue "Not solved yet."
  member x.PartTwo = part2 |> Option.defaultValue "Not solved yet."

  member x.Return(value) =
    match value with
    | PartOne f -> ProblemBuilder(day, Some(f ()), part2)
    | PartTwo f -> ProblemBuilder(day, part1, Some(f ()))

  member x.Combine(p: ProblemBuilder, b: ProblemBuilder) =
    ProblemBuilder(day, Some(p.PartOne), Some(b.PartTwo))

  member x.Delay(f) = f()

  member x.Answer() =
    printfn $"Day %d{day}:"
    printfn $"  Part 1: %s{x.PartOne}"
    printfn $"  Part 2: %s{x.PartTwo}"

  member x.Run(f): ProblemBuilder = f


[<AutoOpen>]
module Problem =
  let foo: int8  = 0b11111111y
  let foor: int8 = 0b00000011y


  let problem (day: int) = ProblemBuilder(day, None, None)
