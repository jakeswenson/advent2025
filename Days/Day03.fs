module Days.Day03


open System
open System.IO
open Common


type Joltage = Joltage of int
type Battery = Battery of Joltage list

let parseProblem (s: string) : Battery list =
  s.Split('\n', StringSplitOptions.RemoveEmptyEntries)
  |> Array.map _.Trim()
  |> Seq.filter (String.IsNullOrWhiteSpace >> not)
  |> Seq.map (fun s -> Seq.map Joltage (s |> Seq.map string |> Seq.map int) |> (Seq.toList>>Battery))
  |> Seq.toList

let input () =
  File.ReadAllText("inputs/day03.txt").Trim()
  |> parseProblem

let sample () =
  """
987654321111111
811111111111119
234234234234278
818181911112111
  """ |> parseProblem

let joltage= Array.fold (fun acc j -> acc * 10L + j) 0L

let activate (bank: int64 array) remainingCells jolt  =
  let hasCapacity (idx: int) : bool = remainingCells >= bank.Length - idx - 1

  let rec loop idx =
    if idx >= bank.Length then bank
    else if hasCapacity idx && bank.[idx] < jolt
    then
      bank.[idx] <- jolt
      bank.[idx+1..] <- Array.zeroCreate (bank.Length - idx - 1)
      bank
    else loop (idx + 1)

  loop 0

let turnOn (bank: unit -> int64 array) (Battery jolts) =
  let length = List.length jolts

  jolts
  |> Seq.mapi (fun i j -> (i, j))
  |> Seq.fold (fun jolts (idx, Joltage j) ->
      activate jolts (length - idx - 1) j
  ) (bank ())
  |> joltage

let part1 () =
  let bank () = Array.zeroCreate 2
  input ()
  |> Seq.map (turnOn bank)
  // |> Seq.inspect (fun j -> printfn $"%d{j}")
  |> Seq.sum
  |> string

let part2 () =
  let bank () = Array.zeroCreate 12
  input ()
  |> Seq.map (turnOn bank)
  // |> Seq.map (fun j -> printfn $"%d{j}"; j)
  |> Seq.sum
  |> string


let day03 = problem 3 {
  return PartOne part1
  return PartTwo part2
}

