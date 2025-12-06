module Days.Day06


open System
open System.IO
open Common
open System.Collections.Generic
open Days


type NumberColumn = int64 array
type Operations = int64 -> int64 -> int64

type Sheet =
  { Rows: NumberColumn array; Operations: Operations array }

let parseProblem (s: string) : Sheet =
  let lines = s.Split('\n', StringSplitOptions.RemoveEmptyEntries)
  let ops =
      lines
      |> Seq.last
      |> _.Split(' ', StringSplitOptions.RemoveEmptyEntries)
      |> Array.map (function "+" -> (+) | "*" -> (*))
  let rows =
    lines
    |> Seq.take (lines.Length - 1)
    |> Seq.map (fun l -> l.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Array.map int64)
    |> Array.ofSeq

  { Rows = rows; Operations = ops}

let parseProblem2 (s: string) : Sheet =
    let lines = s.Split('\n', StringSplitOptions.RemoveEmptyEntries)
    let width = lines |> Array.map _.Length |> Array.max
    let paddedLines = lines |> Array.map (fun l -> l.PadRight(width))
    let numDataRows = lines.Length - 1

    // Fold returns list of (numbers, op, isAdd) tuples
    let (_, problems) =
        [| width - 1 .. -1 .. 0 |]
        |> Array.fold (fun (currentNums, probs) col ->
            let opChar = paddedLines.[numDataRows].[col]
            let digits =
                [| for row in 0 .. numDataRows - 1 -> paddedLines.[row].[col] |]
                |> Array.filter System.Char.IsDigit

            match opChar, digits.Length > 0 with
            | ('+' | '*'), true ->
                let num = digits |> System.String |> int64
                let nums = num :: currentNums |> List.rev |> Array.ofList
                let op = if opChar = '+' then (+) else (*)
                let isAdd = (opChar = '+')
                ([], (nums, op, isAdd) :: probs)
            | ('+' | '*'), false ->
                let op = if opChar = '+' then (+) else (*)
                let isAdd = (opChar = '+')
                let nums = currentNums |> List.rev |> Array.ofList
                ([], (nums, op, isAdd) :: probs)
            | _, true ->
                let num = digits |> System.String |> int64
                (num :: currentNums, probs)
            | _ ->
                (currentNums, probs)
        ) ([], [])

    let problemArrays = problems |> List.rev |> Array.ofList
    let numProblems = problemArrays.Length
    let maxOperands = problemArrays |> Array.map (fun (nums, _, _) -> nums.Length) |> Array.max

    let rows =
        [| for i in 0 .. maxOperands - 1 ->
             [| for p in 0 .. numProblems - 1 ->
                  let (nums, _, isAdd) = problemArrays.[p]
                  if i < nums.Length then nums.[i]
                  else if isAdd then 0L else 1L |] |]

    let ops = problemArrays |> Array.map (fun (_, op, _) -> op)

    { Rows = rows; Operations = ops }

let input () =
  File.ReadAllText("inputs/day06.txt").Trim()

let sample () =
  """
123 328  51 64
 45 64  387 23
  6 98  215 314
*   +   *   +
  """.Trim()

module Day =
  let source = input ()

let solve (sheet: Sheet) =
  let applyOp problemIdx (op: Operations) =
    sheet.Rows
    |> Seq.map (fun row -> row.[problemIdx])
    |> Seq.reduce op
  sheet.Operations
  |> Seq.mapi applyOp

let part1 () =
  Day.source
  |> parseProblem
  |> solve
  |> Seq.sum
  |> string

let part2 () =
  Day.source
  |> parseProblem2
  |> solve
  |> Seq.sum
  |> string

let day06 = problem 5 {
  return PartOne part1
  return PartTwo part2
}

