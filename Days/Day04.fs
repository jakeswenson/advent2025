
module Days.Day04


open System
open System.IO
open Common


type Cell = PaperRoll | Empty | Removed
type Cells = Cell array array
type Room = Room of Cells
  with
  member x.Cells = match x with Room r -> r

let parseProblem (s: string) : Room =
  let parseRow (row: string) =
    // printfn $"{row}"
    row.ToCharArray() |> Array.map (function '.' -> Empty | '@' -> PaperRoll)

  s.Trim().Split('\n', StringSplitOptions.RemoveEmptyEntries)
  |> Array.map _.Trim()
  |> Seq.filter (String.IsNullOrWhiteSpace >> not)
  |> Seq.map parseRow
  |> Seq.toArray
  |> Room

let input () =
  File.ReadAllText("inputs/day04.txt").Trim()
  |> parseProblem

let sample () =
  """
..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.
  """.Trim() |> parseProblem


let countPaperNeighbours (rows: Cells) (x, y) =
  seq {
    for dx in -1 .. 1 do
      for dy in -1 .. 1 do
        let (x,y) = (x + dx, y + dy)
        if x >= 0 && y >= 0 && x < rows.[0].Length && y < rows.Length then
          yield (x,y)
  }
  |> Seq.map (fun (x,y) -> rows.[y].[x])
  |> Seq.filter _.IsPaperRoll
  |> Seq.length

let markCells (rows: Cells) =
  seq {
    for y in 0 .. rows.Length - 1 do
      for x in 0 .. rows.[0].Length - 1 do
        if rows.[y].[x].IsPaperRoll then
          yield (x,y), countPaperNeighbours rows (x,y)
  }

let accessiblePaperRolls room =
  room
  |> markCells
  |> Seq.filter (snd >> (>=) 4)

let printRoom (cells: Cells) =
  seq {
    for row in cells do
      row
      |> Array.map (function PaperRoll -> '@' | Empty -> '.' | Removed -> 'X')
      |> fun s -> String.Join("", s)
  } |> fun s -> String.Join("\n", s)
  |> fun s -> printfn $"\n%s{s}\nAccessible: %d{accessiblePaperRolls cells |> Seq.length}\n"

let cleanAccessiblePaperRolls (room: Room): int =
  let cells = room.Cells
  printRoom cells
  seq {
    while accessiblePaperRolls cells |> (not << Seq.isEmpty) do
      let currentCells = Array.init room.Cells.Length (fun i -> Array.copy cells.[i])
      yield accessiblePaperRolls currentCells |> Seq.length
      for ((x,y), _) in accessiblePaperRolls currentCells do
        cells.[y].[x] <- Removed
      // printRoom cells
  }
  |> Seq.sum

let part1 () =
  let data = input
  data ()
  |> _.Cells
  // |> fun s -> printRoom s; s
  |> accessiblePaperRolls
  // |> Seq.inspect (fun j -> printfn $"%A{j}")
  |> Seq.length
  |> string

let part2 () =
  let data = input
  data ()
  |> cleanAccessiblePaperRolls
  |> string



let day04 = problem 4 {
  return PartOne part1
  return PartTwo part2
}

