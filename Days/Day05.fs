module Days.Day05


open System
open System.IO
open Common
open System.Collections.Generic


type IngredientRange =
  { Start: int64; End: int64 }

type Problem =
  { Ranges: IngredientRange array; Ingredients: int64 list }

let parseProblem (s: string) : Problem =
  let [|ranges; ingredients|] =
    s.Split("\n\n", StringSplitOptions.RemoveEmptyEntries)

  let ranges =
    ranges.Split("\n")
    |> Seq.map (fun r -> r.Split("-")
                         |> fun [|a; b|] -> { Start = int64 a; End = int64 b } )
    |> Seq.toArray
    |> Array.sortBy (fun i -> (i.Start, i.End))

  printfn "Ranges Count %A" ranges.Length

  let ingredients =
    ingredients.Split("\n")
    |> Array.map (int64)
    |> Array.sort
    |> List.ofArray

  printfn "Ingredient Count %A" ingredients.Length

  { Ranges = ranges; Ingredients = ingredients }

let input () =
  File.ReadAllText("inputs/day05.txt").Trim()
  |> parseProblem

let sample () =
  """
3-5
10-14
16-20
12-18
3-3
5-5
6-7

1
5
8
11
17
32
  """.Trim() |> parseProblem

let collapseRanges (problem: Problem) =
  let collapsedRanges =
    // remove overlapping ranges
    problem.Ranges |> Array.fold (fun acc r ->
      match acc with
      | item :: tail when item.End >= r.Start - 1L -> {Start = item.Start; End = (max item.End r.End)} :: tail
      | ranges -> r :: ranges) []
    |> List.rev |> Array.ofList |> Array.sort

  printfn "Collapsed Ranges Count %A" collapsedRanges.Length

  { problem with Ranges = collapsedRanges }

let rangeComparer (a:IngredientRange) (b: IngredientRange)  =
  // if b is contained in a, return 0
  if b.Start >= a.Start && b.End <= a.End then 0
  else if a.Start.CompareTo(b.Start) = 0
  then a.End.CompareTo(b.End)
  else a.Start.CompareTo(b.Start)

module Day =
  let source = input
  let data = source ()

let part1 () =
  let comparer = Comparer.Create rangeComparer
  let ranges = Day.data |> collapseRanges |> _.Ranges
  Day.data.Ingredients
  |> Seq.filter (fun i ->
    match Array.BinarySearch(ranges, { Start = i; End = i }, comparer) with
    | x when x >= 0 ->
      let r = ranges.[x]
      ranges.[x].Start <= i && i <= ranges.[x].End
    | _ ->
      Array.tryFind (fun r -> i >= r.Start && i <= r.End) Day.data.Ranges
      |> function
          | Some(r) ->
            true
          | None ->
            false
  )
  |> Seq.length
  |> string

let part2 () =
  Day.data |> collapseRanges |> _.Ranges
  |> Seq.map (fun r -> r.End - r.Start + 1L)
  |> Seq.sum
  |> string

let day05 = problem 5 {
  return PartOne part1
  return PartTwo part2
}

