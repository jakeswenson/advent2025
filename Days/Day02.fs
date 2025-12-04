module Days.Day02

open System
open System.IO

let input () = File.ReadAllText("inputs/day02.txt").Trim()

type IdRange = { From: int64; To: int64 }
type Problem = IdRange seq


let parseProblem (s: string) =
  s.Split(',', StringSplitOptions.RemoveEmptyEntries)
  |> Array.map _.Trim()
  |> Seq.filter (String.IsNullOrWhiteSpace >> not)
  |> Seq.map (fun s -> { From = int64 <| s.Split('-')[0]; To = int64 <| s.Split('-')[1] })

let sample =
  """
11-22,95-115,998-1012,1188511880-1188511890,222220-222224,
1698522-1698528,446443-446449,38593856-38593862,565653-565659,
824824821-824824827,2121212118-2121212124
  """ |> parseProblem

let invalidIdsInRange (start: int64) (stop: int64) =
    let maxHalfLen = (string stop).Length / 2 + 1

    seq {
        for k in 1 .. maxHalfLen do
            let multiplier = pown 10L k + 1L
            let minBase = if k = 1 then 1L else pown 10L (k - 1)  // 1, 10, 100...
            let maxBase = pown 10L k - 1L                          // 9, 99, 999...

            // Find bases where base * multiplier ∈ [start, stop]
            let lo = max minBase ((start + multiplier - 1L) / multiplier)
            let hi = min maxBase (stop / multiplier)

            for baseNum in lo .. hi do
                yield baseNum * multiplier
    }

let scan f = Seq.collect (fun r -> f r.From r.To)

let scanInvalid = scan invalidIdsInRange


let part1 () =
  input ()
  |> parseProblem
  |> scanInvalid
  |> Seq.sum
  |> string

/// Compute multiplier for k-digit base repeated r times
/// Formula: (10^(k*r) - 1) / (10^k - 1)
let multiplier k r =
    let pkr = pown 10L (k * r)
    let pk = pown 10L k
    (pkr - 1L) / (pk - 1L)


let invalidIdsInRangePart2 (start: int64) (stop: int64) =
    let maxDigits = (string stop).Length

    seq {
        // k = base pattern length (1, 2, 3, ...)
        for k in 1 .. maxDigits / 2 do
            // r = repetition count (at least 2)
            for r in 2 .. maxDigits / k do
                let mult = multiplier k r

                // Base must be k digits (no leading zeros)
                let minBase = if k = 1 then 1L else pown 10L (k - 1)
                let maxBase = pown 10L k - 1L

                // Find bases where base × mult ∈ [start, stop]
                let lo = max minBase ((start + mult - 1L) / mult)  // ceiling div
                let hi = min maxBase (stop / mult)

                for baseNum in lo .. hi do
                    yield baseNum * mult
    }
    |> Set.ofSeq  // ← Critical: deduplicate!

let part2 () =
  input ()
  |> parseProblem
  |> scan invalidIdsInRangePart2
  |> Seq.sum
  |> string
