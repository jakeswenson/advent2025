module Days.Day01

open System.IO

let input () = File.ReadAllLines("inputs/day01.txt")

let parseLines (s: string) = s.Split("\n") |> Array.map _.Trim() |> Array.filter (fun s -> s.Length > 0)

let sample =
  """
  L68
  L30
  R48
  L5
  R60
  L55
  L1
  L99
  R14
  L82
  """ |> parseLines


let ones = [
  """
  L50
  L50
  """ |> parseLines
]


let (|Left|Right|) (s: string) =
  let value = s.Substring(1) |> int
  match s.Chars 0 with
  | 'L' -> Left value
  | 'R' -> Right value
  | _ -> failwith "Invalid direction"

let modulo a b = ((a % b) + b) % b

let part1 () =
    let startState = 50
    let folder (acc, count) = function
        | Left i ->
            let result = modulo (acc - i) 100
            (result, if result = 0 then count + 1 else count)
        | Right i ->
            let result = modulo (acc + i) 100
            (result, if result = 0 then count + 1 else count)
    let (endAcc, zeros) =
        seq {
          let mutable acc = (startState, 0)
          for line in input () do
            let next = folder acc line
            // printfn "%A" next
            acc <- next

          yield acc
        } |> Seq.last

    zeros.ToString()


let rotate (pos, count) = function
    | Left i ->
        let result = modulo (pos - i) 100
        let totalDelta = pos - i
        let passedZero = if totalDelta < 0 then
                           let stepsToZero = if pos = 0 then 100 else pos
                           let total = if i >= stepsToZero then 1 + ((i - stepsToZero) / 100) else 0
                           if result = 0 then total - 1 else total
                         else 0
        let isAtZero = if result = 0 then count + 1 else count
        (result, isAtZero + passedZero)
    | Right i ->
        let result = modulo (pos + i) 100
        let passedZero =
            (if pos + i > 100 then
              let total = (pos + i) / 100
              if result = 0 then total - 1 else total
           else 0)
        let isAtZero = if result = 0 then count + 1 else count
        (result, isAtZero + passedZero)

let unlock combo =
   let startPos = 50
   seq {
        let mutable acc = (startPos, 0)
        for line in combo  do
          let next = rotate acc line
          // printfn $"%A{line} -> %A{next}"
          acc <- next

        yield acc
   } |> Seq.last


let part2 () =
    let startPos = 50

    printfn "50R1000 = %A" <| rotate (startPos, 0) "R1000"
    printfn "50R950 = %A" <| rotate (startPos, 0) "R950"
    printfn "50L950 = %A" <| rotate (startPos, 0) "L950"
    printfn "2L3 = %A" <| rotate (2, 0) "L3"
    printfn "2L300 = %A" <| rotate (2, 0) "L300"
    printfn "2L102 = %A" <| rotate (2, 0) "L102"

    ones
    |> Seq.iter (unlock >> printfn "%A")

    let (endAcc, zeros) = input () |> unlock

    zeros.ToString()
