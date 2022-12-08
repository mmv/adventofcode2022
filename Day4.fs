module Day4
open Utils

type Range = { min: int; max: int }
let isContained r1 r2 = r1.min >= r2.min && r1.max <= r2.max
let overlaps r1 r2 = r1.min <= r2.max && r2.min <= r1.max

// Our input is a list of pairs of ranges.
// Each range is separated by a '-', each pair by a comma.
// We need to find lines where one of the ranges fully overlaps with the other.
// The result is the number of lines that match.
let coreLogic (inputReader: unit -> string[]) (decisor: Range -> Range -> bool) =
    inputReader ()
    |> Seq.map (fun line ->
        pairSplit "," line
        |> Tuple2.map (
            pairSplit "-"
            >> Tuple2.map int
            >> (fun (a,b) -> { min = a; max = b })
            )
        |> (fun (r1, r2) -> decisor r1 r2 || decisor r2 r1)
    )
    |> Seq.print
    |> Seq.filter id
    |> Seq.length

let solve1 (inputReader: unit -> string[]) = coreLogic inputReader isContained

// Same but now simple overlaps are allowed
let solve2 (inputReader: unit -> string[]) = coreLogic inputReader overlaps