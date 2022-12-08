module Day3
open Utils

// lowercase items a through z have priorities 1 to 26
// uppercase items A through Z have priorities 27 to 52
let priorityMap = 
    let lower = [ for c in 'a'..'z' -> (c, int c - int 'a' + 1) ]
    let upper = [ for c in 'A'..'Z' -> (c, int c - int 'A' + 27) ]
    lower @ upper |> Map.ofList

// Input is a list of items in each rucksack.
// Each item is represented by a character.
// A rucksack has 2 compartments. The each half of the string has the items
// for each compartment.
// We need to find which item is in both compartments.
// Then we need to calculate the priority of that item for each rucksack.
// The output is the sum of the priorities.
let solve1 (inputReader: unit -> string[]) =
    let splitStrInHalfs (s: string) =
        let halfLen = s.Length / 2
        s.Substring(0, halfLen), s.Substring(halfLen, halfLen)

    inputReader ()
    |> Seq.map splitStrInHalfs
    |> Seq.map (fun (s1, s2) ->
        let s1Set = Set.ofSeq s1
        let s2Set = Set.ofSeq s2
        let common = Set.intersect s1Set s2Set
        Seq.exactlyOne common
        )
    |> Seq.map (fun c -> priorityMap.[c])
    |> Seq.sum
    

// We need to separate the input in groups of 3 lines each.
// Then find out which item is in all 3 rucksacks.
// Then calculate the priority of that item.
// The result is the sum of the priorities.
let solve2 (inputReader: unit -> string[]) =
    inputReader ()
    |> Seq.batchBySize 3
    |> Seq.map (
        Seq.map Set.ofSeq
        >> Seq.reduce Set.intersect
        >> Seq.exactlyOne
        )
    |> Seq.map (fun c -> priorityMap.[c])
    |> Seq.sum