module Day1

open Utils

/// The given list has an integer on each line, organized in batches
/// separated by newlines. Each batch represents how many calories
/// an Elf is taking. We need to find how many calories the one carrying
/// the most calories is taking.
let solve1 (inputReader: unit -> string[]) =
    inputReader ()
    |> batchSplit
    |> Seq.map (fun batch ->
        batch
        |> Seq.map int
        |> Seq.sum)
    |> Seq.max

/// Now instead of just getting the top calorie count, we need to
/// sum the top-3 Elves' calorie counts.
let solve2 (inputReader: unit -> string[]) =
    inputReader ()
    |> batchSplit
    |> Seq.map (fun batch ->
        batch
        |> Seq.map int
        |> Seq.sum)
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.sum