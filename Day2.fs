module Day2
open Utils

// let's start by setting up the logic of Rock Paper Scissors
type RPS = Rock | Paper | Scissors

let scorePlay p =
    match p with
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

let scoreRPS (p1: RPS) (p2: RPS) =
    match p1, p2 with
    | Rock, Paper -> -1
    | Rock, Scissors -> 1
    | Paper, Rock -> 1
    | Paper, Scissors -> -1
    | Scissors, Rock -> -1
    | Scissors, Paper -> 1
    | _ -> 0

let scoreMatch p1 p2 =
    (scorePlay p2) + (scoreRPS p2 p1)*3+3

/// Given a list, takes each element of the list and returns a pair of that
/// element with the reamining elements of the list.
let pairWithRemaining (xs: 'a seq) =
    let xset = xs |> Set.ofSeq
    seq {
        for x in xset do
            yield (x, xset |> Set.remove x)
    }


/// given two lists of the same size, return possible combinations
/// of mappings from elements of the first list to elements of the second list
let possibleMappings (xs: 'a list) (ys: 'b list) =
    let rec loop (xs: 'a list) (ys: 'b seq) =
        match xs, ys with
        | [], _ -> [Map.empty]
        | [a], bs -> [ Map.ofList [(a, Seq.exactlyOne bs)] ]
        | x::xs, ys ->
            let possible = pairWithRemaining ys
            seq {
                for (y,tl) in possible do
                    for m in loop xs tl do
                        yield Map.add x y m
            } |> Seq.toList
    loop xs ys

// TODO: implement
let solve1 (inputReader: unit -> string[]) =

    let firstMap = [ ('A', Rock); ('B', Paper); ('C', Scissors) ] |> Map.ofList
    let secondMap = [ ('X', Rock); ('Y', Paper); ('Z', Scissors) ] |> Map.ofList

    inputReader ()
    |> Seq.map (pairSplit " ")
    |> Seq.print
    |> Seq.map (fun (a,b) -> scoreMatch (firstMap.[a.[0]]) (secondMap.[b.[0]]))
    |> Seq.print
    |> Seq.sum


let solve2 (inputReader: unit -> string[]) =
    let firstMap = [ ('A', Rock); ('B', Paper); ('C', Scissors) ] |> Map.ofList

    // now X means lose, Y means tie, Z means win
    let decidePlay theirs goal =
        match theirs, goal with
        | x, 'Y' -> x
        | Rock, 'Z' -> Paper
        | Rock, 'X' -> Scissors
        | Paper, 'Z' -> Scissors
        | Paper, 'X' -> Rock
        | Scissors, 'Z' -> Rock
        | Scissors, 'X' -> Paper
        | _ -> failwith "impossible"
    
    inputReader()
    |> Seq.map (pairSplit " ")
    |> Seq.map (fun (a,b) -> (firstMap.[a.[0]], decidePlay (firstMap.[a.[0]]) b.[0]))
    |> Seq.print
    |> Seq.map (fun (a,b) -> scoreMatch a b)
    |> Seq.sum

// TODO: implement
let solve3 (inputReader: unit -> string[]) =
    let firstMap = [ ('A', Rock); ('B', Paper); ('C', Scissors) ] |> Map.ofList
    let possibleSecondMaps =
        possibleMappings ['X'; 'Y'; 'Z'] [Rock; Paper; Scissors]
    
    let possibleScores =
        possibleSecondMaps
        |> Seq.map (fun secondMap ->
            let score = Map.fold (fun s k v -> s + scoreRPS (Map.find k firstMap) v) 0 secondMap
            (secondMap, score))

    inputReader ()
    |> ignore
    0