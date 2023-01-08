module Day14
open Utils

type Tile =
    | Wall
    | Sand
    | Border

let printMap map =
    let minx, maxx = map |> Map.keys |> Seq.map fst |> Seq.minmax
    let miny, maxy = map |> Map.keys |> Seq.map snd |> Seq.minmax
    for y in { miny..maxy } do
        for x in { minx..maxx } do
            printf "%s" (match Map.tryFind (x,y) map with
                         | None -> "."
                         | Some(Wall) -> "#"
                         | Some(Sand) -> "o"
                         | Some(Border) -> "!")
        printfn ""

// Our input will be a list of paths that make up walls and floors.
// Each line is given with a list of coords like
// 498,4 -> 498,6 -> 496,6
let parseLine map (line: string) =
    line.Split(" -> ")
    |> Seq.map (pairSplit ",")
    |> Seq.map (Tuple2.map int)
    |> Seq.pairwise
    |> Seq.collect (fun ((x1,y1),(x2,y2)) ->
        if x1 = x2 then
            seq { for y in { (min y1 y2)..(max y1 y2) } do yield (x1, y) }
        elif y1 = y2 then
            seq { for x in { (min x1 x2)..(max x1 x2) } do yield (x, y1) }
        else
            failwith $"bad line {line}")
    |> Seq.fold (fun map p -> Map.add p Wall map) map

let makeBorder map =
    let minx, maxx = map |> Map.keys |> Seq.map fst |> Seq.minmax
    let maxy = map |> Map.keys |> Seq.map snd |> Seq.max
    seq { for x in { minx-1..maxx+1 } do yield (x, maxy+1) }
    |> Seq.fold (fun map p -> Map.add p Border map) map

// the rules of placing sand in the map are:
// - sand flows down until it hits something
// - if the left-down is available, then flows there instead
// - if the right-down is available, then flows there instead
// - otherwise stays there
// we report if the sand falls off the map (by hitting the border)
let rec sandFlow (x,y) map =
    match Map.tryFind (x, y+1) map with
    | None -> sandFlow (x, y+1) map
    | Some(Border) -> false, map
    | _ ->
        if Map.tryFind (x-1, y+1) map = None then
            sandFlow (x-1, y+1) map
        elif Map.tryFind (x+1, y+1) map = None then
            sandFlow (x+1, y+1) map
        else
            true, Map.add (x, y) Sand map

// how many grains can fall from 500,0 until any grain falls off the map?
let solve1 (inputReader: unit -> string[]) =
    let map =
        inputReader ()
        |> Seq.fold parseLine Map.empty
        |> makeBorder
    Seq.initInfinite (fun _ -> (500,0))
    |> Seq.scan (fun m p -> (sandFlow p (snd m))) (true, map)
    // |> Seq.tap (snd >> printMap)
    |> Seq.takeWhile fst
    |> Seq.length
    |> (-) <| 1 // scan will yield the empty map first
    |> string

// for the second part we use a different flow and stopping condition:
// we simulate an infinite floor at a given 'y'
// and the stopping condition becomes the sand hitting the 500,0 point
let rec sandFlow2 lowerLimit (x,y) map =
    match Map.tryFind (x, y+1) map with
    | None when y < lowerLimit -> sandFlow2 lowerLimit (x, y+1) map
    | _ ->
        if y >= lowerLimit then
            true, Map.add (x, y) Sand map
        elif Map.tryFind (x-1, y+1) map = None then
            sandFlow2 lowerLimit (x-1, y+1) map
        elif Map.tryFind (x+1, y+1) map = None then
            sandFlow2 lowerLimit (x+1, y+1) map
        else
            (y > 0), Map.add (x, y) Sand map


let solve2 (inputReader: unit -> string[]) =
    let map =
        inputReader ()
        |> Seq.fold parseLine Map.empty
    let lowerLimit = map |> Map.keys |> Seq.map snd |> Seq.max |> (+) 1
    Seq.initInfinite (fun _ -> (500,0))
    |> Seq.scan (fun m p -> (sandFlow2 lowerLimit p (snd m))) (true, map)
    // |> Seq.tap (snd >> printMap)
    |> Seq.takeWhile fst
    |> Seq.length
    //|> (-) <| 1 // no need to remove one because we need to account for the final grain
    |> string