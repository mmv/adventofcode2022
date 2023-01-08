module Day12
open Utils

// The puzzle input is a grid with elevations where the elevations
// are represented by letters a-z. There's also S for the starting
// position (which will be an 'a' elevation) and E for the target
// position (which will be a 'z' elevation).
// We can only move in the 4 directions and to squares that are at
// most 1 elevation higher than the current one. We need to find the
// shortest path from S to E.

type Grid = {
    Width: int
    Height: int
    Start: int*int
    Target: int*int
    Value: (int*int) -> int
}

let parseGrid (input: string[]) =
    let width = input.[0].Length
    let height = input.Length
    let vmap =
        input
        |> Seq.mapi (fun y line -> line |> Seq.mapi (fun x c -> (x, y), (int c) - (int 'a')))
        |> Seq.concat
        |> Map.ofSeq
    let start = vmap |> Map.findKey (fun _ v -> v = (int 'S') - (int 'a'))
    let target = vmap |> Map.findKey (fun _ v -> v = (int 'E') - (int 'a'))
    let vmap' = vmap |> Map.add start 0 |> Map.add target (int 'z' - int 'a')
    let value p = vmap'.[p]
    { Width = width
      Height = height
      Start = start
      Target = target
      Value = value }

let inMap (grid: Grid) (x, y) =
    x >= 0 && x < grid.Width && y >= 0 && y < grid.Height

let pathfind grid = 
    let rec loop (queue: ((int*int)*int) queue) (visited: Set<(int*int)>) =
        match Queue.dequeue queue with
        | (((x, y), depth), queue') ->
            if (x, y) = grid.Target then depth
            elif Set.contains (x, y) visited then loop queue' visited
            else
                let visited = Set.add (x, y) visited
                let neighbors =
                    Utils.neighbors4
                    |> Array.toList
                    |> List.map (Tuple2.join (+) (x, y))
                    |> List.filter (inMap grid)
                    |> List.filter (fun p -> grid.Value p <= grid.Value (x, y) + 1)
                    |> List.filter (fun p -> not (Set.contains p visited))
                    |> List.map (fun p -> (p, depth+1))
                loop (Queue.append neighbors queue') visited
    loop (Queue.singleton (grid.Start,0)) Set.empty

let solve1 (inputReader: unit -> string[]) =
    let grid = inputReader() |> parseGrid
    pathfind grid |> string

// For part 2 we need to find the 'a' tile for which the path to 'E' is
// the shortest. The easiest way to do this is by adapting the pathfinder
// algorithm to start at the end and BFS flood until it finds any 'a'.

let pathfind2 grid = 
    let rec loop (queue: ((int*int)*int) queue) (visited: Set<(int*int)>) =
        match Queue.dequeue queue with
        | (((x, y), depth), queue') ->
            if grid.Value (x, y) = 0 then depth
            elif Set.contains (x, y) visited then loop queue' visited
            else
                let visited = Set.add (x, y) visited
                let neighbors =
                    Utils.neighbors4
                    |> Array.toList
                    |> List.map (Tuple2.join (+) (x, y))
                    |> List.filter (inMap grid)
                    |> List.filter (fun p -> grid.Value (x, y) <= grid.Value p + 1)
                    |> List.filter (fun p -> not (Set.contains p visited))
                    |> List.map (fun p -> (p, depth+1))
                loop (Queue.append neighbors queue') visited
    loop (Queue.singleton (grid.Target,0)) Set.empty

let solve2 (inputReader: unit -> string[]) =
    let grid = inputReader() |> parseGrid
    pathfind2 grid |> string