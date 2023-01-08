module Day8
open Utils

// A grid with tree heights is provided.
// A tree is visible if all other trees between it and the edge
// of the grid are shorter than it.

type Grid = { width: int; height: int; trees: Map2d<int> }

let inputToGrid (lines: string[]) =
    let width = lines.[0].Length
    let height = lines.Length
    let trees =
        lines
        |> Seq.mapi (fun y line ->
            line
            |> Seq.mapi (fun x c ->
                ((x,y), (int c) - (int '0'))))
        |> Seq.concat
        |> Map.ofSeq
    { width = width; height = height; trees = trees }

// check if a point is outside the grid
let isOutside (x,y) grid =
    x < 0 || y < 0 || x >= grid.width || y >= grid.height

let findVisibleTrees grid =
    // build a line of sight, and then advance it
    // collecting all visible trees

    let minsInit sz = Array.init sz (fun x -> -1)

    let look mins depths coord =

        depths
        |> Seq.fold (fun (visible, mins) y ->
            let checks =
                mins
                |> Seq.mapi (fun x min ->
                    let thisValue = grid.trees.[coord x y]
                    if thisValue > min
                    then
                        // printfn "replacing oldmin %d with %d at %A" min thisValue (coord x y)
                        (true, thisValue)
                    else (false, min))
            let visible' =
                checks
                |> Seq.mapi (fun x (visible, _) -> visible, (coord x y))
                |> Seq.filter fst
                |> Seq.map snd
                |> Set.ofSeq
                |> Set.union visible
            let mins' =
                checks
                |> Seq.map snd
                |> Array.ofSeq
            (visible', mins'))
            (Set.empty, mins)
        |> fst
        
    let visibleTrees =
        (look (minsInit grid.width) (Seq.init grid.height id) (fun x y -> x, y))
        |> Set.union
            (look (minsInit grid.height) (Seq.init grid.width id) (fun y x -> x, y))
        |> Set.union
            (look (minsInit grid.width) (Seq.init grid.height id) (fun x y -> x, grid.height - y - 1))
        |> Set.union
            (look (minsInit grid.height) (Seq.init grid.width id) (fun y x -> grid.width - 1 - x, y))
    
    // for p in visibleTrees do
    //     printfn "%A" p
    visibleTrees

// TODO: implement
let solve1 (inputReader: unit -> string[]) =
    let grid =
        inputReader ()
        |> inputToGrid
    let visibleTrees = findVisibleTrees grid
    visibleTrees
    |> Seq.length
    |> string

// given a tree position, create sequence of positions
// that correspond to the line of sight in each of the
// four possible directions
let lineOfSight grid (x,y) =
    seq {
        yield seq { for x' in { x-1..-1..0 } do yield (x', y) }
        yield seq { for x' in { x+1..grid.width-1 } do yield (x', y) }
        yield seq { for y' in { y-1..-1..0 } do yield (x, y') }
        yield seq { for y' in { y+1..grid.height-1 } do yield (x, y') }
    }

let scoreLineOfSight grid (x,y) =

    let treeHeight = grid.trees.[x,y]

    let scoreLine line =
        // scoring the line is tricky because we need to account
        // all trees including the final (or not if we reached an edge)
        seq {
            for p in line do
                if grid.trees.[p] < treeHeight
                then yield true
                else
                    yield true // this tree is visible
                    yield false // but the next one is not
        }
        |> Seq.takeWhile id
        |> Seq.length

    lineOfSight grid (x,y)
    |> Seq.map scoreLine
    |> Seq.reduce ( * )

// TODO: implement
let solve2 (inputReader: unit -> string[]) =
    let grid = inputReader () |> inputToGrid
    let bestSightScore =
        grid.trees
        |> Map.keys
        |> Seq.filter (fun (x,y) -> x > 0 && y > 0 && x < grid.width - 1 && y < grid.height - 1)
        // |> Seq.print
        |> Seq.map (scoreLineOfSight grid)
        // |> Seq.print
        |> Seq.max
    
    bestSightScore |> string