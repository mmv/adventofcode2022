module Day17
open Utils

let fallingShapes =
    [|
        [| "####" |]
        [|
            ".#."
            "###"
            ".#."
        |]
        [|
            "..#"
            "..#"
            "###"
        |]
        [|
            "#"
            "#"
            "#"
            "#"
        |]
        [|
            "##"
            "##"
        |]
    |]
    |> Array.map (fun shape ->
        shape
        |> Array.rev
        |> Array.mapi (fun y row ->
            row
            |> Seq.mapi (fun x c -> (x, y), c = '#')
            |> Seq.filter snd
            |> Seq.map fst
            |> Array.ofSeq
        )
        |> Array.concat
    )

let shapeHeights =
    fallingShapes
    |> Array.map (fun shape ->
        shape
        |> Array.map snd
        |> Array.max
    )


let printMap map =
    let minx, maxx = map |> Seq.map fst |> Seq.minmax
    let maxy = map |> Seq.map snd |> Seq.max
    for y in { maxy..(-1)..0 } do
        for x in { 0..6 } do
            printf "%s" (if Set.contains (x,y) map then "#" else ".")
        printfn ""

let move shape delta =
    shape
    |> Array.map (Tuple2.add delta)

let wind (pattern: string) turn =
    if pattern.[turn % pattern.Length] = '<' then -1 else 1

let towerHeight (tower: Set<int*int>) =
    if tower.IsEmpty then 0
    else (tower |> Seq.maxBy snd |> snd)+1

// main loop for a single shape
// start by placing the rock 3 rows above the top of the current tower
// then move the shape down until it hits anything (or the bottom of the tower)
let rockLoop (tower: Set<int*int>) (shape: (int*int) array) (shapeHeight) windFlow windTurn =
    let initialDrop = move shape (2, (towerHeight tower) + 3)
    let rec loop (rock: (int*int) array) turn =
        let movedByWind = move rock ((windFlow turn), 0)
        // printMap (Set.ofSeq rock)
        // printfn "---"
        let rock' =
            if movedByWind |> Array.exists (fun (x,y) -> tower.Contains (x,y) || x < 0 || x >= 7)
            // wind couldn't move rock further because the new position is occupied
            then rock
            // wind moved rock so gravity acts on new position
            else movedByWind
        let rock'' =
            move
                rock'
                (0,-1)
        if rock'' |> Array.exists (fun p -> tower.Contains p) || rock'' |> Seq.exists(fun (_,y) -> y < 0) then
            rock', turn
        else
            loop rock'' (turn+1)
    loop initialDrop windTurn


// TODO: implement
let solve1 (inputReader: unit -> string[]) =
    // inputReader ()
    // |> ignore
    // "0"
    let input = inputReader() |> Seq.exactlyOne
    let windFlow = wind input

    let towerState, _ =
        Seq.init 2022 id
        |> Seq.fold (fun (tower,wt) i ->
            let shapeNum = i % fallingShapes.Length
            let (shapePlace, windTurn) = rockLoop tower fallingShapes.[shapeNum] shapeHeights.[shapeNum] windFlow wt
            let tower' = 
                shapePlace
                |> Set.ofSeq
                |> Set.union tower
            tower', (windTurn + 1)
        ) (Set.empty, 0)

    // printfn "%A" towerState
    // printMap towerState
    let finalHeight = towerHeight towerState

    finalHeight |> string

// For the second part, instead of looping for 2022 cycles
// we need to loop for 1000000000000 cycles, which is obviously too much
// so we need to find a pattern to shortcut.
let solve2 (inputReader: unit -> string[]) =
    
    let encodeRow xs =
        seq { 1..7 }
        |> Seq.fold (fun acc x -> acc + (if Set.contains x xs then 1 else 0) * 2) 0

    let towerHead (tower: Set<int*int>) =
        tower
        |> Seq.groupBy snd
        |> Seq.sortByDescending fst
        |> Seq.take 50
        |> Seq.map (fun (_, xs) -> encodeRow (xs |> Seq.map fst |> Set.ofSeq))
        |> List.ofSeq
    
    // We can find a pattern between any full rows. A full row essentially means that
    // no pieces can fall through it, so it represents an "initial state" of the tower.
    // If full-row patterns repeat themselves, then we can just shortcut everything that
    // happens in between.

    let input = inputReader() |> Seq.exactlyOne
    let windFlow = wind input

    let cycleLength = input.Length * (Array.length fallingShapes)

    let fullCycleLoop towerStart wtStart =
        printfn "Starting full cycle loop starting on cycle %d" wtStart
        Seq.init cycleLength id
        |> Seq.fold (fun (tower,wt) i ->
            let shapeNum = i % fallingShapes.Length
            let (shapePlace, windTurn) = rockLoop tower fallingShapes.[shapeNum] shapeHeights.[shapeNum] windFlow wt
            let tower' = 
                shapePlace
                |> Set.ofSeq
                |> Set.union tower
            tower', (windTurn + 1)
        ) (towerStart, wtStart)
    
    // Do full cycle loops until we get a tower head equal to one we've seen before
    let rec loop tower cycle seen =
        let head = towerHead tower
        match Map.tryFind head seen with
        | Some((cycleStart, cycleHeight)) ->
            printfn "Found a pattern"
            printfn "Cycle %A matches cycle %A" cycle cycleStart
            let curHeight = towerHeight tower
            printfn "Current height %A matched with %A" curHeight cycleHeight
            let cycleDiff = cycle - cycleStart
            let heightDiff = curHeight - cycleHeight
            let cyclesLeft = 1000000000000I - cycle
            let cyclesToSkip = cyclesLeft / cycleDiff
            let cyclesToSkipHeight = cyclesToSkip * (bigint heightDiff)
            let afterSkipHeight = (bigint curHeight) + cyclesToSkipHeight
            let remaining = int (cyclesLeft % cycleDiff)
            printfn "If we jumped to cycle %A, we would be at height %A" (cycle + cyclesToSkip * cycleDiff) afterSkipHeight
            printfn "We need to do %d more cycles" remaining
            let remainingTower =
                Seq.init remaining id
                |> Seq.fold
                    (fun (tower') (c: int) ->
                        let shapeNum = c % fallingShapes.Length
                        let (shapePlace, _) = rockLoop tower' fallingShapes.[shapeNum] shapeHeights.[shapeNum] windFlow 0
                        let tower'' = 
                            shapePlace
                            |> Set.ofSeq
                            |> Set.union tower'
                        tower''
                    )
                    tower
            let finalHeight = bigint (towerHeight remainingTower) + cyclesToSkipHeight
            finalHeight
        | None ->
            let (tower', cycle') = fullCycleLoop tower (int cycle)
            loop tower' (bigint cycle') (Map.add head ((bigint cycle'), towerHeight tower) seen)
    
    let (tower, cycle) = fullCycleLoop Set.empty 0
    let finalHeight = loop tower (bigint cycle) Map.empty

    finalHeight |> string