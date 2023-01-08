module Day15
open Utils

// The input is in a format like
// Sensor at x=2, y=18: closest beacon is at x=-2, y=15

// For the first task we need to determine the amount of positions in the
// y=2000000 line that can't have a beacon.
// We know that a position can't have a beacon if it's closer to a sensor
// than the "closest beacon" for that sensor.

type SensorSignal = { sensor: int*int; beacon: int*int }
let parseLine (line: string) =
    let regex = System.Text.RegularExpressions.Regex("Sensor at x=(?<sensorx>-?[0-9]+), y=(?<sensory>-?[0-9]+): closest beacon is at x=(?<beaconx>-?[0-9]+), y=(?<beacony>-?[0-9]+)")
    let m = regex.Match(line)
    let sensorx = int m.Groups.["sensorx"].Value
    let sensory = int m.Groups.["sensory"].Value
    let beaconx = int m.Groups.["beaconx"].Value
    let beacony = int m.Groups.["beacony"].Value
    { sensor = sensorx, sensory; beacon = beaconx, beacony }

let manhattanDistance (x1,y1) (x2,y2) =
    abs (x1 - x2) + abs (y1 - y2)

let findCloserPositionsAtY (y: int) (signals: SensorSignal seq) =
    signals
    |> Seq.collect (fun { sensor = sx, sy; beacon = bx, by } ->
        // the closest candidate at y is the point with same X as the sensor.
        // we can move further away in the x axis and keep storing points that
        // are still closer to the sensor
        let closestDistance = manhattanDistance (sx,sy) (bx,by)
        Seq.initInfinite id
        |> Seq.takeWhile (fun dx ->
            manhattanDistance (sx,sy) (sx+dx,y) <= closestDistance)
        |> Seq.collect (fun dx -> [ (sx+dx,y); (sx-dx,y) ])
    )
    |> Set.ofSeq

let solve1 (inputReader: unit -> string[]) =
    let signals = inputReader () |> Seq.map parseLine
    let y = 2000000
    let closerPositions = findCloserPositionsAtY y signals
    let occupied = 
        signals |> Seq.map (fun { sensor = sx, sy } -> (sx,sy))
        |> Seq.append (signals |> Seq.map (fun { beacon = bx, by } -> (bx,by) ))
        |> Set.ofSeq
    let result = Set.difference closerPositions occupied
    result
    |> Set.count
    |> string


// Given an area ((0,0) to (4000000,4000000)) we need to find the single position
// that's not covered by any sensor.
// If there's a single point, that means it needs to be:
// - at most 1 point away from the cover-area of any sensor
// - outside the cover area of any sensor

// If we search the whole 0x4000000 area we'll take too much time,
// but maybe it's fast enough to just search the edges of the sensor coverage areas.

let sensorCoverageEdgePoints sensor =
    let { sensor = sx, sy; beacon = bx, by } = sensor
    let distance = 1 + manhattanDistance (sx,sy) (bx,by)
    seq {
        for i in 0..distance do
            yield (sx-distance+i, sy-i)
            yield (sx-distance+i, sy+i)
            yield (sx+distance-i, sy-i)
            yield (sx+distance-i, sy+i)
    }

let solve2 (inputReader: unit -> string[]) =
    let signals = inputReader () |> Seq.map parseLine |> Seq.cache

    let edgePoints =
        signals
        |> Seq.collect sensorCoverageEdgePoints
        |> Seq.filter (fun (x,y) -> x >= 0 && x <= 4000000 && y >= 0 && y <= 4000000)
        |> Seq.distinct
        |> Seq.cache
    
    printfn "calculated %d points" (edgePoints |> Seq.length)

    edgePoints
    |> Seq.filter (fun (x,y) ->
        signals
        |> Seq.forall (fun signal ->
            let { sensor = sx, sy; beacon = bx, by } = signal
            let distance = manhattanDistance (sx,sy) (bx,by)
            manhattanDistance (x,y) (sx,sy) > distance))
    |> Seq.exactlyOne
    |> (fun (x,y) -> (bigint x)*4000000I + (bigint y))
    |> string