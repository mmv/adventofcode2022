module Day16
open Utils

// The input is in the following format:
// Valve AA has flow rate=0; tunnels lead to valves DD, II, BB

// We take 1 minute to open a valve and 1 minute to navigate to the next.
// There's a total of 30 minutes and the goal is to maximize the pressure
// released, which will be the time by which the valve gets opened times
// the remaining minutes.

// We can treat this as a graph search, where at each valve we have the
// choice of opening it or following to another and want to maximize the
// pressure release.

type Valve = { rate: int; tunnels: string list }
let parseLine (line: string) =
    let regex = System.Text.RegularExpressions.Regex("Valve (?<name>[A-Z]+) has flow rate=(?<rate>-?[0-9]+); tunnels? leads? to valves? (?<tunnels>[A-Z, ]+)")
    let m = regex.Match(line)
    let name = m.Groups.["name"].Value
    let rate = int m.Groups.["rate"].Value |> int
    let tunnels = m.Groups.["tunnels"].Value.Split(", ") |> List.ofArray
    name, { rate = rate; tunnels = tunnels }

// Graph algorithms work better when no cycles are involved. But in this case
// it's legit to cycle back to a valve to access others connected just to it.
// So one way to go about this, is to create a new graph where we push the cost
// of going through a valve and simulate a direct edge.

let buildDirectAccessGraph valves =
    
    let rec bfsNodeDistance distances visited queue target =
        if Queue.isEmpty queue then distances
        else
            let (next, level), queue' = Queue.dequeue queue
            if Set.contains next visited then bfsNodeDistance distances visited queue' target
            else
                let distances' =
                    distances
                    |> Map.add next level
                let visited' = Set.add next visited
                let queue'' =
                    queue'
                    |> Queue.append (
                        valves
                        |> Map.find next
                        |> fun v -> v.tunnels
                        |> Seq.map (fun t -> t, level+1))
                bfsNodeDistance distances' visited' queue'' target
    
    valves
    |> Map.keys
    |> Seq.map (fun k -> k, bfsNodeDistance Map.empty Set.empty (Queue.singleton (k,0)) k)
    |> Seq.collect (fun (k, distances) -> distances |> Map.toSeq |> Seq.map (fun (t,v) -> (k,t), v))
    // |> Seq.filter (fun ((k,t),_) -> k <> t)
    |> Seq.fold (fun distances (kt,v) ->
        match distances |> Map.tryFind kt with
        | Some v' when v' < v -> distances
        | _ -> distances |> Map.add kt v
        ) Map.empty

let rec searchMaxFlow valves minutes openValves distances visited current cache =
    if minutes < 0 then cache, 0
    elif Map.containsKey (minutes, current, visited) cache then
        cache, (cache |> Map.find (minutes, current, visited))
    elif Set.count visited = Map.count valves then cache, 0
    else
        let possibleNext =
            valves
            |> Map.keys
            |> Seq.filter (fun k ->
                not (Set.contains k visited)
                && k <> current
                && (Map.find k valves).rate > 0
                && not (Set.contains k openValves))
            |> Seq.map (fun k ->
                let distance = distances |> Map.find (current, k)
                let remainingMinutes = minutes - distance - 1
                k, remainingMinutes)
            |> Seq.sortByDescending (fun (k,v) -> (Map.find k valves).rate * (v - 1))
        
        let newBestCache, newBestValue =
            possibleNext
            |> Seq.fold (fun (newCache, prevBest) (k,v) ->
                let flow = (Map.find k valves).rate * v
                let visited' = Set.add k visited
                let openValves' = Set.add k openValves
                let nextCache, value = searchMaxFlow valves v openValves' distances visited' k newCache
                nextCache, (max (flow + value) prevBest)
                ) (cache, 0)
        
        (newBestCache |> Map.add (minutes, current, visited) newBestValue), newBestValue


// a different version of searchMaxFlow for part 2 where we have 2 players
// running through the valves and opening them at the same time...
let rec searchMaxFlow2 valves openValves distances visited minutes p otherwait otherp cache =
    // minutes are the remaining minutes
    // p is the "current" player
    // otherminutes is the amount of time that the other player has before it reaches the next valve
    // otherp is the valve that the other player is going to

    if minutes < 0 then cache, 0
    elif Map.containsKey (minutes - otherwait, p, visited) cache then
        cache, (cache |> Map.find (minutes - otherwait, p, visited))
    elif otherwait = 0 && Map.containsKey (minutes, otherp, visited) cache then
        cache, (cache |> Map.find (minutes, otherp, visited))
    elif Set.count visited = Map.count valves then cache, 0
    else
        let possibleNext p minutes =
            valves
            |> Map.keys
            |> Seq.filter (fun k ->
                not (Set.contains k visited)
                && k <> p
                && k <> otherp
                && (Map.find k valves).rate > 0
                && not (Set.contains k openValves))
            |> Seq.map (fun k ->
                let distance = distances |> Map.find (p, k)
                let remainingMinutes = minutes - distance - 1
                k, remainingMinutes)
        
        let possibleNext = possibleNext p minutes

        let newBestCache, newBestValue =
            possibleNext
            |> Seq.fold (fun (newCache, prevBest) (k,v) ->
                let flow = (Map.find k valves).rate * v
                let visited' = Set.add k visited
                let openValves' = Set.add k openValves
                let cost = minutes - v
                let nextCache, value =
                    if cost > otherwait then
                        searchMaxFlow2 valves openValves' distances visited' (minutes - otherwait) otherp (cost - otherwait) k newCache
                    else
                        searchMaxFlow2 valves openValves' distances visited' v k (otherwait - cost) otherp newCache
                nextCache, (max (flow + value) prevBest)
                ) (cache, 0)


        
        (newBestCache |> Map.add (minutes - otherwait, p, visited) newBestValue), newBestValue

let solve1 (inputReader: unit -> string[]) =
    let valves =
        inputReader ()
        |> Seq.map parseLine
        |> Map.ofSeq
    let distances = buildDirectAccessGraph valves
    let start = "AA"
    let minutes = 30
    let openValves = Set.empty
    searchMaxFlow valves minutes openValves distances (Set.empty) start Map.empty
    |> string

// TODO: implement
let solve2 (inputReader: unit -> string[]) =
    let valves =
        inputReader ()
        |> Seq.map parseLine
        |> Map.ofSeq
    let distances = buildDirectAccessGraph valves
    let start = "AA"
    let minutes = 26
    let openValves = Set.empty
    searchMaxFlow2 valves openValves distances (Set.empty) minutes start 0 start Map.empty
    |> string