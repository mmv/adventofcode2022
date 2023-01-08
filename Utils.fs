module Utils

open System.IO

module Seq =
    let tap (fn: 'a -> unit) (xs: 'a seq) =
        seq {
            for x in xs do
                fn(x)
                yield x
        }

    let print (xs: 'a seq) =
        seq {
            for x in xs do
                printfn "%A" x
                yield x
        }
    
    let countWith (predicate: 'a -> bool) (xs) =
        Seq.filter predicate xs |> Seq.length

    /// figure simultaneously the min and max of a given seq
    let minmax (xs: 'a seq) =
        use e = xs.GetEnumerator()
        if not (e.MoveNext()) then
            failwith "sequence was empty"
        let mutable minv = e.Current
        let mutable maxv = e.Current
        while e.MoveNext() do
            let x = e.Current
            if x < minv then minv <- x
            if x > maxv then maxv <- x
        (minv, maxv)

module String =
    let split (sep: string) (s: string) =
        s.Split(sep)

module Map =
    let singleton k v = Map.ofList [(k,v)]

module Option =
    let defaultMap defaultValue mapper option =
        option |> Option.defaultValue defaultValue |> mapper |> Some

type Map2d<'a> = Map<(int*int), 'a>
module Map2d =
    let print (m: Map2d<'a>) =
        if Map.isEmpty m
        then
            printfn "(empty map)"
        else
            let keys = m |> Map.keys
            let (minx, maxx) = Seq.minmax (keys |> Seq.map fst)
            let (miny, maxy) = Seq.minmax (keys |> Seq.map snd)
            for y in { miny..maxy } do
                for x in { minx..maxx } do
                    printf "%A" (Map.find (x,y))
                printfn ""
    
    let tapPrint (m: Map2d<'a>) =
        print m
        m
    
    let init x y initial =
        seq {
            for x' in 0..x do
                for y' in 0..y do
                    yield ((x,y),initial)
        }
        |> Map.ofSeq

type Grid2d<'a> = {
    Grid: Map2d<'a>
    Width: int
    Height: int
}

type Counter<'a> = Map of ('a * int)
module Counter =
    // construct a counter based on a sequence of elements
    let from (xs: 'a seq) =
        xs
        |> Seq.countBy id
        |> Map.ofSeq

    let add (m1: Map<'a, int>) (m2: Map<'a, int>) =
        Map.fold
            (fun (m: Map<'a, int>) (k: 'a) v ->
                Map.change
                    k
                    (Option.defaultMap 0 ((+) v))
                    m)
            m1
            m2
            
    let mostCommon (counter: Map<'a, int>) =
        counter
        |> Map.toSeq
        |> Seq.sortByDescending snd

type queue<'a> = Queue of 'a list * 'a list
module Queue =
    let empty = Queue ([],[])
    let isEmpty (Queue (front,back)) = front = [] && back = []
    let enqueue (Queue (front,back)) x = Queue (front, x::back)
    let dequeue (Queue (front,back)) =
        match front with
        | [] -> match List.rev back with
                | [] -> failwith "empty queue"
                | x::xs -> (x, Queue (xs,[]))
        | x::xs -> (x, Queue (xs,back))
    let append (xs: 'a seq) (q: queue<'a>) =
        xs |> Seq.fold (fun q x -> enqueue q x) q
    let singleton x = Queue ([], [x])

let readLines (day: int) =
    let file = $"Inputs/{day}.txt"
    File.ReadAllLines file

let readSampleLines (day: int) =
    let file = $"Inputs/{day}.sample.txt"
    File.ReadAllLines file

let pairSplit (c: string) (s: string) =
    match s.Split(c) with
    | [| a; b |] -> (a,b)
    | _ -> failwith $"didn't split well '{s}' '{c}'"

let batchSplit (lines: string seq) =
    let batchNums = lines |> Seq.scan (fun s i -> if i = "" then s+1 else s) 0
    lines
    |> Seq.zip batchNums
    |> Seq.groupBy (fun (batch,line) -> batch)
    |> Seq.map (fun (key,(g)) ->
        g |> Seq.map (fun (_,b) -> b)
          |> Seq.filter (fun x -> x <> ""))

let neighbors4 = [|
    (-1,0)
    (0,1)
    (1,0)
    (0,-1)
|]

let neighbors8 =
    Array.append
        [| (-1,-1); (-1,1); (1,-1); (1,1) |]
        neighbors4
    

module Tuple2 =
    let map (f: 'a -> 'b) (t: 'a*'a) =
        let (a,b) = t
        (f a, f b)
    
    let uncurry f (x,y) = f x y
    let curry f x y = f (x,y)
    let swap (x,y) = (y,x)
    let map1 f (x,y) = (f x, y)
    let map2 f (x,y) = (x, f y)
    let join f (x,y) (x',y') = (f x x', f y y')
    let add (x,y) (a,b) = (x+a, y+b)