module Debug

let printPositions (ps: seq<int*int>) =
    let minx = ps |> Seq.map fst |> Seq.min
    let maxx = ps |> Seq.map fst |> Seq.max
    let miny = ps |> Seq.map snd |> Seq.min
    let maxy = ps |> Seq.map snd |> Seq.max
    let psset = ps |> Set.ofSeq
    for y in miny .. maxy do
        for x in minx .. maxx do
            if psset |> Set.contains (x,y) then
                printf "X"
            else
                printf "."
        printfn ""
    printfn "(%d,%d) to (%d,%d)" minx miny maxx maxy

let pipeTap f x =
    f x
    x