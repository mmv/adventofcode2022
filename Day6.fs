module Day6
open Utils

// communication device
// needs to lock on to the signal
// start of packet marker, has 4 different characters
// find how many chars need to be processed until we get 4 different chars

let rec takeAtMost n lst =
    match lst with
    | [] -> []
    | x::xs ->
        if n = 0 then []
        else x::(takeAtMost (n-1) xs)

let mkprocessor numChars =
    let rec processor seen chars =
        match chars with
        | [] -> failwith "should've found something"
        | c::cs ->
            if seen |> takeAtMost numChars |> Set.ofList |> Set.count = numChars
            then List.length seen
            else processor (c::seen) cs
    processor

// TODO: implement
let solve numChars (inputReader: unit -> string[]) =
    inputReader ()
    |> Array.exactlyOne
    |> List.ofSeq
    |> (mkprocessor numChars) []
    |> string

// TODO: implement
let solve1 = solve 4
let solve2 = solve 14