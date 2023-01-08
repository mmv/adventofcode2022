module Day13
open Utils

// For this problem, the input is a list of numbers or lists.
// So something like [1,2,[3,4],5] is a valid input, as is
// [[]] and [].
type PacketList =
    | Number of int
    | List of PacketList list

let parseNumber (cs: char list) =
    let rec loop (cs: char list) (acc: int) =
        match cs with
        | [] -> (Number acc), []
        | c :: cs' when c = ',' || c = ']' -> (Number acc), cs
        | c :: cs' -> loop cs' (acc * 10 + (int c - int '0'))
    loop cs 0

let rec parseList (cs: char list) =
    let rec loop (cs: char list) (acc: PacketList list) =
        match cs with
        | [] -> (List acc), []
        | c :: cs' when c = ']' -> (List acc), cs'
        | c :: cs' when c = '[' ->
            let (packet, cs'') = parseList cs'
            loop cs'' (packet :: acc)
        | c :: cs' when c = ',' -> loop cs' acc
        | _ ->
            let (packet, cs'') = parseNumber cs
            loop cs'' (packet :: acc)
    match loop cs [] with
    | (List items, rest) -> (List (List.rev items)), rest
    | _ -> failwith "parseList"

let packetCompare p1 p2 =
    let rec packetCompare p1 p2 =
        match p1, p2 with
        | Number(a), Number(b) -> if a = b then None else Some(a < b)
        | List(a), Number(b) -> packetCompare p1 (List [p2])
        | Number(a), List(b) -> packetCompare (List [p1]) p2
        | List([]), List(_::_) -> Some(true)
        | List(_::_), List([]) -> Some(false)
        | List([]), List([]) -> None
        | List(x::xs), List(y::ys) ->
            match packetCompare x y with
            | None -> packetCompare (List xs) (List ys)
            | Some(x) -> Some(x)
    packetCompare p1 p2 |> Option.get

// Check packet pairs if they're in the right order.
// Report the sum of the index of the packages that are in the right order.
let solve1 (inputReader: unit -> string[]) =
    inputReader ()
    |> batchSplit
    |> Seq.map (fun batch ->
        let xs = batch |> Seq.toArray
        (parseList (xs.[0] |> Seq.toList)), (parseList (xs.[1] |> Seq.toList)))
    |> Seq.map (Tuple2.map fst)
    |> Seq.map (fun (p1, p2) -> packetCompare p1 p2)
    |> Seq.mapi (fun i v -> (i+1,v))
    |> Seq.print
    |> Seq.filter snd
    |> Seq.sumBy fst
    |> string


// Disregard the pairs, treat the whole input as a sequence of packets
// (which means clearing the blank lines).
// Add 2 additional packets ([[2]] and [[6]]) and sort them.
// Multiply the indexes of where the two inserted packets landed.
let solve2 (inputReader: unit -> string[]) =
    let dividerPacket2 = List [List [Number 2]]
    let dividerPacket6 = List [List [Number 6]]
    let sortedList =
        inputReader ()
        |> Seq.filter (fun line -> line <> "")
        |> Seq.map (fun line -> parseList (line |> Seq.toList))
        |> Seq.map fst
        |> Seq.append [dividerPacket2; dividerPacket6]
        |> Seq.sortWith (fun v1 v2 -> if packetCompare v1 v2 then -1 else 1)
        |> Seq.toArray
    let dividerIndex2 = Array.findIndex (fun p -> p = dividerPacket2) sortedList
    let dividerIndex6 = Array.findIndex (fun p -> p = dividerPacket6) sortedList
    (dividerIndex2 + 1) * (dividerIndex6 + 1)
    |> string