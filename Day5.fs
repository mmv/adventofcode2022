module Day5
open Utils

// In day 5, the puzzle input has two parts: a drawing of stacks and a list of instructions.
// The drawing is column oriented, and it goes with something like this
//     [D]    
// [N] [C]    
// [Z] [M] [P]
//  1   2   3 
// so we can start by the last row and get the number of stacks, and then go up and fill those stacks.

let buildStacks (lines: string seq) =
    let lines' = lines |> Seq.toList |> List.rev
    let head = List.head lines'
    let rows = List.tail lines'
    let stackNum = head |> String.length |> (/) <| 3
    let parseRow (row: string) =
        printfn "r %A" row
        (" " + row)
        |> Seq.chunkBySize 4
        |> Seq.print
        |> Seq.map (
            function
                | [|' '; ' '; ' '; ' '|] -> None
                | [|' '; '['; c; ']'|] -> Some c
                | _ -> failwith "Invalid input"
        )

    rows
    |> List.fold
        (fun (stacks) row ->
            parseRow row
            |> Seq.zip stacks
            |> Seq.map (fun (stack, c) ->
                match c with
                | None -> stack
                | Some c -> c::stack)
            |> Seq.toArray
        )
        (Array.init stackNum (fun _ -> []))
    
let rec stackMove9000 count src dst stacks =
    if count = 0 then stacks
    else
        stackMove9000 (count - 1) src dst
            (stacks |> Array.mapi (fun i stack ->
                match i with
                | x when x = src -> stack |> List.tail
                | x when x = dst -> (stacks.[src] |> List.head)::stack
                | _ -> stack))

let stackMove9001 count src dst stacks =
    stacks
    |> Array.mapi (fun i stack ->
        match i with
        | x when x = src -> stack |> List.skip count
        | x when x = dst -> (stacks.[src] |> List.take count)@stack
        | _ -> stack
    )

// move instructions are written as
// move 7 from 6 to 2
let parseMoveInstruction (line: string) =
    let words = line.Split(' ')
    (int words.[1], int words.[3], int words.[5])

let solve (inputReader: unit -> string[]) moveFunction =
    let instructions = inputReader() |> batchSplit |> Seq.toArray
    let stacks = instructions.[0] |> buildStacks
    let moves = instructions.[1] |> Seq.map parseMoveInstruction

    // execute instructions
    moves
    |> Seq.fold (fun (stacks) (count, src, dst) ->
        moveFunction count (src-1) (dst-1) stacks)
        stacks
    // then gather items at top of each stack and concatenate them
    |> Array.map (fun stack -> stack |> List.head)
    |> Array.fold (fun (s) c -> s + c.ToString()) ""
    


let solve1 (inputReader: unit -> string[]) = solve inputReader stackMove9000
let solve2 (inputReader: unit -> string[]) = solve inputReader stackMove9001