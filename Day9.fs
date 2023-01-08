module Day9
open Utils

// The tail of the rope follows the head so it's always adjacent.
// When they're separated by at least one step, the tail moves so it
// becomes adjacent.
// The rule is simple in the 4 main directions (eg: T.H becomes .TH)
// but weird in diagonals.

let adjustTail (hx,hy) (tx,ty) =
    // given the current head and tail positions
    // returns the new tail position
    let dx = tx - hx
    let dy = ty - hy
    if (abs dx) <= 1 && (abs dy) <= 1
    then (tx,ty)
    else
        (hx + (if abs dx > 1 then sign dx else 0),
         hy + (if abs dy > 1 then sign dy else 0))

// input is given as a series of instructions
// with U L D or R followed by a number
let instrToHeadDeltas (instr: string) =
    let dir = instr.[0]
    let steps = int (instr.[2..])
    match dir with
    | 'U' -> (0, -steps)
    | 'D' -> (0, steps)
    | 'L' -> (-steps, 0)
    | 'R' -> (steps, 0)
    | _ -> failwith "invalid instruction"

let headDeltasToHeadSteps (dp) =
    match dp with
    | (0, x) -> Seq.init (abs x) (fun i -> (0, sign x))
    | (x, 0) -> Seq.init (abs x) (fun i -> (sign x, 0))
    | _ -> failwith "invalid head deltas"

// TODO: implement
let solve1 (inputReader: unit -> string[]) =
    inputReader ()
    |> Seq.map instrToHeadDeltas
    |> Seq.map headDeltasToHeadSteps
    |> Seq.concat
    |> Seq.scan (fun (hx,hy) (dx,dy) -> (hx+dx, hy+dy)) (0,0)
    |> Seq.scan (fun (tx,ty) (hx,hy) -> adjustTail (hx,hy) (tx,ty)) (0,0)
    |> Set.ofSeq
    |> Set.count
    |> string

// TODO: implement
let solve2 (inputReader: unit -> string[]) =
    // use the same logic as before, but now the rope consists of
    // 10 knots instead of just 1, which means that each "tail" will
    // look upon its corresponding "head" but in the end we'll be interested
    // only in the positions visited by the final tail

    let headMoves =
        inputReader ()
        |> Seq.map instrToHeadDeltas
        |> Seq.map headDeltasToHeadSteps
        |> Seq.concat
        |> Seq.scan (fun (hx,hy) (dx,dy) -> (hx+dx, hy+dy)) (0,0)
    
    let tailPosFromHeadMoves headMoves =
        headMoves
        |> Seq.scan (fun (tx,ty) (hx,hy) -> adjustTail (hx,hy) (tx,ty)) (0,0)
    
    let finalTailPos =
        Seq.init 9 id
        |> Seq.scan (fun headMoves _ -> headMoves |> tailPosFromHeadMoves) headMoves
        |> Seq.last
    
    finalTailPos
    |> Seq.distinct
    |> Seq.length
    |> string