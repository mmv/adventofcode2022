module Day10
open Utils

// We'll have to emulate a simple CPU with 2 instructions: `addx` and `noop`.
// the CPU will also have a clock cycle counter. Add will take 2 cycles and noop
// will take a single cycle.

type InstructionSet =
    | AddX of int
    | NoOp

let parseInstruction s =
    match s |> String.split " " with
    | [| "addx"; x |] -> AddX (int x)
    | [| "noop" |] -> NoOp
    | _ -> failwithf "invalid instruction: %s" s

type MachineState = { clock: int; regX: int }

let runCycles state instr =
    match instr with
    | AddX x -> { state with regX = state.regX + x; clock = state.clock + 2 }
    | NoOp -> { state with clock = state.clock + 1 }

let rec clockFill state instrs =
    seq {
        match instrs with
        | [] -> yield! Seq.empty
        | instr :: rest ->
            let curClock = state.clock
            let nextState = runCycles state instr
            if nextState.clock > curClock + 1 then
                for i in (curClock+1) .. nextState.clock-1 do
                    yield { state with clock = i }
            yield nextState
            yield! clockFill nextState rest
    }

// read the instructions to the CPU
// run them and calculate the value of the register X
// and some the values for every 40 cycles starting on the 20th cycle
let solve1 (inputReader: unit -> string[]) =
    let instructions =
        inputReader ()
        |> Seq.map parseInstruction
        |> Seq.toList
    let executionStates = clockFill { clock = 1; regX = 1 } instructions

    executionStates
    |> Seq.filter (fun s -> s.clock >= 20 && (s.clock - 20) % 40 = 0)
    |> Seq.print
    |> Seq.map (fun s -> s.regX * s.clock)
    |> Seq.sum
    |> string

// The CRT screen is 40-wide and 6-high.
// The sprite is 3-full pixels and the X register will tell us where the middle
// pixel lies. So we'll have to clock the CRT "write pointer", pair it with the
// CPU clock, and check the position of the sprite at all times to see if we'll
// print a full pixel or an empty one.

let solve2 (inputReader: unit -> string[]) =
    let instructions =
        inputReader ()
        |> Seq.map parseInstruction
        |> Seq.toList
    let executionStates = clockFill { clock = 1; regX = 1 } instructions

    let pixels =
        seq {
            for state in executionStates do
                let xpos = ((state.clock - 1) % 40)
                if state.regX >= xpos - 1 && state.regX <= xpos + 1 then
                    yield "#"
                else
                    yield "."
                if xpos = 39 then yield "\n"
        }
        |> String.concat ""
    
    pixels