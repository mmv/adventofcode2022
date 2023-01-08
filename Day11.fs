module Day11
open Utils

// The input for this particular problem is one of the weirdest formats so far.
// It goes like this
// ```
// Monkey 0:
//   Starting items: 79, 98
//   Operation: new = old * 19
//   Test: divisible by 23
//     If true: throw to monkey 2
//     If false: throw to monkey 3
// ```
// Fortunately the lines will always come ordered the same way.
type Monkey = {
    StartingItems: bigint list
    Operation: bigint -> bigint
    TestDivisibleBy: int
    TestIfTrue: int
    TestIfFalse: int
}

let buildOperation (opdesc: string) =
    match opdesc.Split(" ") with
    | [| "old"; op; y |] ->
        let partialFn (v: bigint) =
            match op with
            | "+" -> (+) v
            | "-" -> (-) v
            | "*" -> (*) v
            | "/" -> (/) v
            | _ -> failwith "Invalid operation"
        if y = "old"
        then (fun v -> partialFn v v)
        else (fun v -> partialFn v (int y |> bigint))
    | _ -> failwith "Invalid operation description"

// take a single monkey definition and produce the Monkey item
let parseMonkey (lines: string array) =
    let startingItems =
        lines.[1].Split(": ").[1].Split(", ")
        |> Seq.map (int >> bigint)
        |> Seq.toList
    let operation = buildOperation (lines.[2].Split("new = ").[1])
    let testDivisibleBy = int (lines.[3].Split("divisible by ").[1])
    let testIfTrue = int (lines.[4].Split("throw to monkey ")[1])
    let testIfFalse = int (lines.[5].Split("throw to monkey ")[1])
    { StartingItems = startingItems
      Operation = operation
      TestDivisibleBy = testDivisibleBy
      TestIfTrue = testIfTrue
      TestIfFalse = testIfFalse }

// processing a turn for a single monkey works as follows:
// - take the first item from the list and check the level
// - apply the opreation to the level
// - divide the result by 3
// - perform the test and throw item to appropriate target
// (item is put at the end of target monkey's items)
// if there were no items in the monkey, nothing is done in
// this turn
let rec processTurnSingleMonkey adjuster idx monkeys itemCounter =
    let monkey = monkeys |> Map.find idx
    match monkey.StartingItems with
    | [] -> monkeys, itemCounter
    | x :: xs ->
        let newLevel = monkey.Operation x
        let adjLevel: bigint = (adjuster newLevel)
        let target =
            if adjLevel % (bigint monkey.TestDivisibleBy) = 0I
            then monkey.TestIfTrue
            else monkey.TestIfFalse
        let monkeys' =
            monkeys
            |> Map.add idx { monkey with StartingItems = xs }
            |> Map.add target {
                monkeys.[target] with
                    StartingItems = monkeys.[target].StartingItems @ [adjLevel]
            }
        let itemCounter' = itemCounter |> Map.add idx (itemCounter.[idx] + 1)
        processTurnSingleMonkey adjuster idx monkeys' itemCounter'

let solve1 (inputReader: unit -> string[]) =
    let monkeys =
        inputReader ()
        |> batchSplit
        |> Seq.map (Seq.toArray >> parseMonkey)
        |> Seq.mapi (fun i m -> i, m)
        |> Map.ofSeq
    let itemCounter = monkeys |> Map.map (fun _ _ -> 0)
    Seq.init 20 id
    |> Seq.fold (fun (monkeys, itemCounter) _ ->
        Seq.init (monkeys |> Map.count) id
        |> Seq.fold (fun (monkeys, itemCounter) idx ->
            processTurnSingleMonkey (fun x -> x/3I) idx monkeys itemCounter
        ) (monkeys, itemCounter)
    ) (monkeys, itemCounter)
    |> snd
    |> Map.values
    |> Seq.sortDescending
    |> Seq.take 2
    |> Seq.reduce (*)
    |> string

// TODO: implement
let solve2 (inputReader: unit -> string[]) =
    let monkeys =
        inputReader ()
        |> batchSplit
        |> Seq.map (Seq.toArray >> parseMonkey)
        |> Seq.mapi (fun i m -> i, m)
        |> Map.ofSeq
    let itemCounter = monkeys |> Map.map (fun _ _ -> 0)

    // I tried running this with the 10k iterations but it just got too slow.
    // We can observe that what really matters is not the final values but just
    // enough of the value to understand how the items will be distributed through
    // the monkeys. That means, we just need to keep enough information to do
    // the test. Since the test is always "divisible by X" where X is always a
    // prime number, we can just keep the modulo of the value with the product
    // of all the Xs. This will give us the same result as if we had run the
    // the full calculations, but without stressing too much the bigint logic, which
    // means we end up saving a lot of CPU cycles and end up finishing in a reasonable
    // amount of time.
    let capLimit = monkeys |> Map.values |> Seq.map (fun m -> m.TestDivisibleBy) |> Seq.reduce (*) |> bigint
    let capper = (fun x -> x % capLimit)
    
    Seq.init 10000 id
    |> Seq.fold (fun (monkeys, itemCounter) _ ->
        Seq.init (monkeys |> Map.count) id
        |> Seq.fold (fun (monkeys, itemCounter) idx ->
            processTurnSingleMonkey capper idx monkeys itemCounter
        ) (monkeys, itemCounter)
    ) (monkeys, itemCounter)
    |> snd
    |> Map.values
    |> Seq.print
    |> Seq.sortDescending
    |> Seq.take 2
    |> Seq.map bigint
    |> Seq.reduce (*)
    |> string