open Utils

/// Check a solver for a day against the sample input and an expected output
let checkSolver (solver: (unit -> string[]) -> obj) (day: int) (expected: string) =
    let result = solver (fun () -> readSampleLines day)
    if result.ToString() = expected then
        printfn "Solver for day %d passed!" day
    else
        printfn "Solver for day %d failed! Expected %s, got %s" day expected (result.ToString())

let solvers = [
    (1, Day1.solve1 >> string) ; (1, Day1.solve2 >> string)
    (2, Day2.solve1 >> string) ; (2, Day2.solve2 >> string)
    (3, Day3.solve1 >> string) ; (3, Day3.solve2 >> string)
    (4, Day4.solve1 >> string) ; (4, Day4.solve2 >> string)
    (5, Day5.solve1) ; (5, Day5.solve2)
    (6, Day6.solve1) ; (6, Day6.solve2)
    (7, Day7.solve1) ; (7, Day7.solve2)
]

let (day, solverDuJour) = Seq.last solvers

printfn "%A" (solverDuJour (fun () -> readSampleLines day))
printfn "%A" (solverDuJour (fun () -> readLines day))