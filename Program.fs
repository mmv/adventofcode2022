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
    (8, Day8.solve1) ; (8, Day8.solve2)
    (9, Day9.solve1) ; (9, Day9.solve2)
    (10, Day10.solve1) ; (10, Day10.solve2)
    (11, Day11.solve1) ; (11, Day11.solve2)
    (12, Day12.solve1) ; (12, Day12.solve2)
    (13, Day13.solve1) ; (13, Day13.solve2)
    (14, Day14.solve1) ; (14, Day14.solve2)
    (15, Day15.solve1) ; (15, Day15.solve2)
    (16, Day16.solve1) ; (16, Day16.solve2)
    (17, Day17.solve1) ; (17, Day17.solve2)
]

let (day, solverDuJour) = Seq.last solvers

printfn "%s" (solverDuJour (fun () -> readSampleLines day))
printfn "%s" (solverDuJour (fun () -> readLines day))