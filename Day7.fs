module Day7
open Utils

// we need to infer the directory and file structure from the terminal log
// lines starting with $ mean a command
// other lines are outputs from ls, containing a number and a filename

type ConsoleLine =
    | Cd of string
    | Ls
    | Dir of string
    | File of int * string

type DirEntry =
    | DirEntry of string
    | FileEntry of int * string

// parse a single line from the console interaction
let parseLine (line: string) =
    match line.Split ' ' with
    | [| "$"; "cd"; dir |]  -> Cd dir
    | [| "$"; "ls" |]       -> Ls
    | [| "dir"; dir |]      -> Dir dir
    | [| num; filename |]   -> File (int num, filename)
    | _                     -> failwith "invalid line"

// Go through the console interactions and create the filesystem structure.
// We use a map from directory path to directory content.
// We index the directories as a list to allow easy push/pop when entering/leaving.
let linesToDirEntries (lines: string list) =
    let rec l2e dirs curDir lines =
        match lines with
        | [] -> dirs
        | line :: lines ->
            match parseLine line with
            | Cd dir -> 
                match dir with
                | "/"  -> l2e dirs [] lines
                | ".." -> l2e dirs (List.tail curDir) lines
                | _    -> l2e dirs (dir :: curDir) lines
            | Ls -> l2e dirs curDir lines
            | Dir dir ->
                l2e
                    (dirs |> Map.add (dir::curDir) [] |> Map.add curDir ((DirEntry dir) :: dirs.[curDir]))
                    curDir lines
            | File (num, filename) ->
                l2e (Map.add curDir (FileEntry (num, filename) :: dirs.[curDir]) dirs) curDir lines
    l2e (Map.singleton [] []) [] lines

// Given the filesystem structure, calculate the size of each directory.
// Accumulate the results in a map from directory path to size.
let dirEntriesToSizes startDir (dirEntries: Map<string list, DirEntry list>) =
    let rec d2s curDir content sizes =
        match content with
        | [] -> sizes
        | (FileEntry (num, _))::content ->
            d2s curDir content (Map.add curDir (num + sizes.[curDir]) sizes)
        | (DirEntry dir)::content ->
            // when we get a directory, we first recurse into it and then use the result
            // as a new accumulator for the current directory
            let nextDir = dir::curDir
            let sizes' = d2s curDir content (d2s (nextDir) (dirEntries.[nextDir]) (Map.add (nextDir) 0 sizes))
            sizes'
            |> Map.add curDir (sizes'.[nextDir] + sizes'.[curDir])
    d2s startDir (dirEntries.[startDir]) (Map.singleton startDir 0)

// sum all directory sizes up to 100000
let solve1 (inputReader: unit -> string[]) =
    let sizes =
        inputReader ()
        |> Seq.toList
        |> linesToDirEntries
        |> dirEntriesToSizes []
    sizes |> Map.values |> Seq.filter (fun x -> x < 100000) |> Seq.sum |> string

// considering total space 70000000
// and used space as the space taken by the root dir
// find the smallest directory that deleted would free up enough space
// to get 30000000 free
let solve2 (inputReader: unit -> string[]) =
    let sizes =
        inputReader ()
        |> Seq.toList
        |> linesToDirEntries
        |> dirEntriesToSizes []
    let usedSpace = sizes.[[]]
    let freeSpace = 70000000 - usedSpace
    let targetSpace = 30000000
    sizes
    |> Map.values
    |> Seq.filter (fun x -> x + freeSpace > targetSpace)
    |> Seq.min
    |> string