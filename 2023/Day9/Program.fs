open System.IO


let rec findDifference' (arr: int[]) (acc: int[]): int[] =
    match arr with
    |  [| _ |] | [||] -> acc
    | _ ->
        let first = arr.[0]
        let second = arr.[1]
        let diff = second - first
        let newArr = Array.append acc [| diff |]
        findDifference' (Array.tail arr) newArr

let findDifference arr = findDifference' arr [||]

let rebuildArray (arr: int[]) (n: int) =
    Array.append arr [| (Array.last arr) + n|]

let rec findAllDifferences' (arr: int[]) (acc: int[][]): int[][] =
    let acc = Array.append acc [| arr |]
    if arr |> Array.forall (fun x -> x = 0) then
        acc
    else
        let diffs = Array.pairwise arr |> Array.map (fun (a, b) -> b - a)
        findAllDifferences' diffs acc
let findAllDifferences arr = findAllDifferences' arr [||]


let predictNext (arrs: int[][]): int[][] =
    let rec processArrays lastPredictedItem remainingArrays acc =
        match remainingArrays with
        | [||] -> acc
        | _ ->
            let arr = remainingArrays.[0]
            let arr = rebuildArray arr lastPredictedItem
            let lastPredictedItem = if Array.length arr = 1 then 0 else Array.last arr
            let acc = Array.append acc [| arr |]
            processArrays lastPredictedItem (Array.sub remainingArrays 1 (Array.length remainingArrays - 1)) acc
    processArrays 0 (Array.rev arrs) [||]


let prependPredictedItem arr predictedItem =
    match arr with
    | [||] -> [| predictedItem |]
    | _ -> Array.append [| arr.[0] - predictedItem |] arr

let predictPrevious (arrs: int[][]): int[][] =
    let rec processArrays nextPredictedItem remainingArrays acc =
        match remainingArrays with
        | [||] -> acc
        | _ ->
            let arr = remainingArrays.[0]
            let arr = prependPredictedItem arr nextPredictedItem
            let nextPredictedItem = if Array.length arr = 1 then 0 else Array.head arr
            let acc = Array.append [| arr |] acc
            processArrays nextPredictedItem (Array.sub remainingArrays 1 (Array.length remainingArrays - 1)) acc
    processArrays 0 (Array.rev arrs) [||]
let input =
    Path.Join(__SOURCE_DIRECTORY__, "input.txt")
    |> File.ReadAllLines
    |> Array.map (fun line -> line.Split(' '))
    |> Array.map (Array.map int)

input
|> Seq.map findAllDifferences
|> Seq.map predictNext
|> Seq.map (Seq.last >> Seq.last)
|> Seq.sum
|> printfn "Part 1: %A"

input
|> Seq.map findAllDifferences
|> Seq.map predictPrevious
|> Seq.map (Seq.head >> Seq.head)
|> Seq.sum
|> printfn "Part 2: %A"

