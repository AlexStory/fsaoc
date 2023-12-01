open System
open System.IO

let replaceDigits (s: string) =
    s.Replace("one", "o1e")
    |> _.Replace("two", "t2o")
    |> _.Replace("three", "t3e")
    |> _.Replace("four", "f4r")
    |> _.Replace("five", "f5e")
    |> _.Replace("six", "s6x")
    |> _.Replace("seven", "s7n")
    |> _.Replace("eight", "e8t")
    |> _.Replace("nine", "n9e")

let input =
    Path.Join(__SOURCE_DIRECTORY__, "input.txt")
    |> File.ReadAllLines // ["1bct2", ...]
    |> Seq.map (String.collect (fun c -> if Char.IsDigit(c)  then c.ToString() else "" )) // ["12", "38" ...]
    |> Seq.map _.ToCharArray()
    |> Seq.map (fun x -> (Array.head x |> _.ToString()) + (Array.last x |> _.ToString()))
    |> Seq.map  int

let input2 =
    Path.Join(__SOURCE_DIRECTORY__, "input.txt")
    |> File.ReadAllLines // ["1bct2", ...]
    |> Seq.map replaceDigits
    |> Seq.map (String.collect (fun c -> if Char.IsDigit(c)  then c.ToString() else "" )) // ["12", "38" ...]
    |> Seq.map (_.ToCharArray())
    |> Seq.map (fun x -> (Array.head x |> _.ToString()) + (Array.last x |> _.ToString()))
    |> Seq.map int

printfn $"Part 1: %d{Seq.sum input}"
printfn $"Part 2: %d{Seq.sum input2}"
