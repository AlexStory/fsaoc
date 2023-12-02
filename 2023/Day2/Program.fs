open System
open System.IO

type Reds = int
type Greens = int
type Blues = int

type Pull = {
    reds: Reds
    greens: Greens
    blues: Blues
}

type Game = {
    id: int
    pulls: Pull list
}

let redLimit = 12
let greenLimit = 13
let blueLimit = 14

let parsePull (s: string): Pull =
    //3 blue, 4 red
    let pulls = s.Split(",")
    let red : string option = Array.tryFind(_.EndsWith("red")) pulls
    let blue : string option = Array.tryFind(_.EndsWith("blue")) pulls
    let green : string option = Array.tryFind(_.EndsWith("green")) pulls

    let reds: Reds = 
        match red with
        | None -> 0
        | Some r -> r.TrimEnd([|' '; 'r'; 'e'; 'd'|]) |> int

    let greens: Greens = 
        match green with
        | None -> 0
        | Some g -> g.TrimEnd([|' '; 'g'; 'r'; 'e'; 'n'|])|> int
    
    let blues: Blues = 
        match blue with
        | None -> 0
        | Some b -> b.TrimEnd([|' '; 'b'; 'l'; 'u'; 'e'|]) |> int

    {reds = reds; greens = greens; blues = blues}


let parseGame (s: string) =
    //Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
    let results = s.Split(":")
    let gameId = results[0]
    let pulls = results[1]
    let gameId = gameId.TrimStart([|'G'; 'a'; 'm'; 'e'; ' '|]) |> int
    
    let pulls = 
        pulls.Split(";")
        |> Seq.map parsePull
        |> Seq.toList

    {
        id = gameId
        pulls = pulls
    }

let isValid game =
    List.forall (fun p -> p.reds <= redLimit && p.greens <= greenLimit && p.blues <= blueLimit) game.pulls 

let getPowers game =
    let reds = game.pulls |> List.maxBy _.reds
    let greens = game.pulls |> List.maxBy _.greens
    let blues = game.pulls |> List.maxBy _.blues
    reds.reds * greens.greens * blues.blues
    


let input =
    Path.Join(__SOURCE_DIRECTORY__, "input.txt")
    |> File.ReadAllLines
    |> Seq.map parseGame

let part1 =
    input
    |> Seq.filter isValid
    |> Seq.map _.id
    |> Seq.sum

let part2 =
    input
    |> Seq.map getPowers
    |> Seq.sum

printfn $"part 1: {part1}"
printfn $"part 2: {part2}"
