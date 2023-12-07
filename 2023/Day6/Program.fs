open System
open System.IO
open System.Numerics

type Race = {
    time: bigint
    distance: bigint
}

type RaceData = {
    charge: bigint
    time: bigint
    distance: bigint
}

module Race =
    let getTimesForRace (race: Race) = 
        [1I..race.time]
        |> Seq.map (fun c -> race, {charge = c; time = (c + (race.time - c) * c); distance = c * (race.time - c)})

let parseRaces (lines: string[]) =
    let time = lines.[0].Split(" ", StringSplitOptions.RemoveEmptyEntries) |> Array.skip 1 |> Array.map BigInteger.Parse
    let distance = lines.[1].Split(" ", StringSplitOptions.RemoveEmptyEntries) |> Array.skip 1 |> Array.map BigInteger.Parse
    Array.zip time distance |> Array.map (fun (time, distance) -> {time = time; distance = distance})

let parseBigRace (lines: string[]) =
    let time = 
        lines.[0].Split(" ", StringSplitOptions.RemoveEmptyEntries) 
        |> Array.skip 1 
        |> fun x -> String.Join("", x) 
        |> BigInteger.Parse
    let distance = 
        lines.[1].Split(" ", StringSplitOptions.RemoveEmptyEntries) 
        |> Array.skip 1 
        |> fun x -> String.Join("", x)
        |> BigInteger.Parse

    {time = time; distance = distance}


let input =
    Path.Join(__SOURCE_DIRECTORY__, "input.txt")
    |> File.ReadAllLines
    |> parseRaces

let bigIngut =
    Path.Join(__SOURCE_DIRECTORY__, "input.txt")
    |> File.ReadAllLines
    |> parseBigRace

input
|> Seq.map Race.getTimesForRace
|> Seq.map (Seq.filter (fun (r: Race, d: RaceData) -> r.distance < d.distance))
|> Seq.map Seq.length
|> Seq.reduce (*)
|> printfn "Part 1: %A"
// |> Seq.iter (fun x -> printfn "%A" x)

bigIngut
|> Race.getTimesForRace
|> Seq.filter (fun (r: Race, d: RaceData) -> r.distance < d.distance)
|> Seq.length
|> printfn "Part 2: %A"
