open System
open System.Numerics
open System.IO

type Translation =
    | SeedToSoil
    | SoilToFertilizer
    | FertilizerToWater
    | WaterToLight
    | LightToTemperature
    | TemperatureToHumidity
    | HumidityToLocation

type Entry = {
    source: bigint
    destination: bigint
    range: bigint
    category: Translation
}

type Data = {
    entries: Entry list
    seeds: bigint list
}

type Range = {
    start: bigint
    stop: bigint
}


type RangeData = {
    entries: Entry list
    ranges: Range list
}

let overlaps entry range =
    let entryStop = entry.source + entry.range - 1I
    not (entry.source > range.stop || entryStop < range.start)        
let mapRangeToEntries entries range =
    let overlappingEntries = entries |> List.filter (fun entry -> overlaps entry range)
    if List.isEmpty overlappingEntries then [range] // If no entries overlap with the range, return the original range
    else
        let mappedRanges = overlappingEntries |> List.fold (fun acc entry ->
                let overlapStart = max entry.source range.start
                let overlapStop = min (entry.source + entry.range - 1I) range.stop
                let mapped = { start = entry.destination + (overlapStart - entry.source); stop = entry.destination + (overlapStop - entry.source) }
                acc @ [mapped]) []
        let minOverlapStart = overlappingEntries |> List.map (fun entry -> entry.source) |> List.min
        let maxOverlapStop = overlappingEntries |> List.map (fun entry -> entry.source + entry.range - 1I) |> List.max
        let beforeOverlap = if minOverlapStart > range.start then [{ start = range.start; stop = minOverlapStart - 1I }] else []
        let afterOverlap = if maxOverlapStop < range.stop then [{ start = maxOverlapStop + 1I; stop = range.stop }] else []
        mappedRanges @ beforeOverlap @ afterOverlap       

        
let mapRanges (rangeData: RangeData) =
    let translations = [SeedToSoil; SoilToFertilizer; FertilizerToWater; WaterToLight; LightToTemperature; TemperatureToHumidity; HumidityToLocation]
    let rec map (translations: Translation list) (rangeData: RangeData) =
        match translations with
        | [] -> rangeData
        | translation :: rest ->
            let entriesForCurrentTranslation = List.filter (fun entry -> entry.category = translation) rangeData.entries
            let mappedRanges = rangeData.ranges |> List.collect (mapRangeToEntries entriesForCurrentTranslation)
            map rest { rangeData with ranges = mappedRanges }
    map translations rangeData
    
let findLowestLocation (rangeData: RangeData) =
    let finalRangeData = mapRanges rangeData

    finalRangeData.ranges
    |> List.minBy (fun range -> range.start)

module Entry =
    let ofString (t: Translation ) (s: string) =
        let nums = s |> _.Split(" ") |> Array.map (BigInteger.Parse)
        { source = nums.[1]; destination = nums.[0]; range = nums[2]; category = t }

module Data =
    let ofString (data: string array) =
        // get seeds
        let seeds = Seq.head data |> _.TrimStart("sed: ".ToCharArray()) |> _.Split(" ") |> Seq.map (BigInteger.Parse) |> Seq.toList
        
        // get seed to soil
        let data = Array.toList data
        let data = List.tail data |> List.skip 2
        let toSoils = List.takeWhile (String.IsNullOrWhiteSpace >> not) data
        let entries = toSoils |> List.map (Entry.ofString SeedToSoil)
        
        // get soil to fertilizer
        let data = List.skip (entries.Length + 2)  data
        let toFertilizers = List.takeWhile (String.IsNullOrWhiteSpace >> not) data
        let entries = entries @ List.map (Entry.ofString SoilToFertilizer) toFertilizers

        // get fertilizer to water
        let data = List.skip (toFertilizers.Length + 2) data
        let toWaters = List.takeWhile (String.IsNullOrWhiteSpace >> not) data
        let entries = entries @ List.map (Entry.ofString FertilizerToWater) toWaters

        // get water to light
        let data = List.skip (toWaters.Length + 2) data
        let toLights = List.takeWhile (String.IsNullOrWhiteSpace >> not) data
        let entries = entries @ List.map (Entry.ofString WaterToLight) toLights

        // get light to temperature
        let data = List.skip (toLights.Length + 2) data
        let toTemperatures = List.takeWhile (String.IsNullOrWhiteSpace >> not) data
        let entries = entries @ List.map (Entry.ofString LightToTemperature) toTemperatures

        // get temperature to humidity
        let data = List.skip (toTemperatures.Length + 2) data
        let toHumidities = List.takeWhile (String.IsNullOrWhiteSpace >> not) data
        let entries = entries @ List.map (Entry.ofString TemperatureToHumidity) toHumidities

        // get humidity to location
        let data = List.skip (toHumidities.Length + 2) data
        let toLocations = List.takeWhile (String.IsNullOrWhiteSpace >> not) data
        let entries = entries @ List.map (Entry.ofString HumidityToLocation) toLocations

        { entries = entries; seeds = seeds}

    
    let toRange (data: Data) =
            let ranges = 
                data.seeds
                |> List.chunkBySize 2
                |> List.map (function
                    | [start; length] -> { start = start; stop = start + length - 1I }
                    | _ -> failwith "Invalid seed data")
            { entries = data.entries; ranges = ranges; }

    let SeedToSoil (data: Data) (seed: bigint) =
        let map = List.tryFind (fun (x: Entry) -> x.category = SeedToSoil && x.source <= seed && (x.source + x.range) >= seed ) data.entries
        match map with
        | Some x -> x.destination + (seed - x.source)
        | None -> seed

    let SoilToFertilizer (data: Data) (soil: bigint) =
        let map = List.tryFind (fun (x: Entry) -> x.category = SoilToFertilizer && x.source <= soil && (x.source + x.range) >= soil ) data.entries
        match map with
        | Some x -> x.destination + (soil - x.source)
        | None -> soil
    
    let FertilizerToWater (data: Data) (fertilizer: bigint) =
        let map = List.tryFind (fun (x: Entry) -> x.category = FertilizerToWater && x.source <= fertilizer && (x.source + x.range) >= fertilizer ) data.entries
        match map with
        | Some x -> x.destination + (fertilizer - x.source)
        | None -> fertilizer

    let WaterToLight (data: Data) (water: bigint) =
        let map = List.tryFind (fun (x: Entry) -> x.category = WaterToLight && x.source <= water && (x.source + x.range) >= water ) data.entries
        match map with
        | Some x -> x.destination + (water - x.source)
        | None -> water
    
    let LightToTemperature (data: Data) (light: bigint) =
        let map = List.tryFind (fun (x: Entry) -> x.category = LightToTemperature && x.source <= light && (x.source + x.range) >= light ) data.entries
        match map with
        | Some x -> x.destination + (light - x.source)
        | None -> light
    
    let TemperatureToHumidity (data: Data) (temperature: bigint) =
        let map = List.tryFind (fun (x: Entry) -> x.category = TemperatureToHumidity && x.source <= temperature && (x.source + x.range) >= temperature ) data.entries
        match map with
        | Some x -> x.destination + (temperature - x.source)
        | None -> temperature
    
    let HumidityToLocation (data: Data) (humidity: bigint) =
        let map = List.tryFind (fun (x: Entry) -> x.category = HumidityToLocation && x.source <= humidity && (x.source + x.range) >= humidity ) data.entries
        match map with
        | Some x -> x.destination + (humidity - x.source)
        | None -> humidity

    let SeedToLocation (data: Data) (seed: bigint) =
        SeedToSoil data seed
        |> SoilToFertilizer data
        |> FertilizerToWater data
        |> WaterToLight data
        |> LightToTemperature data
        |> TemperatureToHumidity data
        |> HumidityToLocation data

let input =
    Path.Join(__SOURCE_DIRECTORY__, "input.txt")
    |> File.ReadAllLines
    |> Data.ofString

// For more information see https://aka.ms/fsharp-console-apps
input
|> _.seeds
|> Seq.map (fun x -> Data.SeedToLocation input x)
|> Seq.min
|> fun x -> printfn $"Part 1: {x}"
// |> Seq.iter (fun x -> printfn $"{x}")

input
|> Data.toRange
|> findLowestLocation
|> (fun x -> printfn $"Part 2:{x.start} {x.stop}")

