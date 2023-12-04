open System
open System.IO

type Card = {
    id: int
    winningNumbers: int list
    numbers: int list
}

module Card =
    let ofString (s: String) =
        let segments = s.Split(":")
        let cardSegment = segments[0]
        let numberSegment = segments[1]

        let id = cardSegment.TrimStart([| 'C'; 'a'; 'r'; 'd'; ' ' |]) |> int
        let numberSegments = numberSegment.Split("|")
        
        let winningNumbers = 
            numberSegments[0]
            |> _.Trim()
            |> _.Split(" ", StringSplitOptions.RemoveEmptyEntries)
            |> Seq.map int
            |> Seq.toList

        let numbers =
            numberSegments[1]
            |> _.Trim()
            |> _.Split(" ", StringSplitOptions.RemoveEmptyEntries)
            |> Seq.map int
            |> Seq.toList
        
        { id = id; winningNumbers = winningNumbers; numbers = numbers }
    
    let getValue (c: Card) =
        let winners = List.filter (fun x -> List.contains x c.winningNumbers) c.numbers
        let count = winners.Length
        match count with
        | 0 -> 0
        | 1 -> 1
        | n -> Math.Pow(float 2, (float n - 1.0)) |> int

    let getWinningCount (c: Card) =
        let winners = List.filter (fun x -> List.contains x c.winningNumbers) c.numbers
        winners.Length

let rec getWinningCards' (allCards: Card list) (cards: Card list) (cardMap: Map<int, bigint>) (acc: bigint) =
    match cards with
    | [] -> acc
    | card :: rest ->
        match Map.exists (fun k _ -> k = card.id) cardMap with
            | true -> cardMap[card.id] + acc
            | false ->
                let winningCount = Card.getWinningCount card
                let cardIds = [1 .. winningCount] |> List.map (fun x -> x + card.id)
                let newCards = 
                    Map.filter (fun k _ -> List.contains k cardIds) cardMap
                    |> Map.values
                    |> Seq.sum
                let newAcc = acc + newCards + 1I
                let newCardMap = Map.add card.id (newCards + 1I) cardMap
                printfn $"Card {card.id} has {winningCount} winning numbers: %A{cardIds} cache value: {newCards + 1I}"
                printfn $"Cached Values: {newCards}: {newCardMap}"
                getWinningCards' allCards rest newCardMap newAcc

let input =
    Path.Join(__SOURCE_DIRECTORY__, "input.txt")
    |> File.ReadAllLines 
    |> Seq.map Card.ofString

let getWinningCards =
    let cList = input |> Seq.toList
    getWinningCards' cList (List.rev cList) Map.empty 0I



input
|> Seq.map Card.getValue
|> Seq.sum
|> fun x -> printfn $"Part1: {x}"

getWinningCards
|> fun x -> printfn $"Part2: {x}" 
