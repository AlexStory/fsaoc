open System
open System.IO

type Card =
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace

type WildCard =
    | Joker
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Queen
    | King
    | Ace

type Hand = {
    cards: Card list
    bet: int
}

type WildHand = {
    cards: WildCard list
    bet: int
}

type HandType = 
    | HighCard
    | Pair
    | TwoPair
    | ThreeOfAKind
    | FullHouse
    | FourOfAKind
    | FiveOfAKind

type Score = {
    handType: HandType
    hand: Hand
}

type WildScore = {
    handType: HandType
    hand: WildHand
}

module Card =
    let ofChar (s: char) : Card =
        match s with
        | '2' -> Card.Two
        | '3' -> Card.Three
        | '4' -> Card.Four
        | '5' -> Card.Five
        | '6' -> Card.Six
        | '7' -> Card.Seven
        | '8' -> Card.Eight
        | '9' -> Card.Nine
        | 'T' -> Card.Ten
        | 'J' -> Card.Jack
        | 'Q' -> Card.Queen
        | 'K' -> Card.King
        | 'A' -> Card.Ace
        | _ -> failwith "Invalid card"

module WildCard =
    let ofChar (s: char) : WildCard =
        match s with
        | 'J' -> WildCard.Joker
        | '2' -> WildCard.Two
        | '3' -> WildCard.Three
        | '4' -> WildCard.Four
        | '5' -> WildCard.Five
        | '6' -> WildCard.Six
        | '7' -> WildCard.Seven
        | '8' -> WildCard.Eight
        | '9' -> WildCard.Nine
        | 'T' -> WildCard.Ten
        | 'Q' -> WildCard.Queen
        | 'K' -> WildCard.King
        | 'A' -> WildCard.Ace
        | _ -> failwith "Invalid card"

module Hand =
    let ofString (s: string) : Hand =
        let cards = s.Split(' ').[0]
        let bet = s.Split(' ').[1]
        { cards = cards |> Seq.map Card.ofChar |> Seq.toList; bet = int bet }

    let scoreHand (hand: Hand) : Score =
        let cards = hand.cards
        let cardCounts = cards |> Seq.groupBy id |> Seq.map (fun (card, cards) -> (card, Seq.length cards)) |> Seq.toList
        let sortedCardCounts = cardCounts |> Seq.sortByDescending snd |> Seq.toList
        let handType =
            match sortedCardCounts with
            | [ (_, 5) ] -> FiveOfAKind
            | [ (_, 4); (_, 1) ] -> FourOfAKind
            | [ (_, 3); (_, 2) ] -> FullHouse
            | [ (_, 3); (_, 1); (_, 1) ] -> ThreeOfAKind
            | [ (_, 2); (_, 2); (_, 1) ] -> TwoPair
            | [ (_, 2); (_, 1); (_, 1); (_, 1) ] -> Pair
            | [ (_, 1); (_, 1); (_, 1); (_, 1); (_, 1) ] -> HighCard
            | _ -> failwith "Invalid hand"
        { handType = handType; hand = hand }


module WildHand =
    let ofString (s: string) : WildHand =
        let cards = s.Split(' ').[0]
        let bet = s.Split(' ').[1]
        { cards = cards |> Seq.map WildCard.ofChar |> Seq.toList; bet = int bet }


    let scoreHand (hand: WildHand) : WildScore =
        let jokers = hand.cards |> Seq.filter (fun card -> card = Joker) |> Seq.length
        if jokers = 5 then
            { handType = FiveOfAKind; hand = hand }
        else
            let cards = hand.cards |> Seq.filter (fun card -> card <> Joker)
            let cardCounts = cards |> Seq.groupBy id |> Seq.map (fun (card, cards) -> (card, Seq.length cards)) |> Seq.toList
            let sortedCardCounts = cardCounts |> Seq.sortByDescending snd |> Seq.toList
            let sortedCardCounts = 
                match sortedCardCounts with
                | (card, count) :: tail -> (card, count + jokers) :: tail
                | [] -> []
            let handType =
                match sortedCardCounts with
                | [ (_, 5) ] -> FiveOfAKind
                | [ (_, 4); (_, 1) ] -> FourOfAKind
                | [ (_, 3); (_, 2) ] -> FullHouse
                | [ (_, 3); (_, 1); (_, 1) ] -> ThreeOfAKind
                | [ (_, 2); (_, 2); (_, 1) ] -> TwoPair
                | [ (_, 2); (_, 1); (_, 1); (_, 1) ] -> Pair
                | [ (_, 1); (_, 1); (_, 1); (_, 1); (_, 1) ] -> HighCard
                | _ -> failwith "Invalid hand"
            { handType = handType; hand = hand }        
        
let input =
    Path.Join(__SOURCE_DIRECTORY__, "input.txt")
    |> File.ReadAllLines
    |> Array.map Hand.ofString

let wildInput =
    Path.Join(__SOURCE_DIRECTORY__, "input.txt")
    |> File.ReadAllLines
    |> Array.map WildHand.ofString


input
|> Array.map Hand.scoreHand
|> Array.sort
|> Array.mapi (fun i s -> (i + 1) * s.hand.bet)
|> Array.sum
|> printfn "Part 1: %A"


wildInput
|> Array.map WildHand.scoreHand
|> Array.sort
|> Array.mapi (fun i s -> (i + 1) * s.hand.bet)
|> Array.sum
|> printfn "Part 2: %A"

