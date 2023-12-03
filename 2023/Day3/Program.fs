open System
open System.IO


let symbols = ['*'; '#'; '+'; '$'; '/'; '-'; '%'; '='; '@'; '&']
type Input = char array list
type Part = {
    row: int
    start: int
    stop: int
}

type Gear = {
    col: int
    row: int
    symbol: char
}

type Gears = Map<Gear, Part list>


module Part =
    let getValue (data: Input) (part: Part) =
        data.[part.row][part.start .. part.stop]
        //|> fun x -> printfn $"%A{x} row: {part.row} start: {part.start} stop: {part.stop}"; x
        |> String
        |> int
    
    let isValid (data: Input) (part: Part) =
        let minY = 0
        let maxY = data.Length - 1
        let minX = 0
        let maxX = data[0].Length - 1
        let row1 = data[max minY (part.row - 1)][max minX (part.start - 1) .. min maxX (part.stop + 1)] |> Array.toList
        let row2 = data[part.row][max minX (part.start - 1)] :: [data[part.row][min maxX (part.stop + 1)]]
        let row3 = data[min maxY (part.row + 1)][max minX (part.start - 1) .. min maxX (part.stop + 1)] |> Array.toList
        let allRows = List.concat [row1; row2; row3]
        List.exists (fun (c: Char) -> List.contains c symbols) allRows


let getMaxCol (part: Part) (input: Input) =
    let len = input[0].Length
    min (part.stop + 1) (len - 1)

let getMinCol (part: Part) (input: Input) =
    max 0 (part.start - 1)

let getMaxRow (part: Part) (input: Input) =
    let len = input.Length
    min (part.row + 1) (len - 1)

let getMinRow (part: Part) (input: Input) =
    max 0 (part.row - 1)


let rec getGears' (input: Input) (parts: Part list) (row: int) (col: int) (gears: Gears) : Gears =
    let gear = { row = row; col = col; symbol = '*'}
    let part = List.head parts
    let newPart = col = (getMaxCol part input) && row = (getMaxRow part input)
    let parts = if newPart then List.tail parts else parts
    
    let gears =
        if input[row][col] = '*' then
            match Map.exists (fun g _ -> g = gear) gears with
            | false -> Map.add gear [part] gears
            | true -> Map.change gear (fun v -> Option.map(fun l -> part :: l) v) gears
        else
            gears
    
    let nextRow = 
        if col = getMaxCol part input then
            row + 1 
        else 
            row

    let nextCol = 
        if col = (getMaxCol part input) then
            getMinCol part input
        else 
            col + 1
    

    if newPart && parts.IsEmpty then
        gears
    else 
        let nextRow = if newPart then getMinRow (List.head parts) input else nextRow
        let nextCol = if newPart then getMinCol (List.head parts) input else nextCol
        getGears' input parts nextRow nextCol gears
    
let getGears input parts =
    let row = getMinRow (List.head parts) input
    let col = getMinCol (List.head parts) input
    getGears' input parts row col Map.empty


let input: Input = 
    Path.Join(__SOURCE_DIRECTORY__, "input.txt")
    |> File.ReadAllLines
    |> Seq.map _.ToCharArray()
    |> Seq.toList


let rec findParts' (input: Input) (row: int) (col: int) (current: char list) (acc: Part list) =
    let rows = input.Length
    let cols = input[0].Length
    let token = input[row][col]
    let isDigit = Char.IsDigit(token)

    let getPart () =
        match col with
        | 0 -> { row = row - 1; start = cols - (List.length current); stop = cols}
        | _ -> {row = row; start = col - (List.length current); stop = col - 1}

    let acc = 
            if (not isDigit && not current.IsEmpty) || ( col = 0 && not current.IsEmpty) then
                acc @ [getPart ()]
            else
                acc
    let current =
        if col = 0 then
            []
        else 
            current

    let current = 
        match isDigit with
        | true -> current @ [token]
        | false -> []

    let nextRow, nextCol = 
        if col + 1 = cols then
            row + 1, 0
        else row, col + 1

    if nextRow = rows then
        if not current.IsEmpty then
            acc @ [{row = row; start = cols - (List.length current); stop = cols}]
        else
            acc
    else
        findParts' input nextRow nextCol current acc


let findParts = 
    findParts' input 0 0 [] []


// findParts    
// |> Seq.iter (fun x -> printfn $"{Part.getValue input x} {Part.isValid input x}") 

// Part1
findParts
|> Seq.filter (Part.isValid input)
|> Seq.map (Part.getValue input)
|> Seq.sum
|> fun x -> printfn $"Part 1: {x}"

// Part 2
getGears input findParts
|> Map.filter (fun _ l -> List.length l = 2)
|> Map.map (fun _ l -> List.map (Part.getValue input )l)
|> Map.map (fun _ l -> l[0] * l[1])
|> Map.values
|> Seq.sum
|> fun x -> printfn $"{x}"

