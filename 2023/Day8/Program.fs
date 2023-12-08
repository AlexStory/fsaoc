open System
open System.IO

type Step = Left | Right
type Node = { 
    name: string
    left: string
    right: string
}

type Instructions = {
    steps: Step list
    nodes: Node list
}

module Instructions = 
    let ofString (lines: string array) =
        let steps = lines.[0].ToCharArray() |> Array.map (fun c -> if c = 'L' then Left else Right) |> Array.toList
        let nodes = 
            lines.[2..] |> Array.map (fun line ->
                let parts = line.Split('=')
                let name = parts.[0].Trim()
                let values = parts.[1].Trim().Replace("(", "").Replace(")", "").Split(',')
                let left = values.[0].Trim()
                let right = values.[1].Trim()
                { name = name; left = left; right = right }
            ) |> Array.toList
        { steps = steps; nodes = nodes }



    let rec followSteps' currentNode (instructions: Instructions) iterations =
        if currentNode = "ZZZ" then iterations
        else
            let node = instructions.nodes |> List.find (fun n -> n.name = currentNode)
            let step = instructions.steps.[iterations % List.length instructions.steps]
            let nextNode = if step = Left then node.left else node.right
            followSteps' nextNode instructions (iterations + 1)
   
    let followSteps (instructions: Instructions) = followSteps' "AAA" instructions 0

    let rec cycleLength startNode (instructions: Instructions) =
        let rec cycleLengthRec currentNode iterations =
            let node = instructions.nodes |> List.find (fun n -> n.name = currentNode)
            let step = instructions.steps.[int (iterations % bigint (List.length instructions.steps))]
            let nextNode = if step = Left then node.left else node.right
            if nextNode.EndsWith("Z") then iterations + 1I
            else cycleLengthRec nextNode (iterations + 1I)
        cycleLengthRec startNode 0I

    let lcm a b =
        let rec gcd a b = if b = 0I then a else gcd b (a % b)
        (a / gcd a b) * b

    let followGhostSteps (instructions: Instructions) =
        let startingNodes = instructions.nodes |> List.filter (fun n -> n.name.EndsWith("A")) |> List.map (fun n -> n.name)
        let cycleLengths = startingNodes |> List.map (fun node -> cycleLength node instructions)
        cycleLengths |> List.reduce lcm   


let input =
    Path.Join(__SOURCE_DIRECTORY__, "input.txt")
    |> File.ReadAllLines
    |> Instructions.ofString

input
|> Instructions.followSteps
|> printfn "Part 1: %A"

input
|> Instructions.followGhostSteps
|> printfn "Part 2: %A"
