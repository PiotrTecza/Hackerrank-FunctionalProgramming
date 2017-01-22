module SwapNodes
open System.Collections.Generic
open System

[<AllowNullLiteral>]
type Node(x: int) = 
    let value = x
    let mutable left:Node = null
    let mutable right:Node = null

    member this.Left with get()= left
                      and set newValue = left <- newValue
    member this.Right with get()= right
                      and set newValue = right <- newValue
    member this.Value = value

    member this.Swap depth current =
        if current % depth = 0 then
            let temp = left
            this.Left <- this.Right
            this.Right <- temp
        if not (isNull left) then left.Swap depth (current + 1)
        if not (isNull right) then right.Swap depth (current + 1)

    member this.InorderTraverse() = 
        if not (isNull left) then
            left.InorderTraverse()
        printf "%d " value
        if not (isNull right) then
            right.InorderTraverse()

let BuildTree inputList= 
    let root = new Node(1)
    let nodeList = new List<Node>()
    nodeList.Add(root);
    let mutable index = 0;

    for i in inputList do
        let current = nodeList.[0]     
        
        if index % 2 = 0 then
            if i > 0 then
                current.Left <- new Node(i)
                nodeList.Add(current.Left)
            index <- index + 1                
        else
            if i > 0 then
                current.Right <- new Node(i)
                nodeList.Add(current.Right)
            index <- index + 1
            nodeList.RemoveAt(0)
    root 

let Run() = 
    let nodeCount = Console.ReadLine() |> int
    let mutable nodeString = ""
    for i in 1..nodeCount do
        nodeString <- nodeString + " " + Console.ReadLine()
    let nodes = 
        nodeString.Split([|" "|], StringSplitOptions.RemoveEmptyEntries) |>
        Array.map int
    
    let root = BuildTree nodes

    let transformationCount = Console.ReadLine() |> int

    for i in 1..transformationCount do
        let depth = Console.ReadLine() |> int
        root.Swap depth 1
        root.InorderTraverse()
        printfn ""

Run()

