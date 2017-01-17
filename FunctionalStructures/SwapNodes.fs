module SwapNodes

[<AllowNullLiteral>]
type Node(x: int) = 
    let value = x
    let mutable left:Node = null
    let mutable right:Node = null

    member this.Left with get()= left
                      and set newValue = left <- newValue

    member this.Right with get()= right
                      and set newValue = right <- newValue

    member this.Swap() =
        let temp = left
        this.Left <- this.Right
        this.Right <- temp

let BuildTree inputList:int*int = 
    let root = new Node(1)
    let mutable nodeList = [root]

    match inputList with
    | (l,r) -> 
        if l <> -1 then
            let left = new Node(l)
            nodeList.Head.Left <- left
            nodeList <- nodeList
        if r <> -1 then 
            let right = new Node(r)
            nodeList.Head.Right <- right

        nodeList <- nodeList.Tail