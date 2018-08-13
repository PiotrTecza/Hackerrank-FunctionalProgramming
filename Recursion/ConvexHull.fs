module ConvexHull
open System.Collections.Generic

type Point = { X: int; Y: int; Angle:float; Dist:float }

let ccw p1 p2 p3 = (p2.X - p1.X)*(p3.Y - p1.Y) - (p2.Y - p1.Y)*(p3.X - p1.X)

let findMin points =
    let mutable min = points |> List.head
    for p in points do
        if(p.Y < min.Y || (p.Y = min.Y && p.X < min.X)) then
            min <- p
    min

let dist (p1,p2) = 
    pown (p2.X-p1.X) 2 + pown (p2.Y-p1.Y) 2
    |> float
    |> sqrt

let cos p1 p2 = 
    match p1 = p2 with
    | true -> 2.0
    | false ->
        let x = p2.X - p1.X |> float
        let r =
            pown (p2.X - p1.X) 2 + pown (p2.Y - p1.Y) 2
            |> float
            |> sqrt
        x/r

let perimeter (points:Point list) = 
    List.sumBy dist (points |> List.pairwise)

let mapPoints minPoint point  =
    {
        X = point.X;
        Y = point.Y;
        Angle = cos minPoint point;
        Dist = dist (minPoint,point)
    }

let filterColinear points =
    let list = 
        [
            let mutable max = List.head points
            for point in points do
                if point.Angle = max.Angle then
                    if point.Dist > max.Dist then max <- point
                else
                    yield max
                    max <- point
            yield max
        ]
    list

let grahamScan points =
    let stack = Stack<Point>()
    let min = findMin points
    let sorted = 
        points
        |> List.map (mapPoints min)
        |> List.sortByDescending(fun p -> p.Angle)
        |> filterColinear
    let matrix = sorted @ [List.head sorted]
    let mutable counter = 0
    for point in matrix do
        match counter < 3 with
        | true -> 
            counter <- counter + 1
            stack.Push point
        | false -> 
            let mutable top = stack.Pop()
            let mutable top2 = stack.Pop()
            while ccw top2 top point <= 0 do
                top <- top2
                top2 <- stack.Pop()
            stack.Push top2
            stack.Push top
            stack.Push point
    let finalList = stack |> List.ofSeq
    perimeter finalList
