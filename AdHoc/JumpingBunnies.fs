module JumpingBunnies
open System
open System.Numerics

let rec gcd (a:bigint) (b:bigint) = 
    if b = 0I then a
    else gcd b (a%b)

let lcm (a:bigint) (b:bigint) = a * b / gcd a b

let Run() = 
    let bunniesCount = Console.ReadLine() |> int
    let bunnies = 
        Console.ReadLine().Split([|" "|], StringSplitOptions.RemoveEmptyEntries) |>
        Array.map(fun x -> BigInteger.Parse(x))
    let mutable result = 1I
    for bunny in bunnies do
        result <- lcm result bunny
    printfn "%A" result

Run()

