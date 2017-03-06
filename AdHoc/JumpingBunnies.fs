module JumpingBunnies

let rec gcd (a:bigint) (b:bigint) = 
    if b = 0I then a
    else gcd b (a%b)


