open System

let eps = 10.0**(-9.0)
let a = 0.0
let b = 0.5
let step = 0.05

let builtin (x: float) =
    let nextTerm n =
        let term = -(1.0 + 2.0 / (3.0 ** float (n + 1))) * (x ** float n)
        Some(term, n + 1)
    
    let terms = Seq.unfold nextTerm 0 |> Seq.takeWhile (fun t -> abs t > eps) |> Seq.toList
    (Seq.sum terms, List.length terms)

let smarttaylor(x: float) =
    let mutable part1 = -1.0
    let mutable part2 = -2.0 / 3.0
    let mutable result = 0.0
    let mutable count = 0
    let mutable element = part1 + part2

    while abs(part1 + part2) > eps do
        result <- result + part1 + part2 
        count <- count + 1
        part1 <- part1 * x            
        part2 <- part2 * (x / 3.0)
        element <- part1 + part2  
    (result, count)

let dumbtaylor (x: float) =
    let mutable result = 0.0
    let mutable n = 1.0
    let mutable element = -(1.0 + 2.0 / 3.0)
    let mutable count = 0
    
    while abs(element) > eps do
        result <- result + element
        count <- count + 1
        element <- -(1.0 + 2.0 / (3.0**(n + 1.0)))*(x**n)
        n <- n + 1.0
    (result, count)

[<EntryPoint>]
let main argv =
    printfn "| %8s | %12s | %12s | %8s | %12s | %8s |" "x" "Builtin" "Smart Taylor" "# terms" "Dumb Taylor" "# terms"
    printfn "|%s|%s|%s|%s|%s|%s|" (String.replicate 10 "-") (String.replicate 14 "-") (String.replicate 14 "-") (String.replicate 10 "-") (String.replicate 14 "-") (String.replicate 10 "-")
    
    let mutable i = a
    while i <= b + eps do
        let (dumb, dumbTerms) = dumbtaylor i
        let (smart, smartTerms) = smarttaylor i
        let (built, builtTerms) = builtin i
        
        printfn "| %8.4f | %12.9f | %12.9f | %8d | %12.9f | %8d |" 
            i built smart smartTerms dumb dumbTerms
        i <- i + step
    
    0