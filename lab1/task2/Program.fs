open System

let eps = 10.0**(-9.0)
let maxiter = 10000 

let func1 x = (3.0 * x - 5.0) / (x**2.0 - 4.0 * x + 3.0)
let func2 x = (sin x)**2.0
let func3 x = (1.0 - x**2.0 / 2.0)*(cos x) - (x / 2.0) * sin x

let g1 x = x - 0.01 * func1 x
let g2 x = x - 0.01 * func2 x  
let g3 x = x - 0.01 * func3 x 

let deriv1 x = 
    let num = 3.0 * (x**2.0 - 4.0*x + 3.0) - (3.0*x - 5.0)*(2.0*x - 4.0)
    let den = (x**2.0 - 4.0*x + 3.0)**2.0
    num / den

let deriv2 x = 2.0 * sin x * cos x

let deriv3 x = 
    let term1 = (-x) * cos x
    let term2 = (1.0 - x**2.0/2.0) * (-sin x)
    let term3 = -0.5 * sin x
    let term4 = -(x/2.0) * cos x
    term1 + term2 + term3 + term4

let bisectionmethod (starta: float) (startb: float) f =
    let mutable a = starta
    let mutable b = startb
    let mutable mid = 0.0
    let mutable flag = false

    if (f a) * (f b) > 0.0 then
        printfn "Метод дихотомии не подходит для начальных a и b"
        nan
    else
        while not flag && abs(a - b) > eps do
            while Double.IsInfinity(f a) || Double.IsNaN(f a) do
                    a <- a + eps
            while Double.IsInfinity(f b) || Double.IsNaN(f b) do
                    b <- b - eps
            mid <- (a + b) / 2.0

            if (f mid) = 0.0 then
                flag <- true
            elif (f a) * (f mid) < 0.0 then
                b <- mid
            else
                a <- mid

        printfn "Метод дихотомии: корень = %f" mid
        mid

let iterationmethod (x0: float) f g =
    let mutable x = x0
    let mutable i = 0
    let maxcorrections = 100
    let mutable converged = false
    let mutable result = nan

    while not converged && i < maxiter do
        let mutable xnext = g x
        let mutable corr = 0

        while (Double.IsInfinity(xnext) || Double.IsNaN(xnext)) && corr < maxcorrections do
            x <- x + eps
            xnext <- g x
            corr <- corr + 1

        if corr >= maxcorrections then
            printfn "Слишком много попыток уйти от разрыва"
            result <- nan
            converged <- true
        else
            if abs (xnext - x) < eps && abs (f xnext) < eps then
                printfn "Метод итераций: корень = %f итераций: %d" xnext i
                result <- xnext
                converged <- true
            else
                x <- xnext
                i <- i + 1

    if not converged then
        printfn "Метод итераций не сошелся за %d итераций" maxiter
        nan
    else
        result

let newtonmethod (x0: float) f f' =
    let mutable x = x0
    let mutable i = 0
    let mutable converged = false
    let mutable result = nan

    while not converged && i < maxiter do
        if Double.IsInfinity(x) || Double.IsNaN(x) then
            printfn "На итерации %d получено некорректное значение x" i
            result <- nan
            converged <- true
        else
            let fx = f x
            let fpx = f' x

            if Double.IsInfinity(fpx) || Double.IsNaN(fpx) || abs fpx < 1e-15 then
                printfn "Производная близка к нулю или некорректна в точке x=%f" x
                result <- nan
                converged <- true
            else
                let xnext = x - fx / fpx

                if Double.IsInfinity(xnext) || Double.IsNaN(xnext) then
                    printfn "На итерации %d получено некорректное следующее приближение" i
                    result <- nan
                    converged <- true
                else
                    if abs (xnext - x) < eps && abs (f xnext) < eps then
                        printfn "Метод Ньютона: корень = %f итераций: %d" xnext i
                        result <- xnext
                        converged <- true
                    else
                        x <- xnext
                        i <- i + 1

    if not converged then
        printfn "Метод Ньютона не сошелся за %d итераций" maxiter
        nan
    else
        result

[<EntryPoint>]
let main argv =
    printfn "\nМетод дихотомии для func1 с начальным промежутком a = 1.0 b = 4.0:"
    bisectionmethod 1.0 4.0 func1 

    printfn "\nМетод дихотомии для func2 с начальным промежутком a = -1.0 b = 15.0:"
    bisectionmethod -1.0 15.0 func2 

    printfn "\nМетод дихотомии для func3 с начальным промежутком a = -1.0 b = 0.0:"
    bisectionmethod -1.0 0.0 func3 

    printfn "\nМетод итераций для func1 (g1) с начальным приближением 1.5:"
    iterationmethod 1.5 func1 g1 

    printfn "\nМетод итераций для func2 (g2) с начальным приближением 3.0:"
    iterationmethod 3.0 func2 g2

    printfn "\nМетод итераций для func3 (g3) с начальным приближением 0.1:"
    iterationmethod 0.1 func3 g3 

    printfn "\nМетод Ньютона для func1 с начальным приближением 1.5:"
    newtonmethod 1.5 func1 deriv1 

    printfn "\nМетод Ньютона для func2 с начальным приближением 3.0:"
    newtonmethod 3.0 func2 deriv2 

    printfn "\nМетод Ньютона для func3 с начальным приближением 0.1:"
    newtonmethod 0.5 func3 deriv3 

    0