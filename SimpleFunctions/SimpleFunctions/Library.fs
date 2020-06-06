namespace SimpleFunctions

module Functions =
    let Factorial n =
        let rec Fact s a b =
            if a = b then s 
            else Fact (s * (a + 1)) (a + 1) b
        Fact 1 1 n 
    
    let Fibonacci n =
        let rec Fib a b i n =
            if i = n then a
            else Fib b (a + b) (i + 1) n
        Fib 1 1 1 n

    let ReverseList llist =
        let rec Rev oldlist newlist = 
            match oldlist with
            | [] -> newlist
            | head::tail -> Rev tail (head::newlist)
        Rev llist []

    let PowerTwo n m =
        let rec Power n m list = 
            let Makingpower n =
                let rec pow a b = 
                    if b = 0 then a
                    else pow (a * 2) (b - 1)
                pow 1 n
            if n = m then list
            else Power (n + 1) m (list @ [Makingpower n])
        Power n (n + m + 1) []

    let Find list n =
        let rec Findd list i =
            match list with
            | [] -> -1
            | head::tail when head = n -> i
            | _ -> Findd list.Tail (i + 1)
        Findd list 0