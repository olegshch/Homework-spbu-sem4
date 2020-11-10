namespace SimpleFunctions

module Functions =
    let factorial n =
        let rec fact s a b =
            if a = b then s 
            else fact (s * (a + 1)) (a + 1) b
        fact 1 1 n 
    
    let fibonacci n =
        let rec fib a b i n =
            if i = n then a
            else fib b (a + b) (i + 1) n
        fib 1 1 1 n

    let reverseList llist =
        let rec rev oldlist newlist = 
            match oldlist with
            | [] -> newlist
            | head::tail -> rev tail (head::newlist)
        rev llist []

    let powerTwo n m =
        let rec power n m list = 
            let makingPower n =
                let rec pow a b = 
                    if b = 0 then a
                    else pow (a * 2) (b - 1)
                pow 1 n
            if n = m then list
            else power (n + 1) m (list @ [makingPower n])
        power n (n + m + 1) []

    let find list n =
        let rec findd list i =
            match list with
            | [] -> -1
            | head::tail when head = n -> i
            | _ -> findd list.Tail (i + 1)
        findd list 0