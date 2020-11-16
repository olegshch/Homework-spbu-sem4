namespace SimpleFunctions

module Functions =
    let factorial number =
        let rec fact acc current limit =
            if current = limit then acc 
            else fact (acc * (current + 1)) (current + 1) limit
        fact 1 1 number 
    
    let fibonacci number =
        let rec fib prev cur i limit =
            if i = limit then prev
            else fib cur (prev + cur) (i + 1) limit
        fib 1 1 1 number

    let reverseList curlist =
        let rec rev oldlist newlist = 
            match oldlist with
            | [] -> newlist
            | head::tail -> rev tail (head::newlist)
        rev curlist []

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