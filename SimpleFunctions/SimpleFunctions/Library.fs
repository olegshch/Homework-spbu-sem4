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

    let powerTwoList n m =
        let rec power n limit list = 
            let makingPower power =
                let rec pow res powerTwo = 
                    if powerTwo = 0 then res
                    else pow (res * 2) (powerTwo - 1)
                pow 1 power
            if n = limit then list
            else power (n + 1) limit (list @ [makingPower n])
        power n (n + m + 1) []

    let find list number =
        let rec findLocal list position =
            match list with
            | [] -> -1
            | head::tail when head = number -> position
            | _ -> findLocal list.Tail (position + 1)
        findLocal list 0