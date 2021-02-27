namespace SimpleFunctions

module Functions =
    let factorial number =
        let rec fact acc current limit =
            if limit < 0 then -1
            elif limit = 0 then 1
            elif current = limit then acc 
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
        let rec makingPower x n acc =
            match n with
            | 0 -> 1
            | 1 -> x * acc
            | _ when n % 2 = 0 -> makingPower (x * x) (n / 2) acc
            | _ -> makingPower (x * x) (n / 2) (acc * x)

        let rec power n limit acc =            
            if n = limit then acc
            else power (n + 1) limit ((acc.Head / 2)::acc)

        power 0 m [makingPower 2 (n + m) 1]

    let find list number =
        let rec findLocal list position =
            match list with
            | [] -> None
            | head::tail when head = number -> Some(position)
            | _ -> findLocal list.Tail (position + 1)

        findLocal list 0