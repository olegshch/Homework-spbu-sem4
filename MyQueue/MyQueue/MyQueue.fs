namespace MyQueue

open System

    type 'a MyQueue()=
        let mutable elements = []

        //Add element 
        member this.Enqueue value =
            let list = [value]
            elements <-  elements @ list

        //Get head
        member this.Dequeue =
            match elements with
            | [] -> raise (InvalidOperationException())
            | h::t ->
                elements <- t
                h    