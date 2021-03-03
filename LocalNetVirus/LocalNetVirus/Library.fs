namespace LocalNetVirus

open System

module Models =

    /// Описание ОС с вероятностью заражения
    type OS (infectProb : float) =
       
        member this.InfectProb = infectProb

    /// Описание компьютера
    type Computer (id, os : OS, isInfected : bool) =
        let mutable isInfected = isInfected
    
        member this.Id = id
        member this.OS = os
        member this.IsInfected
            with get () = isInfected
            and set (value) = isInfected <- value

    /// Описание сети
    type Network (computers: list<Computer>, matrix: list<list<bool>>) =
        
        let mutable computers = computers
    
        member this.Computers = computers
    
        /// Шаг заражения
        member this.Infect () =
            if not (computers |> List.filter (fun x -> not x.IsInfected) |> List.isEmpty) then
    
                let mutable neighboursToInfect = 0
                let mutable allAreInvurnelable = true
    
                /// Попытка заражения
                let tryToInfectNeighbours index =
                    for i in 0 .. computers.Length - 1 do
                        if matrix.[index].[i] then
                            neighboursToInfect <- neighboursToInfect + 1
    
                            if (computers.[i].OS.InfectProb <> 1.0) then
                                allAreInvurnelable <- false
    
                            /// Заражение
                            if (not computers.[i].IsInfected && computers.[i].OS.InfectProb > Random().NextDouble()) then
                                computers.[i].IsInfected <- true
    
                let infected = computers |> List.filter (fun x -> x.IsInfected)
    
                for pc in infected do
                    computers |> List.findIndex (fun x -> x.Id = pc.Id) |> tryToInfectNeighbours
    
                neighboursToInfect <> 0 && allAreInvurnelable
            else
                false
    
        /// Вывод состояния
        member this.PrintReport () =
            computers |>
            List.iter (fun item -> printfn "id: %A\nInfection probability: %A\nIs infected: %A\n" item.Id item.OS.InfectProb item.IsInfected)