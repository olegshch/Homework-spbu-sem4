namespace LocalNetVirus

open System

module Models =

    /// Описание ОС с вероятностью заражения
    type OS (infectProbability : float) =
       
        member this.InfectProbability = infectProbability

    /// Описание компьютера
    type Computer (id, os : OS, isInfected : bool) =
        let mutable isInfected = isInfected
    
        member this.Id = id
        member this.OS = os
        member this.IsInfected
            with get () = isInfected
            and set (value) = isInfected <- value

        member this.Attempt (randomGen : System.Random) =

            /// Заражение
            if (not isInfected && this.OS.InfectProbability > randomGen.NextDouble()) then
                isInfected <- true

    /// Описание сети
    type Network (computers: list<Computer>, matrix: list<list<bool>>) =
        
        let randomGen = Random()

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
    
                            if (computers.[i].OS.InfectProbability <> 1.0) then
                                allAreInvurnelable <- false    

                            computers.[i].Attempt(randomGen)
    
                let infected = computers |> List.filter (fun x -> x.IsInfected)
    
                for pc in infected do
                    computers |> List.findIndex (fun x -> x.Id = pc.Id) |> tryToInfectNeighbours
    
                neighboursToInfect <> 0 && allAreInvurnelable
            else
                false
    
        /// Вывод состояния
        member this.PrintReport () =
            computers |>
            List.iter (fun item -> printfn "id: %A\nInfection probability: %A\nIs infected: %A\n" item.Id item.OS.InfectProbability item.IsInfected)