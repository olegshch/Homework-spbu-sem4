open System
open PhonebookLogic.Phonebook

[<EntryPoint>]
let main argv =
    let rec loop book =
        match Console.ReadLine () with
        | "0" -> 0

        | "1" ->
            Phonebook.ToList book |> List.iter (fun (name, num) -> printfn "%s %s" name num)
            loop book

        | "2" ->
            printf "name: "
            let name = Console.ReadLine ()
            printf "number: "
            let number = Console.ReadLine ()
            if Phonebook.NameExists name book then
                printfn "already exists"
                loop book
            elif Phonebook.NumberExists number book then
                printfn "already exists"
                loop book
            else loop <| Phonebook.Add name number book

        | "3" -> 
            printf "Enter name to find the number: "
            let name = Console.ReadLine ()
            match Phonebook.FindNumber name book with
            | None ->
                printfn "does not exist."
                loop book
            | Some(number) ->
                printfn "%s %s" name number
                loop book

        | "4" ->
            printf "Enter number to find the name: "
            let number = Console.ReadLine ()
            match Phonebook.FindName number book with
            | None ->
                printfn "does not exist."
                loop book
            | Some(name) ->
                printfn "%s %s" name number
                loop book

        | "5" ->
            printf "Enter path to the file to read data from: "
            let path = Console.ReadLine ()
            if System.IO.File.Exists path then
                loop <| Phonebook.FromFile path book
            else
                printfn "wrong path"
                loop book

        | "6" ->
            printf "Enter path to the file to write data: "
            let path = Console.ReadLine ()
            Phonebook.ToFile path book                
            loop book

        | _ -> loop book

    printfn " Phonebook commands:      "
    printfn " 0 -- exit                "
    printfn " 1 -- print data  "
    printfn " 2 -- add      "
    printfn " 3 -- find number by name "
    printfn " 4 -- find name by number "
    printfn " 5 -- read from file "
    printfn " 6 -- write to file  "

    loop <| Phonebook ()
