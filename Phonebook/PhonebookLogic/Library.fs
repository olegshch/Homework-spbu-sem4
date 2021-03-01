namespace PhonebookLogic

module Phonebook =

    /// Описание телефонной книги
    type Phonebook (records) = 
        let records = Map<string, string> records

        member this.Records with get () = records

        new () = Phonebook []

        /// Количество записей
        static member Count (book: Phonebook) = Map.count book.Records

        /// Поиск номера по имени
        static member FindNumber name (book: Phonebook) = Map.tryFind name book.Records

        /// Поиск имени по номеру
        static member FindName number (book: Phonebook) = Map.tryFindKey (fun name num -> num = number) book.Records

        /// Проверка существования по имени
        static member NameExists name (book: Phonebook) = Map.containsKey name book.Records

        /// Проверка существования по номеру
        static member NumberExists number (book: Phonebook) = Map.exists (fun name num -> num = number) book.Records

        static member ToList (book: Phonebook) = book.Records |> Map.toList

        /// Добавление новой записи
        static member Add name number (book: Phonebook) =
            if not <| Phonebook.NameExists name book && not <| Phonebook.NumberExists number book then
                book.Records |> Map.add name number |> Map.toList |> Phonebook
            else book

        /// Считывание с файла
        static member FromFile path book = System.IO.File.ReadLines path |> Seq.chunkBySize 2 |> Seq.fold (fun b l -> if l.Length = 2 then Phonebook.Add l.[0] l.[1] b else b) book

        /// Запись в файл
        static member ToFile path (book: Phonebook) = System.IO.File.WriteAllLines (path, book.Records |> Map.toList |> List.map (fun (name, num) -> [name; num]) |> List.concat)