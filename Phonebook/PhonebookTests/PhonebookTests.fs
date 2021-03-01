module PhonebookTests

open NUnit.Framework
open FsUnit
open PhonebookLogic.Phonebook

[<Test>]
let ``Count for empty is zero`` () =
    Phonebook () |> Phonebook.Count |> should equal 0

[<Test>]
let ``Check for existing name`` () =
    let book = Phonebook () |> Phonebook.Add "kek" "12345"
    Phonebook.NameExists "kek" book |> should be True

[<Test>]
let ``Added number exists`` () =
    let book = Phonebook () |> Phonebook.Add "kek" "1-2-3"
    Phonebook.NumberExists "1-2-3" book |> should be True

[<Test>]
let ``Not added name does not exist`` () =
    let book = Phonebook () |> Phonebook.Add "humster" "543"
    Phonebook.NameExists "ca" book |> should be False

[<Test>]
let ``Find number by name`` () =
    let book = Phonebook () |> Phonebook.Add "kek" "321"
    Phonebook.FindNumber "kek" book |> should equal (Some("321"))

[<Test>]
let ``Find name by number`` () =
    let book = Phonebook () |> Phonebook.Add "kek" "321"
    Phonebook.FindName "321" book |> should equal (Some("kek"))

[<Test>]
let ``Try to find name which does not exist`` () =
    let book = Phonebook () |> Phonebook.Add "kek" "0000"
    Phonebook.FindName "00" book |> should equal None

[<Test>]
let ``Try to find number which does not exist`` () =
    let book = Phonebook () |> Phonebook.Add "kek" "123"
    Phonebook.FindNumber "321" book |> should equal None

[<Test>]
let ``Try to add number which already exists`` () =
    let book = Phonebook () |> Phonebook.Add "kek" "1" |> Phonebook.Add "cheburek" "1"
    Phonebook.NameExists "cheburek" book |> should be False
    Phonebook.Count book |> should equal 1

[<Test>]
let ``Try to add name which already exists`` () =
    let book = Phonebook () |> Phonebook.Add "kek" "1" |> Phonebook.Add "kek" "2"
    Phonebook.FindNumber "kek" book |> should equal (Some("1"))
    Phonebook.Count book |> should equal 1

[<Test>]
let ``Read file`` () =
    let book = Phonebook () |> Phonebook.Add "kek" "1" |> Phonebook.FromFile "Test1.txt"

    Phonebook.FindNumber "cheburek" book |> should equal (Some("2"))
    Phonebook.FindNumber "kek" book |> should equal (Some("1"))
    Phonebook.Count book |> should equal 2

[<Test>]
let ``Write and read data from file`` () =
    if System.IO.File.Exists "Test2.txt" then
        System.IO.File.Delete "Test2.txt"

    let book = Phonebook () |> Phonebook.Add "kek" "123" 
    Phonebook.ToFile "Test2.txt" book

    let bookFromFile = Phonebook () |> Phonebook.FromFile "Test2.txt" 
    Phonebook.FindNumber "kek" bookFromFile |> should equal (Some("123"))
