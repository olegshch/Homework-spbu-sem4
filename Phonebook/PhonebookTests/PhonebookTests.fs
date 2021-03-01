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
