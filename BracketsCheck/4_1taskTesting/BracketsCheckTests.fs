module _4_1taskTesting

open NUnit.Framework
open FsUnit
open BracketsCheck.BracketsCheck

let testCases = 
    [       
        "", true
        "()", true
        "[]", true
        "{}", true
        "[{}]", true
        "([{[]}])", true
        "[}", false
        "[{]}", false
        "[())]", false
        "[[{((", false
        "[}", false
        "]", false
        "(", false
        "1", false
        "(2)", false
    ] |> List.map (fun (str, res) -> TestCaseData(str, res))

[<Test>]
[<TestCaseSource("testCases")>]
let bracketsTest str res =
    checkBalance str |> should equal res
