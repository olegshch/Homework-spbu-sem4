namespace SecondFunctions

/// Функции для подсчета четных чисел в списке
module EvenCount = 

    /// Подсчет четных чисел в списке
    let evenFilter =
        List.filter (fun x -> x % 2 = 0) >> List.length

    /// Подсчет четных чисел в списке
    let evenMap =
        List.map (fun x -> 1 - abs(x % 2)) >> List.fold (+) 0

    /// Подсчет четных чисел в списке
    let evenFold =
        List.fold (fun acc x -> acc + 1 - abs(x % 2)) 0

/// Map для дерева
module MapTree =

    /// Описание дерева
    type Tree<'a> =
        | Node of 'a * Tree<'a> * Tree<'a>
        | Empty

    /// Применение функции к дереву
    let rec mapTree func tree = 
        match tree with
        | Node(h, l, r) -> Node(func h, mapTree func l, mapTree func r)
        | Empty -> Empty

module ExpressionEvaluation =

    /// Arithmetical expression tree representation with discriminated union.
    type Expr =
        | Sum of Expr * Expr
        | Mult of Expr * Expr
        | Diff of Expr * Expr
        | Div of Expr * Expr
        | Operand of int

    /// Evaluates an arithmetical expression tree.
    let rec eval = function
        | Sum(a, b) -> eval a + eval b
        | Mult(a, b) -> eval a * eval b
        | Diff(a, b) -> eval a - eval b
        | Div(a, b) -> eval a / eval b
        | Operand(x) -> x