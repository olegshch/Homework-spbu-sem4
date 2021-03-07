namespace LambdaInterpretator

module LambdaInterpretator =

    /// Описание лямбда-выражения
    type Term =
        | Var of char
        | Abs of char * Term
        | App of Term * Term

    /// Бета-редукция
    let rec reduction expr =
        
        /// Проверка на свободную переменную
        let rec isFreeVar expr var = 
            match expr with 
            | Var x -> x = var
            | Abs(vr, term) -> vr <> var && (isFreeVar term var) 
            | App(l, r) -> isFreeVar l var || isFreeVar r var

        /// Подстановка
        let rec substitute expr insteadOf value =
            match expr with
            | Var x when x = insteadOf -> value
            | Var _ -> expr
            | Abs(var, lmd) when var = insteadOf -> expr
            | Abs(var, lmd) when not (isFreeVar value var) -> Abs(var, substitute lmd insteadOf value)
            | Abs(var, lmd) -> 
                let newSym = ['a'..'z'] |> List.filter (not << isFreeVar value) |> List.filter (not << isFreeVar lmd) |> List.head
                Abs(newSym, substitute value insteadOf <| substitute lmd var (Var newSym))
            | App(l, r) -> App(substitute l insteadOf value, substitute r insteadOf value)
            | _ -> expr

        /// Удаление левого терма
        let rec reduceLeft expr =
            match expr with
            | App(l, r) -> match reduceLeft l with 
                              | Abs(var, lmd) -> substitute lmd var r 
                              | var -> App(var, r)
            | _ -> expr

        match expr with
        | Var _ -> expr
        | App(l, r) -> match reduceLeft l with 
                            | Abs(var, lmd) -> reduction (substitute lmd var r)
                            | var -> App(reduction var, reduction r)
        | Abs(var, lmd) -> Abs(var, reduction lmd)
