namespace BracketsCheck

module BracketsCheck =

    ///Функция проверки последовательности
    let checkBalance str =

        ///Проверка на открывающий символ
        let isOpeningBracket symbol =
            symbol = '(' || symbol = '[' || symbol = '{'
        
        ///Проверка на скобочную пару
        let isPair first second =
            match first with
            | '(' -> second = ')'
            | '[' -> second = ']'
            | '{' -> second = '}'
            | _ -> false

        ///Проверка скобочного баланса
        let rec check str stack =
            if str = [] then stack = []
            else
                let strHead = List.head str
                if (isOpeningBracket strHead) then check (List.tail str) (strHead :: stack)
                elif ((stack <> []) && isPair (List.head stack) strHead) then check (List.tail str) (List.tail stack)
                else false

        check (Seq.toList str) []
