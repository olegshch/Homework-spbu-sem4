namespace TreeFilter

module Tree =
    type 'T Tree =
        | Node of 'T * 'T Tree * 'T Tree
        | Emmpty
    
    ///returns list of filtered elements
    let treeFilter tree condition = 
        let rec treeFilterRecursive currentTree currentCondition currentList =
            match currentTree with
            | Emmpty -> currentList
            | Node(x,l,r) ->
                if (currentCondition x) then
                    treeFilterRecursive l currentCondition (x::currentList) |> treeFilterRecursive r currentCondition
                else
                    treeFilterRecursive l currentCondition currentList |> treeFilterRecursive r currentCondition
        treeFilterRecursive tree condition []