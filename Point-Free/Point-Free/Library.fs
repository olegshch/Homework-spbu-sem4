namespace Point_Free

module PointFree =

    /// Перевод в point-free стиль
    let mult1 x l = List.map (fun y -> y * x) l
    
    let mult2 x = List.map (fun y -> y * x)
    
    let mult3 x = List.map << (*) <| x
    
    let multPointFree = List.map << (*)