namespace RhymePuzzle

module Async =
    
    let lift f asyncComputation =
        async {
            let! x = asyncComputation
            return f x
        }

module String =
    let toList str =
        let mutable characters = []
        String.iter (fun c -> characters <- c::characters) str
        
        List.rev characters

module List =
    // (a -> b list) -> a list -> b list
    let bind f = List.map f >> List.concat

    // apply : (a -> b) list -> a list -> b list
    let rec apply fs xs =
        match fs with
        | f::fs -> List.append (List.map f xs) (apply fs xs)
        | [] -> []
       
    let any = List.fold (||) false

    let mapFoldi step init =
        List.mapi (fun i x -> i, x)
        >> List.mapFold (fun state (i, x) -> step i state x) init

