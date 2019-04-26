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
    let bind f = List.map f >> List.concat
    let xs (>>=) f = bind

    let mapFoldi step init =
        List.mapi (fun i x -> i, x)
        >> List.mapFold (fun state (i, x) -> step i state x) init