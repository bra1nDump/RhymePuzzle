namespace RhymePuzzle

module Async =
    
    let lift f asyncComputation =
        async {
            let! x = asyncComputation
            return f x
        }

module Option =
    let first<'a, 'state> 
        (f: 'a -> 'state -> 'state option) 
        (init: 'state) 
        (xs: 'a list)
        : 'state option = 
        List.fold (fun state x ->
            match state with
            | Some state -> Some state
            | None -> f x init)
            None
            xs

    // create a fold with optional function that mutates the state but
    // once it fails the entire computation fails
    let all<'a, 'b> (f: 'b -> 'a -> 'b option) (init: 'b) (xs: 'a list): 'b option =
        List.fold 
            (fun maybeState x -> 
                maybeState
                |> Option.bind (fun state -> f state x))
            (Some init)
            xs


module String =
    open System

    let toList str =
        let mutable characters = []
        String.iter (fun c -> characters <- c::characters) str
        
        List.rev characters

    let fromCharList =
        List.map Char.ToString
        >> String.concat ""

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

    let takeMax k xs =
        if List.length xs <= k
        then xs
        else List.take k xs

    let rec zip xs ys =
        match xs, ys with 
        | x::xs, y::ys -> (x, y)::zip xs ys
        | _ -> []

    let rec zipWithNone xs ys =
        match xs, ys with
        | x::xs, y::ys -> (Some x, Some y)::zipWithNone xs ys
        | x::xs, [] -> (Some x, None)::zipWithNone xs []
        | [], y::ys -> (None, Some y)::zipWithNone [] ys
        | _ -> []

[<AutoOpen>]
module Map =
    let merge oldMap updates = Map.foldBack Map.add updates oldMap