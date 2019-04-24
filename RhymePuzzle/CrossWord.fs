namespace RhymePuzzle

module CrossWord =    
    let gridSide = 10
    let initGrid: Map<(int * int), char> = Map.empty

    let acceptLetter (position, letter) grid =
        if position > (gridSide, gridSide) then
            None
        else
            match Map.tryFind position grid with 
            | None -> Map.add position letter grid |> Some
            | Some oldLetter when oldLetter = letter -> grid |> Some
            | _ -> None

    let rec applyN n f x =
        if n = 0 then x
        else x |> f |> applyN (n - 1) f

    let acceptWord grid word slide position =
        List.mapi (fun i letter -> applyN i slide position, letter) word
        |> List.fold 
            (fun maybeGrid letterPosition ->
                Option.bind (acceptLetter letterPosition) maybeGrid
            )
            (Some grid)

    // lower the score the better the position
    let scoreAxis value =
        let center = gridSide / 2
        center - value |> abs

    let positionScore (x, y) =
        scoreAxis x, scoreAxis y

    let wordCenter word (x, y, slide) =
        applyN (List.length word) slide (x, y)

    let addToGrid word grid =
        [
            for x in 1..gridSide do
            for y in 1..gridSide do
            yield x, y, (fun (x, y) -> (x, y + 1))
            yield x, y, (fun (x, y) -> (x + 1, y))
        ]
        |> List.sortBy (wordCenter word >> (fun (x, y) -> x + y))
        |> List.map (fun (x, y, slide) -> 
            acceptWord grid word slide (x, y))
        |> List.filter Option.isSome
        |> function
        | head::_ -> head
        | [] -> None
        
    let buildGrid =
        List.fold 
            (fun maybeGrid word ->
                Option.bind (addToGrid word) maybeGrid
            )
            (Some initGrid)


    