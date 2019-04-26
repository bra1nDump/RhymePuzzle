namespace RhymePuzzle

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

module WordGrid =
    type Word = char list
    type Point = int * int
    type Orientation = 
        | Horizontal
        | Vertical

    type PlacedWord = {
            Word: Word
            Position: Point
            Orientation: Orientation
        }

    type Grid = PlacedWord list

    let move orientation (x, y) n =
        match orientation with 
        | Horizontal -> x, y + n
        | Vertical -> x + n, y

    let flip = function
        | Horizontal -> Vertical
        | Vertical -> Horizontal
    
    let gridSize = 10

    let wordToPlacedWordList word =
        let wordSize = List.length word
        [
            for x in 1..gridSize do
            for y in 1..(gridSize - wordSize + 1) do
            yield { Word = word; Position = x, y; Orientation = Horizontal }
        ]
        @
        [
            for x in 1..(gridSize - wordSize + 1) do
            for y in 1..gridSize do
            yield { Word = word; Position = x, y; Orientation = Vertical }
        ]
        |> List.sortBy (fun word -> word.Position)
        
    let wordAreaList { Word = word; Position = start; Orientation = orientation } =
        List.init (List.length word) (move orientation start)

    let wordArea = wordAreaList >> Set.ofList

    let letterAt position word =
        List.zip
            (wordAreaList word)
            word.Word
        |> Map.ofList
        |> Map.tryFind position
        
    let area1 word =
        let area = wordArea word |> Set.toList
        let mutations = 
            [ 
                fun x -> x + 1
                fun x -> x - 1
                id
            ]
        let (<*>) = List.apply

        [ fun first second (x,y) -> first x, second y ]
        <*> mutations
        <*> mutations
        <*> area
        |> Set.ofList

    let acceptWordPair candidate fixedWord =
        let fixedWordArea = wordArea fixedWord
        if candidate.Orientation = fixedWord.Orientation then
            Set.intersect 
                fixedWordArea
                (area1 candidate)
            |> Set.isEmpty
        else
            match Set.intersect
                    fixedWordArea
                    (area1 candidate)
                |> Set.toList with
            | [] -> true
            | overlaps ->
                overlaps 
                |> List.map
                    (fun overlap ->
                        let char1 = letterAt overlap candidate
                        let char2 = letterAt overlap fixedWord
                        char1 = char2
                        && char1.IsSome)
                |> List.any

    let tryPlaceWord word placedWords =
        if List.forall (acceptWordPair word) placedWords
        then word::placedWords |> Some
        else None

    let tryWord (grid: PlacedWord list): char list -> PlacedWord list option =
        wordToPlacedWordList
        >> Option.first tryPlaceWord grid

    let tryPlaceWords: string list -> PlacedWord list option =
        List.map String.toList
        >> Option.all tryWord []

    let at position = List.tryPick (letterAt position)
        