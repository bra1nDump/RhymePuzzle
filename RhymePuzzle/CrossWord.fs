namespace RhymePuzzle

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
    
    let gridSize = 12
    let gridCenter = gridSize / 2, gridSize / 2

    let wordToPlacedWordList word =
        let placedWordAndScore point orientation =
            let (-) (x1,y1) (x2, y2) = x1 - x2, y1 - y2
            let abs (x, y) = (abs x, abs y)
            let wordCenter = move orientation point (List.length word / 2)
            { Word = word; Position = point; Orientation = orientation }
            , gridCenter - wordCenter |> abs
        let wordSize = List.length word
        [
            for x in 1..gridSize do
            for y in 1..(gridSize - wordSize + 1) do
            yield placedWordAndScore (x, y) Horizontal
        ]
        @
        [
            for x in 1..(gridSize - wordSize + 1) do
            for y in 1..gridSize do
            yield placedWordAndScore (x, y) Vertical
        ]
        |> List.sortBy snd
        |> List.map fst
        
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

    let tryWord (grid: PlacedWord list): Word -> PlacedWord list option =
        wordToPlacedWordList
        >> Option.first tryPlaceWord grid

    let tryPlaceWords: string list -> PlacedWord list option =
        List.map String.toList
        >> Option.all tryWord []

    let at position = List.tryPick (letterAt position)
        