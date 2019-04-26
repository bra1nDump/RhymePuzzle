namespace RhymePuzzle

module Option =
    let any<'a> (xs: 'a option list): 'a option = 
        xs
        |> List.fold (fun xs ->
            function 
            | Some x -> x::xs
            | None -> xs)
            []
        |> List.rev
        |> List.tryHead

module WordGrid =
    type Orientation = 
        | Horizontal
        | Vertical

    type WordPosition =
        | Start
        | End
        | Middle

    type Point = int * int

    type PlacedWord =
        {
            Word: char list
            Position: Point
            Orientation: Orientation
        }

    type Letter =
        {
            Letter: char
            WordOrientation: Orientation
            WordPosition: WordPosition
        }

    type PlacedLetter = Point * Letter

    let next orientation (x, y) =
        match orientation with 
        | Horizontal -> x, y + 1
        | Vertical -> x + 1, y

    let previous orientation (x, y) =
        match orientation with 
        | Horizontal -> x, y - 1
        | Vertical -> x - 1, y
    
    let gridSize = 10

    let wordToPlacedWordList word =
        let wordSize = List.length word
        [
            for x in 1..gridSize - wordSize do
            for y in 1..gridSize do
            yield x, y, Horizontal

            for x in 1..gridSize do
            for y in 1..gridSize - wordSize do
            yield x, y, Vertical
        ]
        |> List.map (fun (x, y, orientation) ->
            { Word = word; Position = x, y; Orientation = orientation })

    let placedWordToPlacedLetterList 
        { Word = word; Position = position; Orientation = orientation }
        : PlacedLetter list =
        List.mapFoldi (fun index position letter -> 
            let wordPosition =
                match index with 
                | 0 -> Start
                | _ when List.length word = index + 1 -> End
                | _ -> Middle
            let letter = { 
                Letter = letter
                WordOrientation = orientation
                WordPosition = wordPosition }
            (position, letter), next orientation position)
            position
            word
        |> fst

    let flip = function
        | Horizontal -> Vertical
        | Vertical -> Horizontal

    let maybePlaceLetter ((position, letter): PlacedLetter) grid =
        let isEmptyOrMatchingCheck = 
            match Map.tryFind position grid with
            | Some { Letter = foundLetter; WordOrientation = orientation } when 
                foundLetter <> letter.Letter
                || orientation = letter.WordOrientation -> false
            | _ -> true

        let isEmpty position = Map.containsKey position grid |> not
        let startEndCheck = 
            match letter.WordPosition with 
            | Start ->
                previous letter.WordOrientation position
                |> isEmpty
            | End -> 
                next letter.WordOrientation position
                |> isEmpty
            | Middle -> true

        let isEmptyOrDifferentOrientation position orientation =
            match Map.tryFind position grid with
            | None -> true 
            | Some { WordOrientation = foundOrientation } 
                when foundOrientation <> orientation -> true
            | _ -> false
        let perpendicularCheck =
            let flippedOrientation = flip letter.WordOrientation
            let previous = previous flippedOrientation position
            let next = next flippedOrientation position

            let previousEmptyOrNotEnd = 
                match Map.tryFind previous grid with
                | Some { WordPosition = End } -> false
                | _ -> true

            let nextEmptyOrNotStart =
                match Map.tryFind next grid with
                | Some { WordPosition = Start } -> false
                | _ -> true

            isEmptyOrDifferentOrientation previous letter.WordOrientation
            && isEmptyOrDifferentOrientation next letter.WordOrientation
            && previousEmptyOrNotEnd
            && nextEmptyOrNotStart

        if isEmptyOrMatchingCheck
            && startEndCheck
            && perpendicularCheck
        then Map.add position letter grid |> Some
        else None

    let placeWord word grid = 
        word
        |> placedWordToPlacedLetterList
        |> List.fold 
            (fun grid letter -> 
                Option.bind (maybePlaceLetter letter) grid)
            (Some grid)

    let tryPlaceWord word grid =
        wordToPlacedWordList word
        |> List.map (fun word -> placeWord word grid)
        |> Option.any

    let tryPlaceWords: string list -> Map<Point, char> option =
        List.map String.toList
        >> List.fold
            (fun maybeGrid word -> 
                Option.bind (tryPlaceWord word) maybeGrid)
            (Some Map.empty)
        >> Option.map (Map.map (fun _ { Letter = letter } -> letter))