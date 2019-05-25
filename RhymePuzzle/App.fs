// Copyright 2018 Fabulous contributors. See LICENSE.md for license.
namespace RhymePuzzle

open Fabulous.Core
open Fabulous.DynamicViews
open Xamarin.Forms
open System
open WordGrid

open DataMuseApi

[<AutoOpen>]
module Theme =

    // green
    let primaryButtonColor = Color.FromHex("06A291")
    // blue
    let focusColor = Color.FromHex("00A7E1")
    let darkBlue = Color.FromHex("003459")
    // red
    let errorColor = Color.FromHex("CC0000")
    let borderColor = Color.FromHex("33312E")
    let textColor = Color.FromHex("00171F")

    let clear = Color(0.0, 0.0, 0.0, 0.0)

    // theme
    let primaryHeight = 50
    let png name = ImageSource.FromResource("RhymePuzzle." + name + ".png")

    let snoopaLoompa = png "snoopaLoompa"
    let leftIcon = png "leftIcon"
    let rightIcon = png "rightIcon"
    let closeIcon = png "closeIcon"

    let screen = Device.Info.ScaledScreenSize

    let imageButton image command =
        View.AbsoluteLayout(
            [
                View.Image(image)
                    .LayoutFlags(AbsoluteLayoutFlags.PositionProportional)
                    .LayoutBounds(Rectangle(0.5, 0.5, float primaryHeight * 0.6, float primaryHeight * 0.6))
            ]
            , gestureRecognizers =
                [ View.TapGestureRecognizer(command) ]
        )

module App = 
    open Xamarin.Forms.PlatformConfiguration

    type CrossWordModel = {
            BaseWord: string
            Words: (PlacedWord * DataMuseApi.Word) list
            Letters: Map<Point, char>

            CurrentWordIndex: int option
            HintIndex: int option

            EditingWordEntryRef: ViewRef<Entry>
        }

    type Model =
        | BaselineRhymeWordModel of string
        | LoadingCrosswordModel of string
        | CrossWordModel of CrossWordModel

    type Msg = 
        | BaselineRhymeUpdated of string
        | BaselineRhymeSelected

        | SnoopaLoompaImageCreated
        | WordApiResult of Result<WordsResponse, string>

        | LetterButtonPressed of (int * int)

        | NextHintButtonPressed
        | EditingWordEntryUpdated of string
        | EditingWordEntrySizeChanged
        | CloseWordView

    let init () = BaselineRhymeWordModel "", Cmd.none

    let initCrossWordModel baseWord words =
        {
            BaseWord = baseWord
            Words = words
            Letters = Map.empty

            CurrentWordIndex = None
            HintIndex = None

            EditingWordEntryRef = ViewRef<Entry>()
        }

    let snoopaLoompaImageRef = ViewRef<Image>()

    let update msg model =
        match msg, model with
        | BaselineRhymeUpdated word, _ ->
            BaselineRhymeWordModel word, Cmd.none
        | BaselineRhymeSelected, BaselineRhymeWordModel word ->
            LoadingCrosswordModel word
            ,
            DataMuseApi.rhymes word
            |> Async.map WordApiResult
            |> Cmd.ofAsyncMsg
        | SnoopaLoompaImageCreated, _ ->
            snoopaLoompaImageRef.Value.RotateTo(360.0, uint32 4000, Easing.CubicInOut)
            |> Async.AwaitTask
            |> Async.bind (fun _ -> 
                    snoopaLoompaImageRef.TryValue
                    |> Option.iter (fun image -> image.RotateTo(360.0, uint32 4000, Easing.CubicInOut) |> ignore)
                        
                    Async.result ()
                )
            |> Async.StartImmediate

            model
            , Cmd.none
        | WordApiResult (Ok words), LoadingCrosswordModel baseWord ->
            let words = 
                words
                |> Array.filter 
                    (fun (word: DataMuseApi.Word) -> 
                        Array.isEmpty word.Defs |> not
                    )
            let wordList =
                words
                |> Array.map (fun word -> word.Word)
                |> Array.toList
            let grid =
                List.scan (fun acc word -> word::acc) [] wordList
                |> List.rev
                |> List.pick WordGrid.tryPlaceWords
                |> List.map (fun placedWord -> 
                    placedWord
                    , 
                    Array.find 
                        (fun (word: Word) -> String.toList word.Word = placedWord.Word) 
                        words
                )

            initCrossWordModel baseWord grid |> CrossWordModel
            , Cmd.none
        | WordApiResult (Error _), _ ->
            BaselineRhymeWordModel "", Cmd.none
        | LetterButtonPressed coordinate, CrossWordModel model ->
            let hiddenWords = List.map fst model.Words
            let wordClicked = (wordAt coordinate hiddenWords).Value
            let wordIndex = List.findIndex (fun x -> x = wordClicked) hiddenWords
            
            { model with CurrentWordIndex = Some wordIndex; HintIndex = Some 0 } |> CrossWordModel
            , Cmd.none
        | EditingWordEntrySizeChanged, CrossWordModel model ->
            Option.iter 
                (fun (entry: Entry) -> 
                    entry.Focus() |> ignore
                    let currentWord = 
                        List.item model.CurrentWordIndex.Value model.Words |> snd
                        |> fun word -> word.Word
                    entry.CursorPosition <- String.length currentWord
                )
                model.EditingWordEntryRef.TryValue
            CrossWordModel model, Cmd.none
        | CloseWordView, CrossWordModel model ->
            { model with CurrentWordIndex = None; HintIndex = None } |> CrossWordModel
            , Cmd.none
        | EditingWordEntryUpdated text, CrossWordModel model  ->
            let word = List.item model.CurrentWordIndex.Value model.Words |> fst
            let area = WordGrid.wordAreaList word

            let wordLength = List.length word.Word
            let wordEntered =
                text
                |> String.toList
                |> List.takeMax wordLength
                |> List.map Some

            model.EditingWordEntryRef.Value.Text 
                <- text.Substring(0, wordEntered.Length)
                
            let wordEntered = 
                wordEntered 
                @ List.init (wordLength - wordEntered.Length) (fun _ -> None)

            let letters = 
                List.zip area wordEntered
                |> List.fold (fun map (position, letter) -> 
                        match letter with
                        | Some letter -> Map.add position letter
                        | None -> Map.remove position
                        <| map
                    )
                    model.Letters
            CrossWordModel { model with Letters = letters }, Cmd.none
        | NextHintButtonPressed, CrossWordModel model ->
            // we dont want to loose focus of the current entry
            model.EditingWordEntryRef.Value.Focus() |> ignore

            match model.CurrentWordIndex, model.HintIndex with
            | Some wordIndex, Some index ->
                let _, wordInfo = List.item wordIndex model.Words
                { model with HintIndex = Some ((index + 1) % (Array.length wordInfo.Defs)) } |> CrossWordModel
                , Cmd.none
            | _ -> CrossWordModel model, Cmd.none

    let unitSpacing = 4.0
    let cellSide = (screen.Width - unitSpacing) / float WordGrid.gridSize - unitSpacing
    
    let letterCellAt letters dispatch point = 
        View.Button(
            text = 
                match Map.tryFind point letters with
                | Some letter -> Char.ToString letter
                | None -> ""
            , command = (fun _ -> LetterButtonPressed point |> dispatch)

            , widthRequest = cellSide
            , heightRequest = cellSide
            
            , padding = Thickness(0.0)
            , borderColor = borderColor
            , borderWidth = double 1
            , cornerRadius = 5
        )
            .GridRow(fst point)
            .GridColumn(snd point)
 
    let viewGrid (model: CrossWordModel) dispatch =
        View.ContentPage(
            View.StackLayout(
                [
                    View.AbsoluteLayout(
                        [
                            View.Label(model.BaseWord)
                                .LayoutFlags(AbsoluteLayoutFlags.PositionProportional)
                                .LayoutBounds(Rectangle(0.5, 0.5, AbsoluteLayout.AutoSize, float primaryHeight))
                        ]
                        , heightRequest = float primaryHeight
                    )
                    View.Grid(
                        rowdefs = List.init WordGrid.gridSize (fun _ -> "*" :> obj)
                        , coldefs = List.init WordGrid.gridSize (fun _ -> "*" :> obj)
                        , children = [
                            for word in model.Words |> List.map fst do
                            yield! word 
                                |> WordGrid.wordAreaList 
                                |> List.map (letterCellAt model.Letters dispatch)
                        ]
                        , rowSpacing = unitSpacing
                        , columnSpacing = unitSpacing
                        , heightRequest = screen.Width
                    )
                ]
            )
        )

    let viewWordFocus (wordPlacement, wordInfo: DataMuseApi.Word) model dispatch =
        let wordText = 
            WordGrid.wordAreaList wordPlacement
            |> List.map (fun position -> Map.tryFind position model.Letters)
            |> List.fold (fun letters -> function 
                | Some letter -> letter::letters
                | None -> letters)
                []
            |> List.rev
            |> List.map Char.ToString
            |> String.concat ""

        View.ContentPage(
            View.StackLayout(
                [
                    View.AbsoluteLayout(
                        [
                            (imageButton leftIcon (fun () -> CloseWordView |> dispatch))
                                .LayoutFlags(AbsoluteLayoutFlags.PositionProportional)
                                .LayoutBounds(Rectangle(0.0, 0.5, float primaryHeight, float primaryHeight))
                            View.Label(model.BaseWord)
                                .LayoutFlags(AbsoluteLayoutFlags.PositionProportional)
                                .LayoutBounds(Rectangle(0.5, 0.5, AbsoluteLayout.AutoSize, float primaryHeight))
                        ]
                        , heightRequest = float primaryHeight
                    )
                    
                    View.StackLayout(
                        [
                            View.Label(wordInfo.Defs.[model.HintIndex.Value], horizontalOptions = LayoutOptions.FillAndExpand)
                            View.Button(
                                "next"
                                , command = (fun _ -> NextHintButtonPressed |> dispatch)
                                , verticalOptions = LayoutOptions.Center
                            )
                        ]
                        , orientation = StackOrientation.Horizontal
                    )
                    View.AbsoluteLayout(
                        gestureRecognizers = 
                            [ View.TapGestureRecognizer(fun () -> CloseWordView |> dispatch) ]
                        , children = [
                            View.Entry(
                                text = wordText
                                , textChanged = (fun args -> 
                                    if args.NewTextValue <> args.OldTextValue
                                    then EditingWordEntryUpdated args.NewTextValue |> dispatch
                                    )
                                , sizeChanged = (fun _ -> EditingWordEntrySizeChanged |> dispatch)
                                , ref = model.EditingWordEntryRef
                            )
                                .LayoutFlags(AbsoluteLayoutFlags.All)
                                .LayoutBounds(Rectangle(0.0, 0.0, 1.0, 1.0))

                            View.BoxView(Color.White)
                                .LayoutFlags(AbsoluteLayoutFlags.All)
                                .LayoutBounds(Rectangle(0.0, 0.0, 1.0, 1.0))

                            View.StackLayout(
                                wordPlacement 
                                    |> WordGrid.wordAreaList 
                                    |> List.map (letterCellAt model.Letters dispatch)
                                , orientation = StackOrientation.Horizontal
                                , spacing = 4.0
                            )
                                .LayoutFlags(AbsoluteLayoutFlags.PositionProportional 
                                    ||| AbsoluteLayoutFlags.HeightProportional)
                                .LayoutBounds(Rectangle(0.5, 0.5, AbsoluteLayout.AutoSize, 1.0))
                        ]
                    )
                ]
            )
        )

    let view model dispatch =
        match model with
        | BaselineRhymeWordModel model -> 
            View.ContentPage(
                View.StackLayout(
                    [
                        yield View.Label(
                            "Enter the baseline for your"
                            , horizontalOptions = LayoutOptions.Center
                        )
                        yield View.Entry(
                            model
                            , placeholder = "rhyme"
                            , textColor = primaryButtonColor
                            , textChanged = (fun args -> 
                                if args.NewTextValue <> args.OldTextValue 
                                then args.NewTextValue |> BaselineRhymeUpdated |> dispatch
                            )
                            , horizontalOptions = LayoutOptions.CenterAndExpand
                        )

                        if model = "" then
                            yield View.Label(
                                "no pressure tho"
                                , horizontalOptions = LayoutOptions.Center
                            )
                            yield View.Label(
                                "take your time"
                                , horizontalOptions = LayoutOptions.Center
                            )

                        yield View.Button(
                            "Generate puzzle"
                            , command = (fun _ -> BaselineRhymeSelected |> dispatch)
                            , heightRequest = double primaryHeight
                            , cornerRadius = primaryHeight / 2
                            , borderWidth = 1.0
                            , backgroundColor = focusColor
                        )
                    ]
                )
            )
        | LoadingCrosswordModel word ->
            View.ContentPage(
                View.AbsoluteLayout(
                    [
                        View.Label(
                            "Snoopa loompas assemblying rhymes"
                        )
                            .LayoutFlags(AbsoluteLayoutFlags.PositionProportional)
                            .LayoutBounds(Rectangle(0.5, 0.05, AbsoluteLayout.AutoSize, AbsoluteLayout.AutoSize))

                        View.Label(
                            word
                            , textColor = primaryButtonColor
                        )
                            .LayoutFlags(AbsoluteLayoutFlags.PositionProportional)
                            .LayoutBounds(Rectangle(0.5, 0.25, AbsoluteLayout.AutoSize, AbsoluteLayout.AutoSize))

                        View.Image(
                            snoopaLoompa
                            , created = (fun _ -> SnoopaLoompaImageCreated |> dispatch)
                            , ref = snoopaLoompaImageRef
                        )
                            .LayoutFlags(AbsoluteLayoutFlags.All)
                            .LayoutBounds(Rectangle(0.5, 0.5, 0.5, 0.5))
                    ]
                )
            )
        | CrossWordModel model ->
            match model with 
            | { CurrentWordIndex = Some index } ->
                let word = List.item index model.Words
                viewWordFocus word model dispatch
            | model -> 
                viewGrid model dispatch

    // Note, this declaration is needed if you enable LiveUpdate
    let program = Program.mkProgram init update view

type App () as app = 
    inherit Application ()

    let runner = 
        App.program
#if DEBUG
        |> Program.withConsoleTrace
#endif
        |> Program.runWithDynamicView app

#if DEBUG
    // Uncomment this line to enable live update in debug mode. 
    // See https://fsprojects.github.io/Fabulous/tools.html for further  instructions.
    //
    //do runner.EnableLiveUpdate()
#endif    

    // Uncomment this code to save the application state to app.Properties using Newtonsoft.Json
    // See https://fsprojects.github.io/Fabulous/models.html for further  instructions.
#if APPSAVE
    let modelId = "model"
    override __.OnSleep() = 

        let json = Newtonsoft.Json.JsonConvert.SerializeObject(runner.CurrentModel)
        Console.WriteLine("OnSleep: saving model into app.Properties, json = {0}", json)

        app.Properties.[modelId] <- json

    override __.OnResume() = 
        Console.WriteLine "OnResume: checking for model in app.Properties"
        try 
            match app.Properties.TryGetValue modelId with
            | true, (:? string as json) -> 

                Console.WriteLine("OnResume: restoring model from app.Properties, json = {0}", json)
                let model = Newtonsoft.Json.JsonConvert.DeserializeObject<App.Model>(json)

                Console.WriteLine("OnResume: restoring model from app.Properties, model = {0}", (sprintf "%0A" model))
                runner.SetCurrentModel (model, Cmd.none)

            | _ -> ()
        with ex -> 
            App.program.onError("Error while restoring model found in app.Properties", ex)

    override this.OnStart() = 
        Console.WriteLine "OnStart: using same logic as OnResume()"
        this.OnResume()
#endif


