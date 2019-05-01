// Copyright 2018 Fabulous contributors. See LICENSE.md for license.
namespace RhymePuzzle

open Fabulous.Core
open Fabulous.DynamicViews
open Xamarin.Forms
open System
open WordGrid

open DataMuseApi

module App = 
    open Xamarin.Forms
    open Hopac.Extensions.Seq

    type Model = {
            Grid: (PlacedWord * PlacedWord) list
            EditingAt: int option
        }

    type Msg = 
        | WordApiResult of Result<WordsResponse, string>
        | LetterButtonPressed of (int * int)
        | EditingWordEntryUpdated of TextChangedEventArgs

    let init () =
        {
            Grid = []
            EditingAt = None
        }
        , DataMuseApi.rhymes "sex"
        |> Async.map WordApiResult
        |> Cmd.ofAsyncMsg 

    let update msg model =
        match msg with
        | WordApiResult (Ok words) ->
            let wordList =
                words
                |> Array.map (fun word -> word.Word)
                |> Array.toList
            let grid =
                List.scan (fun acc word -> word::acc) [] wordList
                |> List.rev
                |> List.pick WordGrid.tryPlaceWords
                |> List.map (fun placedWord -> placedWord, { placedWord with Word = [] })
            { Grid = grid; EditingAt = None }
            , Cmd.none
        | WordApiResult (Error _) ->
            model, Cmd.none
        | LetterButtonPressed coordinate ->
            let hiddenWords = List.map fst model.Grid
            let wordClicked = (wordAt coordinate hiddenWords).Value
            { model with EditingAt = (Some (List.findIndex (fun x -> x = wordClicked) hiddenWords)) }, Cmd.none
        | EditingWordEntryUpdated args ->
            let word =
                args.NewTextValue
                |> String.toList
            let grid =
                List.mapi 
                    (fun index (correct, attempt) ->
                        if index <> model.EditingAt.Value
                        then (correct, attempt)
                        else (correct, { attempt with Word = word })
                    )
                    model.Grid
            { model with Grid = grid }, Cmd.none

    let view (model: Model) dispatch =
        let hidden = List.map fst model.Grid
        let displayed = List.map snd model.Grid
        let button point = 
            match WordGrid.at point hidden, WordGrid.at point displayed with
            | Some correct, Some attempted -> 
                View.Button(
                    text = Char.ToString attempted
                    , backgroundColor = (if correct = attempted then Color.DarkBlue else Color.Red)
                    , padding = Thickness(0.0)
                    , borderColor = Color.DarkGray
                    , borderWidth = double 1
                    , command = (fun _ -> LetterButtonPressed point |> dispatch))
            | Some _, None -> 
                View.Button(
                    text = " "
                    , padding = Thickness(0.0)
                    , borderColor = Color.DarkGray
                    , borderWidth = double 1
                    , command = (fun _ -> LetterButtonPressed point |> dispatch))
            | _ -> View.Label(text = " ")
        
        let gridSide = WordGrid.gridSize
        View.ContentPage(
            View.StackLayout(
                [
                    yield View.RelativeLayout(
                        [
                            yield View.Grid(
                                rowdefs = [ for _ in 1..gridSide do yield "*" ]
                                , coldefs = [ for _ in 1..gridSide do yield "*" ]
                                , rowSpacing = 2.0
                                , columnSpacing = 2.0
                                , children = [
                                    for x in 1..gridSide do
                                    for y in 1..gridSide do
                                    yield (button (x,y))
                                        .GridRow(x).GridColumn(y)
                                ]
                            )
                                .WidthConstraint(Constraint.RelativeToParent(fun parent -> parent.Width))
                                .HeightConstraint(Constraint.RelativeToParent(fun parent -> parent.Width))
                        ])
                                       
                    match model.EditingAt with
                    | None -> ()
                    | Some position ->
                        let currentWord = 
                            List.item position model.Grid |> snd
                            |> fun word -> word.Word
                            |> List.map Char.ToString
                            |> String.concat ""
                        yield View.Entry(text = currentWord, textChanged = (EditingWordEntryUpdated >> dispatch))
                            .YConstraint(Constraint.RelativeToParent(fun parent -> parent.Height))
                            .Height(50.0)
                            .WidthConstraint(Constraint.RelativeToParent(fun parent -> parent.Width))
                ]))

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


