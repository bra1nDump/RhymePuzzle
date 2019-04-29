﻿// Copyright 2018 Fabulous contributors. See LICENSE.md for license.
namespace RhymePuzzle

open Fabulous.Core
open Fabulous.DynamicViews
open Xamarin.Forms
open System
open WordGrid

module App = 
    type Model = PlacedWord list

    type Msg = 
        | WordApiResult of Result<string list, string>
        | LetterButtonPressed of (int * int)

    let init () =
        []
        , WordsApi.getRhymeCandidates (decimal 2, decimal 7) "sex"
        |> Async.map WordApiResult
        |> Cmd.ofAsyncMsg 

    let update msg model =
        match msg with
        | WordApiResult (Ok words) ->
             List.scan (fun acc word -> word::acc) [] words
            |> List.rev
            |> List.pick WordGrid.tryPlaceWords
            , Cmd.none
        | WordApiResult (Error _) ->
            model, Cmd.none
        | LetterButtonPressed coordinate ->
            model, Cmd.none

    let view (model: Model) dispatch =
        let button point = 
            match WordGrid.at point model with
            | None -> View.Label(text = " ")
            | Some char -> 
                View.Button(
                    text = Char.ToString char
                    , command = (fun _ -> LetterButtonPressed point |> dispatch))
                |> ignore
                View.Label(Char.ToString char)
        
        let gridSide = WordGrid.gridSize
        View.ContentPage(
            View.Grid(
                rowdefs = [ for _ in 1..gridSide do yield "*" ]
                , coldefs = [ for _ in 1..gridSide do yield "*" ]
                , rowSpacing = 5.0
                , columnSpacing = 5.0
                , backgroundColor = Color.Aquamarine
                , children = [
                    for x in 1..gridSide do
                    for y in 1..gridSide do
                    yield (button (x,y))
                        .GridRow(x).GridColumn(y)
                ]
            ))

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


