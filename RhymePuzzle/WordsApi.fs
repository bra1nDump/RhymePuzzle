namespace RhymePuzzle 

open HttpFs.Client
open Request
open Response
open Hopac
open Infixes
open FSharp.Data

module Async =
    
    let lift f asyncComputation =
        async {
            let! x = asyncComputation
            return f x
        }

module List =
    
    let bind f = List.map f >> List.concat
    let xs (>>=) f = bind

module String =

    let toList str = 
        let mutable charactersRev = []
        String.iter (fun c -> charactersRev <- c::charactersRev) str
        List.rev charactersRev

module WordsApi =

    type RhymesResponseTypeProvider = 
        JsonProvider<"""
        {
            "word": "consolidate",
            "rhymes": {
                "all": [
                    "abbreviate",
                    "abdicate",
                    "abirritate",
                    "abietate",
                    "ablate",
                    "ablegate",
                    "abnegate"
                ]
             }
        }
        """>

    type FrequencyResponseTypeProvider = 
        JsonProvider<"""
        {
            "word": "apartment",
            "frequency": {
                "zipf": 4.82,
                "perMillion": 65.76,
                "diversity": 0.17
            }
        }
        """>

    type Endpoint =
        | Rhymes
        | Frequency

    let get endpoint word =
        let endpoint = match endpoint with | Rhymes -> "/rhymes" | Frequency -> "/frequency"
        createUrl Get ("https://wordsapiv1.p.rapidapi.com/words/" + word + endpoint)
        |> setHeader (Custom ("X-RapidAPI-Host", "wordsapiv1.p.rapidapi.com"))
        |> setHeader (Custom ("X-RapidAPI-Key", "500a91fb47mshde5d4a490fff295p15b6efjsn7d327a4b6779"))
        |> getResponse
        >>= readBodyAsString
        |> Job.toAsync

    let getRhymeCandidates word =
        async {
            let! rhymesResponse = get Rhymes word
            
            let! rhymes =
                rhymesResponse
                |> RhymesResponseTypeProvider.Parse
                |> fun res -> res.Rhymes.All |> Seq.toList
                |> List.take 10
                |> List.map (get Frequency)
                |> Async.Parallel

            return rhymes
                |> Seq.toList
                |> List.map FrequencyResponseTypeProvider.Parse
                |> List.map (fun freq -> freq.Word)
        }

    let sampleRhymes =
        RhymesResponseTypeProvider.GetSample().Rhymes.All
        |> Seq.toList
        |> List.map String.toList
        