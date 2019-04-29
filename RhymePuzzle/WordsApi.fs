namespace RhymePuzzle 

open HttpFs.Client
open Request
open Response
open Hopac
open Infixes
open FSharp.Data

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
                    "ablate",
                    "escalate",
                    "procrastinate",
                    "terminate",
                    "create"
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

    let getRhymeCandidates (minZipf, maxZipf) word =
        async {
            try
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
                    |> List.filter (fun freq -> 
                        try
                            let zipf = freq.Frequency.Zipf
                            minZipf < zipf
                            && zipf < maxZipf
                        with
                        | _ -> false
                        )
                    |> List.map (fun freq -> freq.Word)
                    |> Ok
            with
            | e -> return Error (e.ToString())
        }

    let sampleRhymes =
        RhymesResponseTypeProvider.GetSample().Rhymes.All
        |> Seq.toList
        