namespace RhymePuzzle 

open HttpFs.Client
open Request
open Response
open Hopac
open Infixes
open FSharp.Data

module DataMuseApi =

    type WordsResponseTypeProvider = 
        JsonProvider<"""
        [
            {
                "word": "power",
                "score": 5370,
                "numSyllables": 2,
                "tags": [
                    "f:547.875315"
                ],
                "defs": [
                    "n\tpossession of controlling influence",
                    "n\tone possessing or exercising power or influence or authority",
                    "n\t(physics) the rate of doing work; measured in watts (= joules/second)",
                    "n\tpossession of the qualities (especially mental qualities) required to do something or get something done",
                    "n\ta mathematical notation indicating the number of times a quantity is multiplied by itself",
                    "n\t(of a government or government official) holding an office means being in power",
                    "n\tphysical strength",
                    "n\ta state powerful enough to influence events throughout the world",
                    "n\ta very wealthy or powerful businessman",
                    "v\tsupply the force or power for the functioning of"
                ]
            }
        ]
        """>

    type WordsResponse = WordsResponseTypeProvider.Root array

    let withTry = flip Job.tryWith

    let exnToString (exn: exn) = exn.ToString()

    let rhymes word =
        createUrl Get ("https://api.datamuse.com/words")
        |> queryStringItem "rel_rhy" word
        |> queryStringItem "md" "fd" // produce frequency & defintitions
        |> queryStringItem "max" "10" // lets be respectful of the api
        |> getResponse
        >>= readBodyAsString
        >>- WordsResponseTypeProvider.Parse
        >>- Ok
        |> withTry (exnToString >> Error >> Job.result)
        |> Job.toAsync
        