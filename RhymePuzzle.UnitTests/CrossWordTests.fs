module Tests

open FsUnit.Xunit
open Xunit
open RhymePuzzle
open WordGrid

let succeed () = () |> should equal ()
let fail () = false |> should equal true

[<Fact>]
let ``a single word will be positioned at 1,1 - 1,2 `` () =
    match tryPlaceWords [ "lz" ] with
    | Some grid when
        Map.find (1,1) grid = 'l'
        && Map.find (1, 2) grid = 'z' -> succeed()
    | _ -> fail()

[<Fact>]
let ``2 words on the same axis do not get positioned overlaping each other`` () =
    let x = tryPlaceWords [ "lz"; "zo" ]
    match tryPlaceWords [ "lz"; "zo" ] with
    | Some grid when
        Map.find (1, 4) grid = 'z'
        && Map.find (1, 5) grid = 'o' -> succeed()
    | _ -> fail()

