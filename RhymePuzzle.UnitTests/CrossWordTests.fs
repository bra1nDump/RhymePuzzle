module Tests

open FsUnit.Xunit
open Xunit
open RhymePuzzle.CrossWord

let succeed () = () |> should equal ()
let fail () = false |> should equal true

[<Fact>]
let ``a single word will be positioned at 0, 0 `` () =
    match buildGrid [ [  'l'; 'z' ] ] with
    | Some grid when
        Map.find (0,0) grid = 'l'
        && Map.find (0, 1) grid = 'z' -> succeed()
    | _ -> fail()

[<Fact>]
let ``2d word is positioned and overlaps correctly with 1st `` () =
    match buildGrid [ ['l'; 'z']; ['z'; 'o'; 'b'] ] with
    | Some grid when Map.find (0, 3) grid = 'b' -> succeed()
    | _ -> fail()
