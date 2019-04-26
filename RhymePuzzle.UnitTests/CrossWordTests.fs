module Tests

open FsUnit.Xunit
open Xunit
open RhymePuzzle
open WordGrid

let succeed () = () |> should equal ()
let fail () = false |> should equal true

let samplePlacedWord = 
    { Word = [ 'a'; 'b' ]; Position = 1, 1; Orientation = Horizontal }

[<Fact>]
let ``wordAreaList count length correct`` () =
    wordAreaList samplePlacedWord
    |> List.length
    |> should equal 2

[<Fact>]
let ``area1 cell count correct`` () =
    area1 samplePlacedWord
    |> Set.count
    |> should equal 12

[<Fact>]
let ``1 word is positioned at 1,1 horizontaly`` () =
    match tryPlaceWords [ "lz" ] with
    | Some [{ Position = 1,1; Orientation = Horizontal }] -> succeed()
    | _ -> fail()

[<Fact>]
let ``2 words on the same axis do not overlap`` () =
    match tryPlaceWords [ "lz"; "zo" ] with
    | Some [{ Position = 1,2; Orientation = Vertical }; _] -> succeed()
    | _ -> fail()

[<Fact>]
let ``a word size of grid side fits`` () =
    match tryPlaceWords [ String.init 10 (fun _ -> "o") ] with
    | Some _ -> succeed()
    | None -> fail()

[<Fact>]
let ``2 d long word is placed vertically and overlap`` () =
    let x = tryPlaceWords [ "lz"; "zoooooooo" ]
    match tryPlaceWords [ "lz"; "zoooooooo" ] with
    | Some ({ Position = 1,2; Orientation = Vertical }::_) -> succeed()
    | _ -> fail()

