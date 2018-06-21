module Tests
open Takedown.Restaurants
open System
open Xunit

module TakeTests = 
    [<Fact>]
    let ``Known menuitems`` () =
        let someKnowns = 
            [
                [101..113]
                [115..116]
                [201..206]
                [301..304]
                [401..406]
                [501..519]
                [601..649]
            ] |> List.collect id
        let found = 
            Take.takeDown () 
            |> Seq.collect (fun x -> x.Entries)
            |> Seq.map (fun x -> x.Number)
            |> Seq.toList
        Assert.Empty(someKnowns |> List.except found)
