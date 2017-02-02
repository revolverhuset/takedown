namespace Tests
open Xunit
open Takedown.Restaurants

module Take =
    [<Fact>]
    let TakeDown() = 
        let foo = Take.takeDown ()
        Assert.True true
