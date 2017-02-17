#if INTERACTIVE
//hacky run dotnet publish for fsi to find dlls
#r"System.Net.Http.dll"
#I"Users/hsorbo/.nuget/packages"
#I"/Users/hsorbo/.nuget/packages/HtmlAgilityPack.CssSelectors.NetCore/1.0.0/lib/netstandard1.5/"
#r"HtmlAgilityPack.CssSelectors.NetCore.dll"
#I"/Users/hsorbo/.nuget/packages/Newtonsoft.Json/9.0.1/lib/net45/"
#r"Newtonsoft.Json.dll"
#I"/Users/hsorbo/.nuget/packages/HtmlAgilityPack.NetCore/1.5.0.1/lib/net45/"
#r"HtmlAgilityPack.NetCore.dll"
#load"Parsing.fs"
#load"Http.fs"
#load"HtmlAgilityPack.FSharp.fs"
#endif

namespace Takedown
module Main =
    open Newtonsoft.Json;
    open Newtonsoft.Json.Serialization;
    open Takedown.Restaurants

    let toJson x =
        let settings = new JsonSerializerSettings ( ContractResolver = new CamelCasePropertyNamesContractResolver())
        JsonConvert.SerializeObject(x, Formatting.Indented, settings)

    [<EntryPoint>]
    let main argv =
        let menu = Take.takeDown () |> toJson
        printfn "%s" menu
        0 
