#if INTERACTIVE
//hacky run dotnet publish for fsi to find dlls
#I"bin/Debug/netcoreapp1.0/publish/"
#r"HtmlAgilityPack.CssSelectors.NetCore.dll"
#r"HtmlAgilityPack.NetCore.dll"
#r"System.Net.Http.dll"
#r"Newtonsoft.Json.dll"
#load"Parsing.fs"
#load"Http.fs"
#load"HtmlAgilityPack.FSharp.fs"
#endif

open Newtonsoft.Json;
open Newtonsoft.Json.Serialization;
open Restaurants.Take

let toJson x =
   let settings = new JsonSerializerSettings ( ContractResolver = new CamelCasePropertyNamesContractResolver())
   JsonConvert.SerializeObject(x, Formatting.Indented, settings)

[<EntryPoint>]
let main argv = 
    let menu = takeDown () |> toJson
    printfn "%s" menu
    0 
