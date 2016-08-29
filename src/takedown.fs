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


open HtmlAgilityPack;
open Newtonsoft.Json;
open Newtonsoft.Json.Serialization;
open HtmlAgilityPack.FSharp
open Parsing;
open Http;

type MenuEntry = { Number : int; Name : string;  Price : decimal}
type MenuCategory = { Category : string; Entries : seq<MenuEntry> }

let mapMenuItemHeader (b : HtmlNode) =
    let inner =  b.InnerText.Trim();
    match inner with
    | Regex "^\[(\d*?)\](.*)" [number; name] -> Some(System.Int32.Parse number, name.Trim())
    | _ -> None

let parsePrice (node : HtmlNode) =
    match node.InnerText with
    | Regex "(\d+)" [price] -> System.Decimal.Parse price
    | _ -> 0m

let mapMenuEntry node =
    let bs = node |> descendants "b" |> Seq.toList
    match bs with
    | headerNode::priceNode::_ ->
        match mapMenuItemHeader headerNode with
        | Some (number, name) -> Some { Number = number; Name = name; Price = parsePrice priceNode }
        | None -> None
    | _ -> None

let menuCategory node =
    let xpath = "//div[@class='box box_3']//div[@align='center']"
    match node with
    | SelectNodes xpath [first] -> first.InnerText.Trim()
    | _ -> "UNKNOWN"

let menuEntries node =
    let xpath = "//div[@data-is-row='true']//div[@class='richtextContent clearfix']"
    let menuItemNodes = node |> selectNodes xpath
    { 
        Category = menuCategory node
        Entries =  menuItemNodes |> Seq.choose mapMenuEntry |> Seq.sortBy (fun x -> x.Number)
    }
    
let menyUrls doc = 
    doc
    |> selectNodes "//div[@class='treemenuWrapper']//li//a" 
    |> Seq.map (attr "href")


let baseUrl = "http://takesushi.no"
let relative x = (baseUrl + x)
let discover s = createDoc s |> menyUrls |> Seq.map relative
let takeDown () = crawl [ (relative "/meny/forretter/") ] [] discover

let toJson x =
   let settings = new JsonSerializerSettings ( ContractResolver = new CamelCasePropertyNamesContractResolver())
   JsonConvert.SerializeObject(x, Formatting.Indented, settings)

[<EntryPoint>]
let main argv = 
    let menu =
        takeDown () 
        |> Seq.map (fun (_, doc) -> createDoc doc) 
        |> Seq.map menuEntries
        |> toJson
    printfn "%s" menu
    0 
