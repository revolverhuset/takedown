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
type MenuCategory = { Name : string; Entries : seq<MenuEntry> }

let mapMenuItemHeader (b : HtmlNode) =
    let inner =  b.InnerText.Trim();
    match inner with
    | Regex "^\[(\d*?)\](.*)" [number; name] -> Some(parseInt number, name)
    | _ -> None

let parsePrice (node : HtmlNode) =
    tryParseDecimal (node.InnerText.Replace(",-","").Trim())

let mapMenuEntry node =
    let bs = node |> descendants "b" |> Seq.toList
    match bs with
    | headerNode::priceNode::_ ->
        let price = 
            match parsePrice priceNode with
            | Some x -> x
            | None -> 0M
        match mapMenuItemHeader headerNode with
        | Some (number, name) -> Some { Number = number; Name = name; Price = price }
        | None -> None
    | _ -> None


let menyEntries node =
    let xpath = "//div[@data-is-row='true']//div[@class='richtextContent clearfix']"
    let menuItemNodes = node |> selectNodes xpath
    { 
        Name = ""
        Entries =  menuItemNodes |> Seq.choose mapMenuEntry |> Seq.sortBy (fun x -> x.Number)
    }
    
let menyUrls doc = 
    doc
    |> selectNodes "//div[@class='treemenuWrapper']//li//a" 
    |> Seq.map (attr "href")


let baseUrl = "http://takesushi.no"

let takeDown () =
    let rec crawl visit found =
        if Seq.isEmpty visit then found
        else
            let visited = found |> Seq.map (fun (url, _) -> url)
            let newStuff = 
                visit 
                |> Seq.except visited 
                |> Seq.map (fun x -> async {
                     let! content = fetchAsync (baseUrl + x)
                     return (x, content |> createDoc) })
                |> Async.Parallel
                |> Async.RunSynchronously
                |> Seq.toList

            let urls = 
                newStuff
                |> Seq.map (fun (_, doc) -> doc)
                |> Seq.map menyUrls
                |> Seq.concat
            crawl (urls |> Seq.except visit) (found |> Seq.append newStuff)
    crawl ["/meny/forretter/"] []

let toJson x =
   let settings = new JsonSerializerSettings ( ContractResolver = new CamelCasePropertyNamesContractResolver())
   JsonConvert.SerializeObject(x, Formatting.Indented, settings)

[<EntryPoint>]
let main argv = 
    let menu =
        takeDown () 
        |> Seq.map (fun (_, doc) -> doc) 
        |> Seq.map menyEntries
        |> toJson
    printfn "%s" menu
    0 
