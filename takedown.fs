open HtmlAgilityPack;
open System.Net.Http;
open System.Text.RegularExpressions;
open Newtonsoft.Json;
open HtmlAgilityPack.FSharp

let selectNodes (path : string) (node : HtmlNode) =
    node.SelectNodes(path) |> Seq.cast<HtmlNode>

let fetchAsync (url : string) = async {
    let http = new HttpClient()
    let! html = http.GetStringAsync url |> Async.AwaitTask
    return html}

//let fetch (url : string) =
//    fetchAsync url |> Async.RunSynchronously

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let tryParseInt str =
    match System.Int32.TryParse(str) with
    | (true, int) -> Some int
    | _ -> None
let parseInt str = System.Int32.Parse str

let tryParseDecimal str =
    match System.Decimal.TryParse(str) with
    | (true, decimal) -> Some decimal
    | _ -> None

let (|Int|_|) str = tryParseInt
let (|Decimal|_|) str = tryParseDecimal


type MenuEntry = { Number : int; Name : string;  Price : decimal}
type MenuCategory = { Name : string; Entries : seq<MenuEntry> }

let mapMenuItemHeader (b : HtmlNode) =
    let inner =  b.InnerText.Trim();
    match inner with
    | Regex "^\[(\d*?)\](.*)" [number; name] -> Some(parseInt number, name)
    | _ -> None

let hasRealContent (x : string) = 
    let c = System.String.IsNullOrWhiteSpace(x.Trim())
    not c

let hasText (node : HtmlNode) = hasRealContent node.InnerText 

let parsePrice (node : HtmlNode) =
    tryParseDecimal (node.InnerText.Replace(",-","").Trim())

//ugly
let mapMenuEntry node =
    if not (hasText node) then None
    else
    let bs = node |> descendants "b"
    if Seq.isEmpty bs then None
    else
    let k = bs |> Seq.skip 1 |> Seq.head
    let price = 
        match parsePrice k with
        |Some x -> x
        |None -> 0M
    match mapMenuItemHeader (Seq.head bs) with
    | Some (number, name) -> Some { Number = number; Name = name; Price = price }
    | None -> None


let menyEntries node =
    { 
        Name = ""
        Entries =  
            node
            |> selectNodes "//div[@data-is-row='true']//div[@class='richtextContent clearfix']"
            |> Seq.choose mapMenuEntry
            |> Seq.sortBy (fun x -> x.Number)
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
   Newtonsoft.Json.JsonConvert.SerializeObject(x, Newtonsoft.Json.Formatting.Indented)

[<EntryPoint>]
let main argv = 
    let menu =
        takeDown () 
        |> Seq.map (fun (_, doc) -> doc) 
        |> Seq.map menyEntries
        |> toJson
    printfn "%s" menu
    0 
