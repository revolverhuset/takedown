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

type MenuEntry = { Number : int; Name : string;  Price : decimal}
type MenuCategory = { Name : string; Entries : seq<MenuEntry> }

//ugly
let mapMenuEntry (node : HtmlNode) =
    let m = new Regex("^\[(?<number>\d*?)\](?<name>.*)")
    let nameblock = (node |> descendants "b" |> Seq.head).InnerText
    let nameMatch = m.Match(nameblock)
    let price = (node |> descendants "b" |> Seq.skip 1 |> Seq.head).InnerText
    //let desc = (node |> descendants "i"  |> Seq.head).InnerText.Replace("\n", "")
    {
        Number = System.Int32.Parse(nameMatch.Groups.["number"].Value)
        Name = nameMatch.Groups.["name"].Value.Trim()
        //Description = desc.Trim()
        Price = System.Decimal.Parse(price.Trim().Replace(",-", " "))
    }

let tryMapMenu node =
    try
        let r = mapMenuEntry node
        Some r
    with _ -> 
        None

let menyEntries node =
    { 
        Name = ""
        Entries =  
            node
            |> selectNodes "//div[@data-is-row='true']//div[@class='richtextContent clearfix']"
            |> Seq.choose tryMapMenu
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
