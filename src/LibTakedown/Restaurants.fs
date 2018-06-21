namespace Takedown.Restaurants
open HtmlAgilityPack;
open HtmlAgilityPack.FSharp
open Parsing;
open System.Text.RegularExpressions
open Http;

module Take =
    type MenuEntry = { Number : int; Name : string;  Price : decimal}
    type MenuCategory = { Category : string; Entries : MenuEntry list }

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
        | headerNode::priceNode::headerNode2::priceNode2::_ ->
        [
            (match mapMenuItemHeader headerNode with
            | Some (number, name) -> Some { Number = number; Name = name; Price = parsePrice priceNode }
            | None -> None);
            (match mapMenuItemHeader headerNode2 with
            | Some (number, name) -> Some { Number = number; Name = name; Price = parsePrice priceNode2 }
            | None -> None)
        ] |> Seq.choose id |> Seq.toList
        | headerNode::priceNode::_ ->
            match mapMenuItemHeader headerNode with
            | Some (number, name) -> [ { Number = number; Name = name; Price = parsePrice priceNode } ] |> Seq.toList
            | None -> [] |> Seq.toList
        | _ -> [] |> Seq.toList

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
            Entries =  
                menuItemNodes
                |> Seq.map mapMenuEntry
                |> Seq.concat
                |> Seq.sortBy (fun x -> x.Number)
                |> Seq.toList
        }
    
    let menyUrls doc = 
        doc
        |> selectNodes "//div[@class='treemenuWrapper']//li//a" 
        |> Seq.map (attr "href")


    let baseUrl = "http://takesushi.no"
    let relative x = (baseUrl + x)
    let discover s = createDoc s |> menyUrls |> Seq.map relative
    let takeDown () = 
        let pages = crawl (relative "/meny/forretter/") discover
        pages |> Seq.map (fun (_, doc) -> createDoc doc) 
              |> Seq.map menuEntries
              |> Seq.toList

module Allehjornet =

    let replace (x : string) (old : string) neww = x.Replace( old, neww)  
    let idnHack x = 
        //https://github.com/dotnet/corefx/issues/11213
        replace x "allehjørnet.no" "xn--allehjrnet-5cb.no"

    let baseUrl = @"http://allehjørnet.no/index.php/meny/smaretter"
    let noNbsp x = replace x "&nbsp;" " "

    let findOtherMenuUrls s =
        let path = "//div[@class='itemFullText']//a"
        createDoc s
        |> selectNodes path 
        |> Seq.map (attr "href")
        |> Seq.map idnHack

    let getColumn path doc = 
        selectNodes path doc 
            |> Seq.map (fun x -> x.InnerText)
            |> Seq.map noNbsp

    type OptionToken = 
    | Out of int
    | In of string * int

    type Price = | Inside of int| TakeAway of int
    type MenuPrice = | One of Price | Two of Price * Price
    type MenuOption = { Description : string option; Price : MenuPrice}
    type MenuItem = { Name : string; Description: string option; Number : int; Options : list<MenuOption>}

    let rec removeMultipleSpace (x : string) =
        let n = x.Replace("  ", " ").Replace("\t", " ")
        if n = x then x else removeMultipleSpace n

    let trimHeader s =
        match s with 
        | Regex @"(?s)^.*?(\d+\..*)" [content] -> content
        | _ -> failwith "no000"

    let tokenize s =
        match s with
        | Regex "^\(.*?kr (\d+)" [price] -> [ Out(System.Int32.Parse price) ]
        | Regex "(.*?)kr (\d+)" [option; price] ->[In(option.Trim(), System.Int32.Parse price)]
        | _ -> failwith s

    let parseOptions s =
        Regex.Split(s, "(\( ta med kr \d+,- \))")
        |> Array.toList 
        |> List.filter hasContent 
        |> List.filter (fun x -> not (x.Contains("MILD WOK")))
        |> List.collect tokenize
        |> List.pairwise
        |> List.choose(fun (x, y) ->
            match (x, y) with
            |In(name, price), Out(price2) -> Some (name, Two(Inside(price), TakeAway(price2)))
            |In(name, price), _ -> Some (name, One(Inside(price)))
            |Out(_), _ -> None)
        |> List.map(fun (x,y) -> 
        { 
            Description = if hasContent x then Some x else None
            Price = y
        })

    let parseContent s =
        match s with
        | Regex "(?s)^(\(.*?\))(.*)" [description; details] 
            -> (Some description, parseOptions details)
        | s -> (None, parseOptions s)

    let chunkToMenuItem chunk =
        match chunk with
        | Regex @"(?s)(\d+)\.(.*?)([\(\n].*)" [num; name; content;] -> 
            let (description, opts) = parseContent (content.Trim())
            {
                Name = name.Trim(); 
                Number = (System.Int32.Parse num); 
                Options = opts;
                Description = description;
            }
        | _ -> failwith ":("

    let parseMenuitems s =
        let headerless = trimHeader s
        Regex.Split(headerless, "(?s)(\d+\..*?(?=\d+\.))") 
            |> Array.filter hasContent 
            |> Array.map chunkToMenuItem
    
    let mapMenuItems doc =
        let path = "//div[@class='itemFullText']"
        let content = getColumn path doc
        content 
        |> Seq.map removeMultipleSpace 
        |> Seq.map parseMenuitems
    
    let pers () =   
        let content = crawl (idnHack baseUrl) findOtherMenuUrls
        content 
        |> Seq.map (fun (url, content) -> content)
        |> Seq.map createDoc
        |> Seq.collect mapMenuItems
        |> Seq.toList //for debug
