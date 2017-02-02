namespace Takedown.Restaurants
module Allehjornet =
    open HtmlAgilityPack;
    open HtmlAgilityPack.FSharp
    open Parsing;
    open System.Text.RegularExpressions
    open Http;

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
