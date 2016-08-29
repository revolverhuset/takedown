module Restaurants.Allehjornet

open HtmlAgilityPack;
open HtmlAgilityPack.FSharp
open Parsing;

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

let mapName s =
    match s with
    | Regex "(\d+)\.(.*)" [number; name] -> System.Int32.Parse number, name.Trim()
    | _ -> failwith "W00t"

let mapMenuItems doc =
    let namesPath = "//div[@class='itemFullText']//h3"
    let detailsPath = "//div[@class='itemFullText']//p"
    let names = getColumn namesPath doc 
    let details = getColumn detailsPath doc
    details |> Seq.zip (names |> Seq.map mapName) 
    

let pers () = 
    let content = crawl (idnHack baseUrl) findOtherMenuUrls
    content 
    |> Seq.map (fun (url, content) -> content)
    |> Seq.map createDoc
    |> Seq.collect mapMenuItems 
