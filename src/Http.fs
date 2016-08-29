module Http
open System.Net.Http;

let fetchAsync (url : string) = async {
    let http = new HttpClient()
    let! html = http.GetStringAsync url |> Async.AwaitTask
    return html}

let fetch (url : string) =
    fetchAsync url |> Async.RunSynchronously


let rec crawl visit found discover =
    if Seq.isEmpty visit then found
    else
        let visited = found |> Seq.map (fun (url, _) -> url)
        let newStuff = 
            visit 
            |> Seq.except visited 
            |> Seq.map (fun x -> async {
                    let! content = fetchAsync x
                    return (x, content) })
            |> Async.Parallel
            |> Async.RunSynchronously
            |> Seq.toList

        let urls = 
            newStuff
            |> Seq.map (fun (_, doc) -> doc)
            |> Seq.collect discover
        crawl (urls |> Seq.except visit) (found |> Seq.append newStuff) discover
