module Http
open System.Net.Http;

let fetchAsync (url : string) = async {
    let http = new HttpClient()
    let! html = http.GetStringAsync url |> Async.AwaitTask
    return html}

let fetch (url : string) =
    fetchAsync url |> Async.RunSynchronously
