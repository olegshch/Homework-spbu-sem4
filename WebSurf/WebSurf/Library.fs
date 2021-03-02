namespace WebSurf

open System.Net
open System.IO
open System.Text.RegularExpressions

module WebSurf =

    let regex = new Regex("<a href\s*=\s*\"?(https?://[^\"]+)\"?\s*>")
    
    /// Function that downloads url, prints count of symbols on page and returns it.
    let fetchAsync (url: string) =
       async {
         try
           let request = WebRequest.Create(url)
           let! response = request.AsyncGetResponse()
           let stream = response.GetResponseStream()
           let reader = new StreamReader(stream)
           let html = reader.ReadToEnd()
           do printfn "Read %d characters for %s..." html.Length url
           return Some html
         with
         | _ -> do printfn "error"
                return None
      }
    
    /// Function to get information about all links from start link.
    let getAllLinks url =
        match (url |> fetchAsync |> Async.RunSynchronously) with
        | None -> []
        | Some value -> value |> regex.Matches |> Seq.map(fun x -> x.Groups.[1].Value |> fetchAsync) |> Async.Parallel |> Async.RunSynchronously |> Seq.toList