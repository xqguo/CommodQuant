#r "nuget:FSharp.Data"

open System
open System.IO
open FSharp.Data
//generate expiration csv files: yyyy-MMM-dd
let ttfOpt =
    (new HtmlProvider<"https://www.theice.com/products/71085679/Dutch-TTF-Gas-Options-Futures-Style-Margin/expiry", Culture="en-US">())
        .Tables.``Dutch TTF Gas Options (Futures Style Margin)``.Rows
    |> Array.map (fun r ->
        $""""{r.``Contract Symbol``}",{DateTime.ParseExact(r.``Options LTD``, "M/d/yyyy", null):``yyyy-MM-dd``}""")

let m = ttfOpt.[0].Substring(0, 7) //e.g. "Jun22"
let root = __SOURCE_DIRECTORY__ + @"/../Library/holidays/"

let saveExpiration f d =
    let fn = root + f
    let h = if File.Exists fn then File.ReadAllLines fn else Array.empty
    let x = Array.append (h |> Array.takeWhile (fun l -> l.Substring(0, 7) <> m)) d //keep past intact
    File.WriteAllLines(fn, x)

saveExpiration "TTFopt.csv" ttfOpt
