//#load "FsCheck.fsx"
#r "nuget:FSharp.Data"
#r "nuget:Nager.Date"
#r "bin/Debug/netstandard2.0/CommodLib.dll"
open FSharp.Data
open System
open System.IO
open Commod

type iceFut = CsvProvider<"https://www.theice.com/api/productguide/spec/219/expiry/csv", Culture = "en-US">
type iceOpt = CsvProvider<"https://www.theice.com/api/productguide/spec/218/expiry/csv", Culture = "en-US">
// list of filename to source 
let sources = 
    [ 
       "BRT", @"https://www.theice.com/api/productguide/spec/219/expiry/csv", @"https://www.theice.com/api/productguide/spec/218/expiry/csv"
       "TTF", @"https://www.theice.com/api/productguide/spec/27996665/expiry/csv", @"https://www.theice.com/api/productguide/spec/71085679/expiry/csv"
    ]
let saveIceFutDates f (futurl:string) (opturl:string)= 
    let fut = iceFut.Load futurl
    let opt = iceOpt.Load opturl
    //dates are in mm/dd/yyyy format
    let formatDate str = DateTime.Parse(str, Globalization.CultureInfo.InvariantCulture ).ToString("yyyy-MM-dd")
    //option contracts are shorter than futures contracts, take the common set.
    let futDates = 
        fut.Rows 
        |> Seq.map( fun r -> 
            [ r.``CONTRACT SYMBOL``.Trim([|'='|]) 
              r.LTD |> formatDate ]
            |> String.concat "," )
    
    let optDates = 
        opt.Rows 
        |> Seq.map( fun r -> 
            [ r.``CONTRACT SYMBOL``.Trim([|'='|]) 
              r.``OPTIONS LTD`` |> formatDate ]
            |> String.concat "," )
    //save contract 
    File.WriteAllLines( __SOURCE_DIRECTORY__ +/ "holidays" +/ f + "fut.csv", futDates )
    File.WriteAllLines( __SOURCE_DIRECTORY__ +/ "holidays" +/ f + "opt.csv", optDates )
 
// process each site in the list
sources |> List.unzip3 |||> List.map3 saveIceFutDates 

//cme contracts

//NG https://www.cmegroup.com/CmeWS/mvc/ProductCalendar/Download.xls?productId=444
