#r "../../packages/NETStandard.Library/build/netstandard2.0/ref/netstandard.dll"
#I "../../.paket/load/netstandard2.0/"
#load "main.group.fsx"
//#load "FSharp.Data.fsx"
//#load "Nager.Date.fsx"
//#load "FsCheck.fsx"
#r "bin/Debug/netstandard2.0/CommodLib.dll"
open System
open FSharp.Data
open System.IO
open Commod.Utils

type iceFut = CsvProvider<"https://www.theice.com/api/productguide/spec/219/expiry/csv", Culture = "en-US">
type iceOpt = CsvProvider<"https://www.theice.com/api/productguide/spec/218/expiry/csv", Culture = "en-US">
// list of filename to source 
let sources = 
    [ 
       "brt", @"https://www.theice.com/api/productguide/spec/219/expiry/csv", @"https://www.theice.com/api/productguide/spec/218/expiry/csv"
    ]
let saveIceFutDates f (futurl:string) (opturl:string)= 
    let fut = iceFut.Load futurl
    let opt = iceOpt.Load opturl
    //option contracts are shorter than futures contracts, take the common set.
    let futDates = 
        fut.Rows 
        |> Seq.map( fun r -> 
            [ r.``CONTRACT SYMBOL``.Trim([|'='|]) 
              r.LTD.ToString("yyyy-MM-dd")]
            |> String.concat "," )
    
    let optDates = 
        opt.Rows 
        |> Seq.map( fun r -> 
            [ r.``CONTRACT SYMBOL``.Trim([|'='|]) 
              r.``OPTIONS LTD``.ToString("yyyy-MM-dd") ]
            |> String.concat "," )
    //save contract 
    File.WriteAllLines( __SOURCE_DIRECTORY__ +/ "holidays" +/ f + "fut.csv", futDates )
    File.WriteAllLines( __SOURCE_DIRECTORY__ +/ "holidays" +/ f + "opt.csv", optDates )
 
// process each site in the list
sources |> List.unzip3 |||> List.map3 saveIceFutDates 

//cme contracts

//NG https://www.cmegroup.com/CmeWS/mvc/ProductCalendar/Download.xls?productId=444
