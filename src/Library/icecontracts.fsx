﻿//#load "FsCheck.fsx"
#r "nuget:FSharp.Data"
#r "bin/Debug/netstandard2.0/CommodLib.dll"

open FSharp.Data
open System
open System.IO
open Commod

type iceFut = CsvProvider<"https://www.theice.com/api/productguide/spec/219/expiry/csv", Culture="en-US">
type iceOpt = CsvProvider<"https://www.theice.com/api/productguide/spec/218/expiry/csv", Culture="en-US">
// list of filename to source
let sources =
    [ "BRT",
      @"https://www.theice.com/api/productguide/spec/219/expiry/csv",
      @"https://www.theice.com/api/productguide/spec/218/expiry/csv"
      "TTF",
      @"https://www.theice.com/api/productguide/spec/27996665/expiry/csv",
      @"https://www.theice.com/api/productguide/spec/71085679/expiry/csv"
      "NG",
      @"https://www.theice.com/api/productguide/spec/6590258/expiry/csv",
      @"https://www.theice.com/api/productguide/spec/6590274/expiry/csv"
      "NBP",
      @"https://www.theice.com/api/productguide/spec/910/expiry/csv",
      @"https://www.theice.com/api/productguide/spec/71085728/expiry/csv"
      "JKM",
      @"https://www.theice.com/api/productguide/spec/6753280/expiry/csv",
      @"https://www.theice.com/api/productguide/spec/71090519/expiry/csv"
      "DBRT",
      @"https://www.theice.com/api/productguide/spec/6753541/expiry/csv",
      @"https://www.theice.com/api/productguide/spec/26535747/expiry/csv" ]

let saveIceFutDates f (futurl: string) (opturl: string) =
    let fut = iceFut.Load futurl
    let opt = iceOpt.Load opturl
    //dates are in mm/dd/yyyy format
    let formatDate str =
        DateTime
            .Parse(str, Globalization.CultureInfo.InvariantCulture)
            .ToString("yyyy-MM-dd")
    //option contracts are shorter than futures contracts, take the common set.
    let futDates =
        fut.Rows
        |> Seq.toArray
        |> Array.map (fun r ->
            [ r.``CONTRACT SYMBOL``.Trim([| '=' |]); r.LTD |> formatDate ]
            |> String.concat ",")

    let optDates =
        opt.Rows
        |> Seq.toArray
        |> Array.map (fun r ->
            [ r.``CONTRACT SYMBOL``.Trim([| '=' |]); r.``OPTIONS LTD`` |> formatDate ]
            |> String.concat ",")
    //save contract
    let futfn = __SOURCE_DIRECTORY__ +/ "holidays" +/ f + "fut.csv"
    let optfn = __SOURCE_DIRECTORY__ +/ "holidays" +/ f + "opt.csv"

    let saveExpiration fn (d: string[]) =
        let m = d.[0].Substring(0, 7)
        let h = if File.Exists fn then File.ReadAllLines fn else Array.empty
        let x = Array.append (h |> Array.takeWhile (fun l -> l.Substring(0, 7) <> m)) d //keep past intact

        //fix an error in NG expiration from ICE page.
        let y =
            if fn.EndsWith "NGfut.csv" then
                x
                |> Array.map (fun r ->
                    if r = """"Jan28",2027-12-28""" then
                        """"Jan28",2027-12-29"""
                    else
                        r)
            elif fn.EndsWith "NGopt.csv" then
                x
                |> Array.map (fun r ->
                    if r = """"Jan28",2027-12-27""" then
                        """"Jan28",2027-12-28"""
                    else
                        r)
            else
                x

        File.WriteAllLines(fn, y)

    saveExpiration futfn futDates
    saveExpiration optfn optDates
// process each site in the list
sources |> List.unzip3 |||> List.map3 saveIceFutDates |> ignore
