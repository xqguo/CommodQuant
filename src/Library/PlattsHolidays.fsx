#r "../../packages/NETStandard.Library/build/netstandard2.0/ref/netstandard.dll"
#I "../../.paket/load/netstandard2.0/"
#load "main.group.fsx"
#load "FSharp.Data.fsx"
#load "Nager.Date.fsx"
#load "FsCheck.fsx"
#r "bin/Debug/netstandard2.0/CommodLib.dll"
open System
open System.IO
open Nager.Date
open FSharp.Data
open Commod.Utils
//generate holiday csv files: yyyy-MMM-dd
//get current year from platts website, most authorative 
let source = HtmlDocument.Load("https://www.spglobal.com/platts/en/our-methodology/holiday")
let holRows = 
    let holList = 
        source.Descendants ["div"]
        |> Seq.filter ( fun x -> x.HasId "HolidayList" && x.HasClass "") 
        |> Seq.exactlyOne
    holList.Descendants ["div"]
    |> Seq.filter( fun x -> x.HasClass "holidayID filterable-list__row")

let isPLT o (e:HtmlNode) = 
    let d = ( e.Descendants ["div"] |> Seq.head ).InnerText() |> parseDateExact "ddMMMyyyy" 
    // let desc = ( e.Descendants ["div"] |> Seq.skip 1 |> Seq.head ).InnerText()
    let offices = ( e.Descendants ["div"] |> Seq.skip 2 |> Seq.head ).Descendants ["li"] |> Seq.map ( fun x -> x.InnerText().Trim()) |> set
    // let exchanges = ( e.Descendants ["div"] |> Seq.skip 3 |> Seq.head ).Descendants ["li"] |> Seq.map ( fun x -> x.InnerText() |> String.trim) |> set
    if ( offices.Contains o || offices.Contains "All Platts offices" ) then d else None

let isPLTSG (e:HtmlNode) = isPLT "Singapore" e
let isPLTLDN (e:HtmlNode) = isPLT "London" e

let isExchange s (e:HtmlNode) = 
    let d = ( e.Descendants ["div"] |> Seq.head ).InnerText() |> parseDateExact "ddMMMyyyy" 
    // let desc = ( e.Descendants ["div"] |> Seq.skip 1 |> Seq.head ).InnerText()
    // let offices = ( e.Descendants ["div"] |> Seq.skip 2 |> Seq.head ).Descendants ["li"] |> Seq.map ( fun x -> x.InnerText() |> String.trim) |> set
    if ( e.Descendants ["div"] |> Seq.skip 3 |> Seq.head ).InnerText().Contains s  then d else None 

let getHol f = 
    holRows
    |> Seq.choose f
    |> set

//compute other dates using e from Nager.Date
let yearRange = [2018..2050]
let formatHoliday (h : Model.PublicHoliday) =
    let y, m, d = h.Date.Year, h.Date.Month, h.Date.Day
    sprintf "(%i, %2i, %2i), // %s/%s" y m d h.Name h.LocalName

let iceHol = 
    //ice holiday is a subset of uk holidays
    let iceEvents = [ "New year's Day"; "Good Friday"; "Christmas Day"] |> set
    yearRange
    |> Seq.collect (fun y -> DateSystem.GetPublicHoliday (y , CountryCode.GB) )
    |> Seq.filter( fun y -> Set.contains y.LocalName iceEvents )
    |> Seq.map( fun y -> y.Date)
    |> set
    |> Set.union (isExchange "ICE" |> getHol)

let nymHol = 
    //nymex holiday is US holidays plus good friday, which I get from ice 
    yearRange
    |> Seq.collect (fun y -> DateSystem.GetPublicHoliday (y , CountryCode.US) )
    // |> Seq.filter( fun y -> Set.contains y.LocalName iceEvents )
    |> Seq.map( fun y -> y.Date)
    |> set
    |> Set.union iceHol
    |> Set.union (isExchange "NYMEX" |> getHol)
 
//for Singapore ones use timeanddate.com
let pltsgpHol = 
    let sgHol yr = 
        let source2 = HtmlDocument.Load("https://www.timeanddate.com/calendar/print.html?year=" + yr + "&country=63&hol=9&df=1") //63 is singapore
        let holList = 
            source2.Descendants ["table"]
            |> Seq.filter ( fun x -> x.HasId "ch1") 
            |> Seq.exactlyOne
        holList.Descendants ["span"]
        |> Seq.choose( fun x -> x.InnerText() + yr |> parseDateExact "ddMMMyyyy")
    //platts singapore is singapore holiday
    yearRange
    |> Seq.collect( fun y -> sgHol (string y) )
    |> set
    |> Set.union (getHol isPLTSG)

let pltldnHol = 
    //platts ldn is UK holiday
    yearRange
    |> Seq.collect (fun y -> DateSystem.GetPublicHoliday (y , CountryCode.GB) )
    |> Seq.map( fun y -> y.Date)
    |> set
    |> Set.union (getHol isPLTLDN)

//save the data into Library/holidays
//TODO: merge instead overwrite data and validation 
let root = __SOURCE_DIRECTORY__ + @"/../Library/holidays/"
let saveHoliday f (hol:Set<DateTime>) =
    hol
    |> Set.toList
    |> List.sort
    |> List.map( fun d -> d.ToString("yyyy-MMM-dd") )
    |> (fun x -> File.WriteAllLines ( root + f, x ) ) 

saveHoliday "PLTSGP.txt" pltsgpHol
saveHoliday "PLTLDN.txt" pltldnHol
saveHoliday "ICE.txt" iceHol
saveHoliday "CME.txt" nymHol
