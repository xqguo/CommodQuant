#r "nuget:FSharp.Data"
#r "nuget:Nager.Date, 1.33.1"
#r "bin/Debug/netstandard2.0/CommodLib.dll"
open System
open System.IO
open Nager.Date
open FSharp.Data
open Commod
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

//exchange holiday from platts, not complete
let isExchange (s:string) (e:HtmlNode) = 
    let d = ( e.Descendants ["div"] |> Seq.head ).InnerText() |> parseDateExact "ddMMMyyyy" 
    // let desc = ( e.Descendants ["div"] |> Seq.skip 1 |> Seq.head ).InnerText()
    // let offices = ( e.Descendants ["div"] |> Seq.skip 2 |> Seq.head ).Descendants ["li"] |> Seq.map ( fun x -> x.InnerText() |> String.trim) |> set
    if ( e.Descendants ["div"] |> Seq.skip 3 |> Seq.head ).InnerText().Contains s  then d else None 

let getHol f = 
    holRows
    |> Seq.choose f
    |> set

//compute other dates using e from Nager.Date
let td = DateTime.Today
let yearRange = [td.Year..2050]
let formatHoliday (h : Model.PublicHoliday) =
    let y, m, d = h.Date.Year, h.Date.Month, h.Date.Day
    sprintf "(%i, %2i, %2i), // %s/%s" y m d h.Name h.LocalName

let ukFull = 
    yearRange
    |> Seq.collect (fun y -> DateSystem.GetPublicHolidays (y , CountryCode.GB) )
    |> Seq.filter( fun y -> ( isNull y.Counties ) || (y.Counties |> Array.contains "GB-ENG" ))

let ukHol = 
    ukFull
    |> Seq.map( fun y -> y.Date)
    |> set

//let ukBank = 
//    ukFull
//    |> Seq.filter( fun y -> 
//        y.LocalName.ToLower().Contains("bank") )
//    |> Seq.map( fun y -> y.Date)
//    |> set

let goodfridays = 
    //ice holiday is a subset of uk holidays
    //DateSystem.GetPublicHoliday (2021 , CountryCode.GB) 
    //|> Seq.filter( fun y -> isNull y.Counties || y.Counties |> Array.contains "GB-ENG" )
    //|> Seq.toArray
    let iceEvents = [ "good friday" ] |> set
    ukFull
    |> Seq.filter( fun y -> Set.contains (y.LocalName.ToLower()) iceEvents )
    |> Seq.map( fun y -> y.Date)
    |> set

let iceHol = 
    //ice holiday is a subset of uk holidays
    //DateSystem.GetPublicHoliday (2021 , CountryCode.GB) 
    //|> Seq.filter( fun y -> isNull y.Counties || y.Counties |> Array.contains "GB-ENG" )
    //|> Seq.toArray
    let iceEvents = [ "new year's day"; "good friday"; "christmas day"] |> set
    ukFull
    |> Seq.filter( fun y -> Set.contains (y.LocalName.ToLower()) iceEvents )
    |> Seq.map( fun y -> y.Date)
    |> set
    |> Set.union goodfridays
    |> Set.remove( DateTime(2022,12,27)) //chrismas vs boxing day label error in nager
    |> Set.add( DateTime(2022,12,26))

    //|> Set.union (isExchange "ICE" |> getHol) //platts ICE info not applicable to trading days

let nymHol = 
    //nymex holiday is US holidays 
    //plus good friday ( which is already included in nager.date for US)
    //excluding New year's Day 
    yearRange
    |> Seq.collect (fun y -> DateSystem.GetPublicHolidays (y , CountryCode.US) )
    |> Seq.filter( fun y -> not (y.LocalName = "New Year's Day" && y.Date.Month = 12))
    |> Seq.filter( fun y -> y.Global = true || y.LocalName = "Good Friday")
    |> Seq.map( fun y -> y.Date)
    |> set
    //|> Set.union goodfridays
    //|> Set.union (isExchange "NYMEX" |> getHol)
 
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
    Set.union (getHol isPLTLDN) ukHol

//save the data into Library/holidays
//keep data for past years only
let root = __SOURCE_DIRECTORY__ + @"/../Library/holidays/"
let saveHoliday f (hol:Set<DateTime>) =
    let fn = root + f 
    let h = if File.Exists fn then File.ReadAllLines fn else Array.empty
    h 
    |> Array.map DateTime.Parse
    |> Array.filter( fun d -> d.Year < td.Year ) //keep last yr and before intact
    |> Array.append( hol |> Set.toArray )
    |> Array.distinct
    |> Array.sort
    |> Array.map( fun d -> d.ToString("yyyy-MMM-dd") )
    |> (fun x -> File.WriteAllLines ( fn, x ) ) 

saveHoliday "PLTSGP.txt" pltsgpHol
saveHoliday "PLTLDN.txt" pltldnHol
saveHoliday "ICE.txt" iceHol
saveHoliday "CME.txt" nymHol
saveHoliday "UK.txt" ukHol

// open Commod.Contracts.Conventions
// let c = getCommod TTF 
// let cnt = c.Contracts.Opt
// let r = 
//     Map.filter( fun k v -> v <> (pillarToDate k  |>  getTtfOptExp) ) cnt
//     |> Map.map(  fun k v -> v , (pillarToDate k  |>  getTtfOptExp) ) 
// //r |> Map.count < 2 //1 known diffs
// Map.isEmpty r