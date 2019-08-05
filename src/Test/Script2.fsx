#r "../../packages/NETStandard.Library/build/netstandard2.0/ref/netstandard.dll "
#r "bin/Debug/netstandard2.0/CommodLib.dll"
#I "../../.paket/load/"
#load "MathNet.Numerics.FSharp.fsx"
#load "FsCheck.fsx"
#load "FsCheck.Xunit.fsx"
open FsCheck
open FsCheck.Xunit
open Utils
open System

type SmallInt =
    static member Int() =
        Arb.Default.Int32()
        |> Arb.filter (fun t -> (t > 1) && (t < 11))

type ValidDate = 
    static member Date() =
        Arb.Default.DateTime()
        |> Arb.filter (fun t -> (t.Year >= 1980) && (t.Year <=2080))

let holiday = [DateTime(2019,10,1);DateTime(2019,10,2);DateTime(2019,10,3);DateTime(2019,10,4);DateTime(2019,10,5)]|> set

let testaddbusinessday n1 n2  (dt:DateTime) =
    let date1 = addBusinessDay n1 holiday dt
    let date2 = addBusinessDay -n2 holiday dt
    if isBusinessDay holiday dt then
        date1 = addBusinessDay (n1+n2)  holiday date2
    else
        date1 = addBusinessDay (n1+n2-1)  holiday date2


Check.One({ Config.Quick with MaxTest = 500;Arbitrary = [typeof<SmallInt>; typeof<ValidDate>]}, testaddbusinessday)
 
let testnumsBizdays (d1:DateTime) n0 n1 (n2:int) = 
    //let d1 = DateTime(1981,7,19)
    //let n0 = 2
    //let n1 = 4
    //let n2 = 2
    let d2 = d1.AddDays (float n0)
    let num1 = numBizdays holiday d1 d2
    let d3 = addBusinessDay n1 holiday d1
    let d4 = addBusinessDay (n1*n2) holiday d2
    let num2 = numBizdays holiday d3 d4
    num1 = num2 - (n1*n2-n1)
Check.One({ Config.Quick with MaxTest = 500;Arbitrary = [typeof<SmallInt>; typeof<ValidDate>]}, testnumsBizdays)

let week = ["Sunday",0;"Monday",1;"Tuesday",2;"Wednesday",3;"Thursday",4;"Friday",5;"Saturday",6]
let weekmap = week|>Map.ofList
let testdayOfWeek (d0:DateTime) n=
    //let d0 = DateTime(2020, 2, 19)
    //let n = 3
    let d = dayOfWeek d0 n
    let delta_days = (d - d0).Days
    let n' = n % 7
    let dayindex = weekmap.[d0.DayOfWeek.ToString()]
    delta_days = (n' - dayindex + 7) % 7
Check.One({ Config.Quick with MaxTest = 500;Arbitrary = [ typeof<ValidDate>;typeof<SmallInt>]}, testdayOfWeek)

let testmonthOfYear (d0:DateTime) n=
    //let d0 = DateTime(2020, 2, 14)
    //let n = 12
    let n' =
        match n with
        |n when n%12<>0  -> n%12
        |_ ->12
    let d1 = monthOfYear d0 n
    let delta_months = d1.Month - d0.Month
    let month = d0.Month
    delta_months = n' - month

Check.One({ Config.Quick with MaxTest = 500;Arbitrary = [ typeof<ValidDate>;typeof<SmallInt>]}, testmonthOfYear)


type BeginOfCalenderInt =
    static member Int() =
        let sample = [0;1;3;6;12]|>Set
        Arb.Default.Int32()
        |> Arb.filter (fun t -> (sample.Contains t))

let testbeginOfCalendarPeriod (d0:DateTime) (n:int) = 
    let d1 = beginOfCalendarPeriod d0 n
    if (n=0) || (n=1) then
        (d1.Month = d0.Month) && (d1.Day=1) && (d1.Year = d0.Year)
    else
        (d1.Month = d0.Month - (d0.Month - 1) % n) && (d1.Day=1) && (d1.Year = d0.Year)
Check.One({ Config.Quick with MaxTest = 500;Arbitrary = [ typeof<ValidDate>;typeof<BeginOfCalenderInt>]}, testbeginOfCalendarPeriod)


let testdateAdjustIgnoreholiday d0 n =
    //let d0 = DateTime(2000, 6, 18)
    //let n = 2
    let d1 = dateAdjust holiday (string(n)+"w") d0
    let d2 = dateAdjust holiday (string(-n) + "w") d1
    let d3 = dateAdjust holiday (string(7*n) + "d") d0
    let d4 = dateAdjust holiday "1y" d0
    let d5 = dateAdjust holiday "12m" d0
    (d1 = d3) && (d0 = d2) && (d4=d5)
Check.One({ Config.Quick with MaxTest = 500;Arbitrary = [ typeof<ValidDate>;typeof<SmallInt>]}, testdateAdjustIgnoreholiday)

let testdateAdjustaeAZ d0 = 
    //let d0 = DateTime(2032, 5, 2)
    let d1 = dateAdjust holiday "a" d0
    let d2 = dateAdjust holiday "e" d0
    let d3 = dateAdjust holiday "A" d0
    let d4 = dateAdjust holiday "Z" d0
    let cdt1 = (d1.Month = d2.Month) && (d0.Month=d1.Month) && (d1.Day=1) && (d2.AddDays(1.).Month = d0.Month + 1)
    let cdt2 = (d3.Year = d0.Year) && (d4.Year=d0.Year) && (d3.AddDays(-1.).Year + 1 = d3.Year) && (d4.AddDays(1.).Year = d4.Year + 1)
    cdt1 && cdt2
Check.One({ Config.Quick with MaxTest = 500;Arbitrary = [ typeof<ValidDate>]}, testdateAdjustaeAZ)


type MonthString = 
    static member String() =
        let month = ["Jan";"Feb";"Mar";"Apr";"May";"Jun";"Jul";"Aug";"Sep";"Oct";"Nov";"Dec"] |>Set
        Arb.Default.String()
        |> Arb.filter (fun t -> month.Contains t)

type IntLessThan100 = 
    static member Int()=
        Arb.Default.Int32()
        |> Arb.filter (fun t -> (t>=0) && (t<80))


let testgetPeriod  n1 n2 = 
    //let n1 = 22
    //let n2 = 82
    let monthmap = [1,"Jan";2,"Feb";3,"Mar";4,"Apr";5,"May";6,"Jun";7,"Jul";8,"Aug";9,"Sep";10,"Oct";11,"Nov";12,"Dec"] |>Map
    let n1' = 
        match n1 with
        | n1 when n1 % 12 = 0 -> 12
        |_ -> n1 % 12
    let n2' =
        match n2 with
        | n2 when n2 < 10 -> "0" + string(n2)
        | _ -> string(n2)
    let mon = monthmap.[n1']
    let str = mon + n2'
    let startdate, enddate = getPeriod str
    let test1 =(startdate.Year=enddate.Year) && (startdate.Month = enddate.Month)
    let test2 = (startdate.Month = n1') && (startdate.Year = 2000 + n2) && (startdate.Day=1)
    test1 && test2
Check.One({ Config.Quick with MaxTest = 500;Arbitrary = [typeof<IntLessThan100> ]}, testgetPeriod)
    
