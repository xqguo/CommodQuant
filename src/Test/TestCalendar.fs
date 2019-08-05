(**
Test calendar using fscheck
========================
*)
module TestCalendar

open System
open FsCheck
open FsCheck.Xunit
open FsCheckTypes
open Commod.Utils

let cal = [ DateTime( 2019,1,1); DateTime(2019,12,25)] |> set

///basic tests for dateAdjust
[<Property>]
let ``test dateAdjust`` d = 
    let dn = dateAdjust cal "n" d 
    let dp = dateAdjust cal "p" d 
    let df = dateAdjust cal "f" d 
    ///nextbd adjust is same as -1b+1b or 0b
    (dn = dateAdjust cal "-1b+1b" d ) |@ "nextbd check" .&. 
    (dn = dateAdjust cal "+0b" d ) |@ "nextbd check" .&.
    ///prevbd adjust is same as 1b-1b
    (dp = dateAdjust cal "+1b-1b" d ) |@ "previousbd check" .&.
    ///add 2b is same as 1b+1b
    ( dateAdjust cal "2b" d = dateAdjust cal "+1b+1b" d ) |@ "plus incremental check" .&.
    ///-2b is same as -1b-1b
    ( dateAdjust cal "-2b" d = dateAdjust cal "-1b-1b" d ) |@ "minus incremental check" .&.
    ///add 2b is same as -3b+5b, if starting from a bd
    ( dateAdjust cal "2b" dn = dateAdjust cal "-3b+5b" dn ) |@ "positive and negative sum check" .&.
    ///modified following adjust never cross the month and same as either n or p 
    ( df.Month = d.Month && (df = dn || df = dp ) ) |@ "modified following check"

[<Property>]
let ``test dateAdjust Q returns beginning of current quarter`` d = 
    let d1 = dateAdjust cal  "Q" d
    let months = [1;4;7;10]
    d.Month >= d1.Month && d.Month - d1.Month < 3 && ( months |> List.contains d1.Month )

[<Property>]
let ``test dateAdjust 3W returns Wednesday`` d = 
    let d1 = dateAdjust cal "3W" d
    d<= d1 && (d1 - d).Days < 7 && ( d1.DayOfWeek = DayOfWeek.Wednesday )

[<Property>]
let ``test dateAdjust failes with unknown adjust str`` d = Prop.throws<Exception,_> (lazy ( dateAdjust cal "*" d))

//[<Property>]
//let ``test monthofYear`` (d0:DateTime) n =  
//    let d1 = monthOfYear d0 n
//    abs <| n % 12 = d1.Month % 12 

[<Property>]
let ``test calmonth`` (d1:DateTime) (d2:DateTime) = 
    let d1' = (min d1 d2).Date
    let d2' = (max d1 d2).Date
    let dates = generateCalMonthSchedule d1' d2'
    (dates |> Seq.head |> fst = d1' ) |@ sprintf "same start %A vs %A" ( dates|> Seq.head ) d1' .&. //format property with data
    (dates |> Seq.last |> snd = d2' ) |@ "same end" .&. 
    ( dates |> Seq.forall( fun (s,e) -> s.Month = e.Month && s.Year = e.Year)) |@ "all periods same month"

[<Property>]
let ``test getPerid`` () =
    (getPeriod "Jan19" = (DateTime(2019,1,1), DateTime(2019,1,31))) |@ "test Jan19" .&.
    (getPeriod "Cal19" = (DateTime(2019,1,1), DateTime(2019,12,31))) |@ "test Cal19" .&.
    (getPeriod "4Q19" = (DateTime(2019,10,1), DateTime(2019,12,31))) |@ "test 4Q19"

//[<Property( MaxTest = 500, Arbitrary = [| typeof<SmallInt>;typeof<ValidDate>|])>]
[<Property>]
let ``testaddbusinessday`` ( PositiveInt n1) ( PositiveInt n2)  (dt:DateTime) =
    let holiday = [DateTime(2019,10,1);DateTime(2019,10,2);DateTime(2019,10,3);DateTime(2019,10,4);DateTime(2019,10,5)]|> set
    let date1 = addBusinessDay n1 holiday dt
    let date2 = addBusinessDay -n2 holiday dt
    let date3 = addBusinessDay (n1+n2)  holiday date2
    let date4 = addBusinessDay (n1+n2-1)  holiday date2
    if isBusinessDay holiday dt then
        date1 = date3 |@ sprintf "Move n1 biz days forward from bizday should be same as move n2 biz days backward and then move n1+n2 forward. %A %A" date1 date3
    else
        date1 = date4 |@ sprintf "Move n1 biz days forward from non-biz day would equal to move n2 backward then n1+n2-1 forward. %A %A" date1 date4

[<Property>]
let ``testnumBizdays`` (d1:DateTime) (d2:DateTime) (PositiveInt n)  = 
    let d1 = d1.Date
    let d2 = d2.Date
    let holiday = [DateTime(2019,10,1);DateTime(2019,10,2);DateTime(2019,10,3);DateTime(2019,10,4);DateTime(2019,10,5)]|> set
    let (a,b) = 
        match d2 with
        | d2 when d2 > d1 -> (d1,d2)
        | _ -> (d2,d1)
  
    let num1 = numBizdays holiday a b
    let d3 = addBusinessDay n holiday a
    let d4 = addBusinessDay n holiday b
    let num2 = numBizdays holiday d3 d4    
    num2 = num1 |@sprintf"Move n biz days forward from a, b, and we get d3, d4, diff between d3, d4 should be the same as diff between a, b %d %d" num1 num2

[<Property>]
let ``testdayOfWeek`` (d0:DateTime) (n: int)=
    let week = ["Sunday",0;"Monday",1;"Tuesday",2;"Wednesday",3;"Thursday",4;"Friday",5;"Saturday",6]
    let weekmap = week|>Map.ofList
    let d = dayOfWeek d0 n
    let delta_days = (d - d0).Days
    let n' = n % 7
    let dayindex = weekmap.[d0.DayOfWeek.ToString()]
    let delta2 = 
        if n >=0 then (n'+ 7 - dayindex) % 7
        else (-(n' + dayindex) - 7) % 7
    let delta3 = abs((d - (dayOfWeek d0 -n)).Days)
    let c = 
        if abs(n') = dayindex || n = 0 then
            delta3 = 0
        else
            delta3 = 7
    delta_days = delta2|@ sprintf "the diff between date and the diff of week days should be consistent %d %d" delta_days delta2 .&.
    c |@ sprintf "if we find the next day of week let say Monday, and then find the last monday, the difference should be zero or 7"


[<Property>]
let ``testmonthOfYear`` (d0:DateTime) n=
    //let d0 = DateTime(1982,2,8)
    //let n = 0
    let n' =
        match n with
        |n when n%12<>0  -> n%12
        |n when n >= 0 ->12
        |_ -> -12
    let d1 = monthOfYear d0 n'
    let d2 = monthOfYear d0 -n'
    let delta_months = d1.Month - d0.Month
    let delta_months2 = abs((d1 - d2).Days)
    let month = d0.Month
    let delta = abs(n') - month
    
    let c = 
        if abs(n') = month  then
            delta_months2 = 0
        else
            delta_months2 >= 365
            
    delta_months = delta |@ sprintf"diff between returned month and expected month should be the same" .&.
    c |@ sprintf"if we find one next month say July, we find last July, the difference days should be zero(if we are in July) or one year"

[<Property( MaxTest = 500, Arbitrary = [| typeof<BeginOfCalenderInt>|])>]
let ``testbeginOfCalendarPeriod`` (d0:DateTime) (n:int) = 
    let d1 = beginOfCalendarPeriod d0 n
    if (n=0) || (n=1) then
        ((d1.Month = d0.Month) && (d1.Day=1) && (d1.Year = d0.Year)) |@ sprintf"When n=0 or 1, the result should have the same month and same year and day1 %A"d1
    else
        ((d1.Month = d0.Month - (d0.Month - 1) % n) && (d1.Day=1) && (d1.Year = d0.Year)) |@ sprintf" Wrong with %A" d1 

[<Property>]
let ``testdateAdjustIgnoreholiday`` d0 n =
    let holiday = [DateTime(2019,10,1);DateTime(2019,10,2);DateTime(2019,10,3);DateTime(2019,10,4);DateTime(2019,10,5)]|> set
    let d1 = dateAdjust holiday (string(n)+"w") d0
    let d2 = dateAdjust holiday (string(-n) + "w") d1
    let d3 = dateAdjust holiday (string(7*n) + "d") d0
    let d4 = dateAdjust holiday "1y" d0
    let d5 = dateAdjust holiday "12m" d0
    (d1 = d3) |@ sprintf" adding n weeks should be the same as add 7 * n days %A %A" d1 d3 .&.
    (d0 = d2) |@ sprintf" adding n weeks and then reduce n weeks on one day, it should be still the same day %A %A" d0 d2 .&.
    (d4 = d5) |@ sprintf "adding 1 year should be the same as adding 12 months %A %A"d4 d5

[<Property>]
let ``testdateAdjustaeAZ`` d0 = 
    let holiday = [DateTime(2019,10,1);DateTime(2019,10,2);DateTime(2019,10,3);DateTime(2019,10,4);DateTime(2019,10,5)]|> set
    //let d0 = DateTime(2020, 12, 29)
    let d1 = dateAdjust holiday "a" d0
    let d2 = dateAdjust holiday "e" d0
    let d3 = dateAdjust holiday "A" d0
    let d4 = dateAdjust holiday "Z" d0
    let cdt1 = (d1.Month = d2.Month) && (d0.Month=d1.Month) && (d1.Day=1) && (d2.AddDays(1.).Month <> d2.Month)
    let cdt2 = (d3.Year = d0.Year) && (d4.Year=d0.Year) && (d3.AddDays(-1.).Year + 1 = d3.Year) && (d4.AddDays(1.).Year = d4.Year + 1)
    cdt1 |@ sprintf" when finding start day and end day in a month of a datetime, it should have the same month, one first day one last day %A %A %A" d0 d1 d2 .&. 
    cdt2 |@ sprintf"when finding the start day and end day in a year, it should have same year, one day is 1/1 and the other is 12/31 %A %A %A" d0 d3 d4


[<Property( MaxTest = 500, Arbitrary = [|typeof<IntLessThan100>|])>]
let ``testgetPeriod``  n1 n2 = 
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
    test1 |@ sprintf"should be the same year and same month %A %A" startdate enddate .&.
    test2 |@ sprintf"should be the same month and same year as the original one %s %A %A" str startdate enddate

