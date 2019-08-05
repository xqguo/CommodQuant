(**
Test markets using fscheck
========================
*)
module TestMarket

open System
open FsCheck
open FsCheck.Xunit
open Commod
open Commod.Markets
open Deedle

[<Property>]
let ``test getCalendar`` (d: Instrument) =
    let cal = getCalendar d calendars
    let b = Set.empty
    if d = JCC then cal = b
    else cal <> b

[<Property>]
let ``test getJkmPeriod`` () =
    (getJkmPeriod "Nov19" = (DateTime(2019,9,14), DateTime(2019,10,15))) .&.
    (getJkmPeriod "Jul20" = (DateTime(2020,5,16), DateTime(2020,6,15))) .&.
    (getJkmPeriod "Dec20" = (DateTime(2020,10,16), DateTime(2020,11,13)))

[<Property>]
let ``test getJccVolPeriod`` () =
    (getJccVolPeriod "Nov19" = (DateTime(2019,10,1), DateTime(2019,10,31))) .&.
    (getJccVolPeriod "Jul20" = (DateTime(2020,6,1), DateTime(2020,6,30))) .&.
    (getJccVolPeriod "Mar20" = (DateTime(2020,2,1), DateTime(2020,2,29)))


[<Property>]
let ``test getCommod `` (PositiveInt q) (PositiveInt l) (ins:Instrument) =
    let dt = [ ("Mar-19", DateTime(2019, 03, 31)) ; ("Apr-19", DateTime(2019, 4, 30))]|> series |> ContractDates
    let q1 = float q / 100.
    let l1 = float l / 100.
    let test = getCommod q1 l1 ins
    let cal = test.Calendar
    let ctt = test.Contracts
    let qtt = test.Quotation
    let lot = test.LotSize
    let s = Set([BRT;JKM;JCC;TTF])
    (if ins = JCC then cal=Set.empty else cal<>Set.empty) .&.
    (qtt = q1 && lot = l1).&.
    (if not (s.Contains(ins)) then ctt=dt else ctt<>dt)