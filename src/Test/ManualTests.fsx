#r "../../packages/NETStandard.Library/build/netstandard2.0/ref/netstandard.dll "
#r "bin/Debug/netstandard2.0/CommodLib.dll"
#I "../../.paket/load/netstandard2.0/"
#load "MathNet.Numerics.FSharp.fsx"
#load "QLNet.fsx"
#load "FsCheck.fsx"
open FsCheck
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Statistics
open MathNet.Numerics.Distributions
open Commod.MC
open System
open QLNet
open Commod.Utils

//#time
let td = DateTime.Today
let T = dateAdjust' "1y" td
let days = (T - td).TotalDays
let r = InterestRate(0.05, Actual365Fixed(), Compounding.Continuous, Frequency.NoFrequency )
r.discountFactor (366./365.)
r.discountFactor( Date(td), Date(T))
exp(-0.05 * 1.0/365.)

let libor3m = USDLibor(Period(3,TimeUnit.Months))
libor3m.fixingDays()

let ois = FedFunds()
ois.maturityDate( Date(td) )

let depo3m = DepositRateHelper(0.05, libor3m)

depo3m.maturityDate()
depo3m.earliestDate()

let q = SimpleQuote(Nullable(0.02)) :> Quote
let h = Handle(q)
let i = FedFunds()
let oishelpers = 
    [ OISRateHelper(2, Period(24, TimeUnit.Months), h, i )] 
    |> List.map( fun x -> x :> RateHelper)
let instruments = ResizeArray(oishelpers)
let termStructure = PiecewiseYieldCurve<Discount, LogLinear>( Date td, instruments, Actual360())
termStructure.discount( Date td)
termStructure.discount( Date T)

[<Literal>]
let  oisquotes = 
    """
    PILLAR,PRICE
    1M,2.3517
    2M,2.3208
    3M,2.2889
    4M,2.2294
    5M,2.1773
    6M,2.1345
    9M,2.0049
    1Y,1.9131
    18M,1.7788
    2Y,1.6992
    3Y,1.642
    4Y,1.645
    5Y,1.668
    7Y,1.751
    """



//Check.One({ Config.Quick with MaxTest = 1000; Arbitrary = [ typeof<SmallInt> ] }, testsmallint)
open Microsoft.FSharp.Reflection

let toCase (x:'a) = 
    match FSharpValue.GetUnionFields(x, typeof<'a>) with
    | case, _ -> case.Name

let toCase' (x:'a) = 
    match FSharpValue.GetUnionFields(x, typeof<'a>) with
    | case, v -> case.Name, v

let fromString<'a> (s:string) =
    match FSharpType.GetUnionCases typeof<'a> |> Array.filter (fun case -> case.Name = s) with
    |[|case|] -> Some(FSharpValue.MakeUnion(case,[||]) :?> 'a)
    |_ -> None

[<Measure>] type bbl
[<Measure>] type mt 
[<Measure>] type mmbtu 
[<Measure>] type USD 
[<Measure>] type GBP
[<Measure>] type EUR
[<Measure>] type lot
//[<Measure>] type point //percentage point for interest rates
type UnitPrice = 
    | USDBBL of decimal<USD/bbl> 
    | USDMT of decimal<USD/mt> 
    | USDMMBTU of decimal<USD/mmbtu> 

type QuantityAmount = 
    | BBL of decimal<bbl> 
    | MT of decimal<mt> 
    | MMBTU of decimal<mmbtu> 
    | LOT of decimal<lot> 

type CurrencyAmount = 
    | USD of decimal<USD> 
    | EUR of decimal<EUR>
    static member applyCcy (ccy:string) (x:decimal) =  
        match ccy with
        | "USD" -> x * 1M<USD> |> USD
        | "EUR" -> x * 1M<EUR> |> EUR
        | _ -> invalidOp "Unknown ccy"
    member this.toCase = 
        match FSharpValue.GetUnionFields(this, typeof<CurrencyAmount>) with
        | case, v -> 
            let v' = v |> Array.head :?> decimal 
            case.Name, v'
    static member (-) (p1:CurrencyAmount, p2:CurrencyAmount) = 
        let case1, v1 = p1.toCase
        let case2, v2 = p2.toCase
        if case1 = case2 then 
            (v1 - v2) |> CurrencyAmount.applyCcy case1
        else invalidOp "Inconsistent ccy"
    static member (+) (p1:CurrencyAmount, p2:CurrencyAmount) = 
        let case1, v1 = p1.toCase
        let case2, v2 = p2.toCase
        if case1 = case2 then 
            (v1 + v2) |> CurrencyAmount.applyCcy case1
        else invalidOp "Inconsistent ccy"

let a = 1M<USD> |> USD
let b = 1M<USD>
b.ToString()
let (_,[|v|]) = a.toCase
let c,v = a.toCase

v * 10M<USD>

a.toString

FSharpType.GetUnionCases t

// Usage:
type A = X|Y|Z with
     member this.toString = toString this
     static member fromString s = fromString<A> s

X.toString
// val it : string = "X"

A.fromString "X";;
// val it : A option = Some X

A.fromString "W";;
// val it : A option = None

toString X
// val it : string = "X"

fromString<A> "X"
// val it : A option = Some X