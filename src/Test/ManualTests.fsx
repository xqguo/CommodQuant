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
open MC
open System
open QLNet
open Utils
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