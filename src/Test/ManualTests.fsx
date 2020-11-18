#r "nuget:FSharp.Data"
#r "nuget:MathNet.Numerics.FSharp"
#r "nuget:QLNet"
#r "nuget:FsCheck"
#r "C:\\Users\\xguo\\OneDrive - Pavilion Energy\\Commodities\\bin\\CommodLib.dll"
open System
open FSharp.Reflection
open FsCheck
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Statistics
open MathNet.Numerics.Distributions
open QLNet
open Commod
open Commod.Contracts.Conventions

Commod.IOcsv.ROOT   <- """C:\\Users\\xguo\\OneDrive - Pavilion Energy\\Commodities\\bin"""

let inst = BRT
let f = getPrices inst
let d1,d2 = getPeriod "Jan21"
SwapPricer inst d1 d2 f 
f.Item "FEB-21"
f.Item "MAR-21"
f.Item "APR-21"
