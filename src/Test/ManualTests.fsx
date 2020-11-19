#r "nuget:FSharp.Data"
//#r "nuget:MathNet.Numerics.FSharp"
//#r "nuget:QLNet"
//#r "nuget:FsCheck"
#r "../Library/bin/Release/netstandard2.0/CommodLib.dll"
open System
//open FSharp.Reflection
//open FsCheck
//open MathNet.Numerics.LinearAlgebra
//open MathNet.Numerics.Statistics
//open MathNet.Numerics.Distributions
//open QLNet
open Commod
//open Commod.Contracts.Conventions

Commod.IOcsv.ROOT <- (IO.Path.Combine( Environment.GetEnvironmentVariable "OneDrive", @"Commodities\bin"))

let inst = JKM
let f = getPrices inst
let d1,d2 = getPeriod "Cal21"
#time
SwapPricer inst d1 d2 f 
f.Item "FEB-21"
f.Item "MAR-21"
f.Item "APR-21"

let a = (5M<USD> / 1M<bbl>) |> USDBBL
let (USDBBL b ) = a
b * 1M
