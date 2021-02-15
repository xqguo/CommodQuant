#r "nuget:FSharp.Data"
#r "nuget:MathNet.Numerics"
#r "nuget:MathNet.Numerics.FSharp"
//#r "nuget:QLNet"
#r "nuget:FsCheck"
#r "../Library/bin/Release/netstandard2.0/CommodLib.dll"
open System
//open FSharp.Reflection
open FsCheck
open MathNet.Numerics.LinearAlgebra
//open MathNet.Numerics.Statistics
//open MathNet.Numerics.Distributions
//open QLNet
open Commod
//open Commod.Contracts.Conventions

Commod.IOcsv.ROOT <- (IO.Path.Combine( Environment.GetEnvironmentVariable "OneDrive", @"Commodities\bin"))

//let inst = JKM
//let f = getPrices inst
//let d1,d2 = getPeriod "Cal21"
//#time
//SwapPricer inst d1 d2 f 
//f.Item "FEB-21"
//f.Item "MAR-21"
//f.Item "APR-21"

//let a = (5M<USD> / 1M<bbl>) |> USDBBL
//let (USDBBL b ) = a
//b * 1M
//Gen.choose (0,5 ) |> Gen.sample 2 10
//Gen.elements ["Jan";"Feb"] |> Gen.sample 0 10
//Arb.Default.NormalFloat().Generator |> Gen.resize 2 |> Gen.sample 0 10

//type ColorEnum = Red=0 | Yellow=1 | Blue=2 
//int ColorEnum.Yellow
//Gen.shuffle [1;2;3] |> Gen.sample 0 10
//Arb.Default. Array<int>().Generator |> Gen.sample 10 2
let t = toVector [| 0 .. 2 |] / 12. + 1.
let w = DenseVector.create t.Count (1./float t.Count)
let f1 = DenseVector.create t.Count 0.381431961
let f2 = DenseVector.create t.Count 6.
let v = DenseVector.create t.Count 0.5
let fw1 = w
let v1 = v
let t1 = t
let fw2 = w 
let t2 = t
let v2 = v
let k = -5.
let rho = 0.78
let p = vector [0.]
let cp = Call
#time
optionChoi2AssetN f1 fw1 t1 v1 f2 fw2 t2 v2 k rho cp [7;3;2]
optionChoi2AssetN f1 fw1 t1 v1 f2 fw2 t2 v2 k rho cp [17;7;5;3;2]
spreadoption f1 fw1 t1 v1 f2 fw2 t2 v2 k rho cp p p p p 

//(1.0, 2.0, NormalFloat -1.0, 3, 2, 1.0, 1.0, 2.0, Corr 0.0, Call)
