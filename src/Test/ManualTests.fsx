#r "nuget:MathNet.Numerics"
#r "nuget:MathNet.Numerics.FSharp"
#r "../Library/bin/Release/netstandard2.0/CommodLib.dll"
open System
open MathNet.Numerics.LinearAlgebra
open Commod

// Commod.IOcsv.ROOT <- (IO.Path.Combine( Environment.GetEnvironmentVariable "OneDrive", @"Commodities"))
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
#time

//(1.0, 2.0, NormalFloat -1.0, 3, 2, 1.0, 1.0, 2.0, Corr 0.0, Call)
let inst = BRT
let pricecurve = getPriceCurve inst None
let smile = getSmile inst
let pd = DateTime.Today
let refMonth = dateAdjust' "+3m" pd |> formatPillar
let expDate = dateAdjust' "+3me" pd 
let o1 = AsianOptionPricerSmile inst [|0|] BusinessDays 60. Call expDate refMonth pd pricecurve smile
let g = getGabillonParam inst
let o2 = AsianOptionPricerSmileGabillon inst [|0|] BusinessDays 60. Call expDate refMonth pd g pricecurve smile
