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

let d = DateTime(2020,8,1)
dateAdjust' "a-1m-1b" d
getExp d BRT
let ins = BRT
let cnts = getContracts ins
cnts.Item "AUG-20"
let avgfwd =  getAvgFwd ins
let d1,d2 = getPeriod "Jun-21"
let dates = getFixingDates avgfwd.Frequency avgfwd.Commod.Calendar d1 d2 |> Seq.toList
let contracts = getFixingContracts (getNrbyContracts avgfwd) dates |> Seq.toList
let getEqualWeights x =
    let n = List.length x
    let w = 1.0 / float n 
    List.replicate n w 
let weights = (getEqualWeights dates)

//chol & svd
let vols= vector [ 0.3 ; 0.2; 0.5]
vols.SubVector(0,3)
let corrs = matrix [ [1.0 ; 0.8 ; 0. ];[0.8; 1.0 ; 0.];[0.0;0.;1.0]] 
let corr11 = matrix [ [0.25 ]] 
let corr12 = matrix [ [0.0 ]] 

corr11.Stack(corr12).Append(corr12.Stack(corr11))

let var = vols.OuterProduct(vols) .* corrs
let ch = var.Cholesky().Factor


ch.Row(1) * ch.Row(1)

ch.Row(0) * ch.Column(0)

let tmp = Array2D.init 10 10 (fun x y -> sprintf "%d,%d" x y)
//3rd row
let row3 = tmp.[2,0..]
//2nd column
let col2 = tmp.[0..,1]

let rulebased = [ "K", 1 ; "K2", 4] |> Map.ofList
let actuals = [ "K", 2 ; "K1", 3] |> Map.ofList
Map.add "K" 100 actuals 
Map.fold (fun acc key value -> Map.add key value acc) rulebased actuals


