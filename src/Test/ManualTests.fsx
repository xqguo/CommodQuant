#I "../../../.paket/load/"
#load "FSharp.Data.fsx"
#load "MathNet.Numerics.FSharp.fsx"
#load "QLNet.fsx"
#load "FsCheck.fsx"
#r "../Library/bin/Debug/netstandard2.0/CommodLib.dll "
open System
open FSharp.Reflection
open FsCheck
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Statistics
open MathNet.Numerics.Distributions
open QLNet
open Commod

let avgfwd = dbrtAvgFwd
let (ContractDates cnts ) = dbrtAvgFwd.Commod.Contracts
let com = getCommod DBRT
let d1,d2 = getPeriod "Jun-21"
let dates = getFixingDates dbrtAvgFwd.Frequency com.Calendar d1 d2 |> Seq.toList
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
