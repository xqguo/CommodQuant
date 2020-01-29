#I "../../../.paket/load/"
#load "FSharp.Data.fsx"
#load "MathNet.Numerics.FSharp.fsx"
#load "QLNet.fsx"
#load "FsCheck.fsx"
#r "../Library/bin/Debug/netstandard2.0/CommodLib.dll "
open System
open FSharp.Reflection
open FsCheck
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Statistics
open MathNet.Numerics.Distributions
open QLNet
open Commod



//chol & svd
let vols= vector [ 0.3 ; 0.2; 0.5]

let corrs = matrix [ [1.0 ; 0.8 ; 0. ];[0.8; 1.0 ; 0.];[0.0;0.;1.0]] 

let var = vols.OuterProduct(vols) .* corrs
let ch = var.Cholesky().Factor


ch.Row(1) * ch.Row(1)

ch.Row(0) * ch.Column(0)
