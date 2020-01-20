#I "../../../.paket/load/"
#load "FSharp.Data.fsx"
#load "MathNet.Numerics.FSharp.fsx"
#load "QLNet.fsx"
#load "FsCheck.fsx"
#r "C:\\Users\\xguo\\OneDrive - Pavilion Energy\\Commodities\\bin\\CommodLib.dll"
open System
open FSharp.Reflection
open FsCheck
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Statistics
open MathNet.Numerics.Distributions
open QLNet
open Commod

let f1 = vector (List.replicate 5 100.)
let fw1 = vector (List.replicate 5 100.)
let t1 = vector (List.init 5 (fun x-> float x/10. + 1.))
let v1 = vector (List.replicate 5 0.3)
let f2 = vector (List.replicate 5 100.)
let fw2 = vector (List.replicate 5 100.) * -1.
let t2 = vector (List.init 5 (fun x-> float x/10. + 1.))
let v2 = vector (List.replicate 5 0.2)
let rho = 0.8
let zerov = vector [0.]
optionChoi f1 fw1 t1 v1 f2 fw2 t2 v2 0.0 rho Call zerov zerov zerov zerov

let m:Matrix<float> = DiagonalMatrix.identity 3
m.Cholesky().Factor.Inverse()
let q = vector [1.; 2.; 0.]
q
let R = householderR q
m*R
