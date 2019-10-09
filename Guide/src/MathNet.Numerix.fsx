(*** hide ***)
#load "../packages/FsLab/Themes/DefaultWhite.fsx"
#load "../packages/FsLab/FsLab.fsx"
(**


MathNet.Numerics
======================== 

It is a library for numerical computation. We can use it for LinearAlgebra, Stats and Distributions etc.

Simulate correlated GBM are useful for Monte Carlo methods used option pricing and PFE etc. 
-----------
*)
#I "../.paket/load/"
#load "MathNet.Numerics.FSharp.fsx"
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Statistics
open MathNet.Numerics.Distributions

let corrs = matrix [ [1.0; 0.8; 0. ];[0.8; 1.0 ; 0.];[0.0;0.;1.0]] 
let ch = corrs.Cholesky().Factor.Transpose() //transpose need if samples are in rows
let r= System.Random(42) //fix a random source for reproducable output
let normrnd = Normal()
normrnd.RandomSource <- r
//10000 sample of 3 correlated variables
let (s:Matrix<float>) = DenseMatrix.random 10000 3 normrnd 
let samples = s * ch

(*** define-output:loading ***)
samples 
|> Matrix.toColArrays
|> Correlation.PearsonMatrix
|> printfn "Similated sample correlation:\n %A" 

printfn "input correlation matrix:\n %A" corrs
(*** include-output:loading ***)




