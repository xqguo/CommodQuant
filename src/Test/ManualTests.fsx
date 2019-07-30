#I "../../.paket/load/net472/"
#load "MathNet.Numerics.FSharp.fsx"
#load "FsCheck.fsx"
#r "../Library/bin/Release/net472/Library.dll"
open FsCheck
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Statistics
open MathNet.Numerics.Distributions
open MC
/////int arb filtered with range 
//type SmallInt =
//    static member Int() =
//        Arb.Default.Int32()
//        |> Arb.filter (fun t -> (t > 1) && (t <= 10))

//let testsmallint s = s > 1 && s < 11
//Check.One({ Config.Quick with MaxTest = 1000; Arbitrary = [ typeof<SmallInt> ] }, testsmallint)

//check timing for mvn
let n = 5
let corrs = matrix [ [1.0; 0.8; 0. ];[0.8; 1.0 ; 0.];[0.0;0.;1.0]] 
let corr' = corrs |> validCorrMatrix
let r= System.Random(42)
let samples = multivariateNormal corr' r

#time
samples 
|> Seq.take 100000
|> matrix
|> Matrix.toColArrays
|> Correlation.PearsonMatrix



