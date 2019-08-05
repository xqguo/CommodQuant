#r "../../packages/NETStandard.Library/build/netstandard2.0/ref/netstandard.dll "
#r "bin/Debug/netstandard2.0/CommodLib.dll"
#I "../../.paket/load/"
#load "MathNet.Numerics.FSharp.fsx"
#load "FsCheck.fsx"
#load "FsCheck.Xunit.fsx"
#load "FsCheckTypes.fs"
open FsCheck
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Distributions
open MathNet.Numerics.Statistics
open Options
open MC
open FsCheck.Xunit
open FsCheckTypes

let testsmallint s = s > 1 && s < 11
Check.One({ Config.Quick with MaxTest = 1000; Arbitrary = [ typeof<SmallInt> ] }, testsmallint)

let testPositiveSmallFloat s = s > 0. && s < 100.
Check.One({ Config.Quick with MaxTest = 1000; Arbitrary = [ typeof<PositiveSmallFloat> ] }, testPositiveSmallFloat)

[<Property>]
let testDifferentSizeStrikeStart nf  k' fstart tstart sstart= 
    let num = 10000
    //let nf = 2
    //let k' = 72.
    //let fstart = 4.86
    //let tstart = 68.
    //let sstart = 30.
    let fpp = DenseVector.init nf (fun i -> + float (i+1) * 0.2 + fstart)
    let tt = DenseVector.init nf ( fun i -> float (i+1) * 0.2 + tstart / 10. )
    let fww = DenseVector.create nf 1./(float nf) 
    let voll = DenseVector.init nf ( fun i -> (float i) *0.01 + sstart / 100.)
    let v0 = asianoption fpp fww tt voll k' Call 0.
    let v1,std =  asianAmavMonteCarloCall fpp tt voll k' num
    // In some cases, std could be zero
    if (std <> 0.) && (v1 >0.0001) then
        abs(v1-v0) <  3. * std
    else
        v0 < 0.005   // In some extreme cases such as strike price 42 and forward
        //price is very low, price of formua is 0.0025, but monte carlo is 0

Check.One({ Config.Quick with MaxTest = 500;Arbitrary = [typeof<SmallInt>; typeof<PositiveSmallFloat>]  }, testDifferentSizeStrikeStart)

