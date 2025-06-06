module TestMonteCarlo

open FsCheck.FSharp
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Distributions
open MathNet.Numerics.Statistics
open FsCheck
open FsCheck.Xunit
open Commod
open FsCheckTypes

let generateCorrsMatrix s seed =
    let n = ContinuousUniform(-0.99, 0.99)
    n.RandomSource <- System.Random(seed)
    let m = DenseMatrix.random<float> s s n

    (m + m.Transpose()) / 2.0
    |> Matrix.mapi (fun i j v -> if i = j then 1.0 else v)
    |> fixCorr

///int arb filtered with range
type SmallInt =
    static member Int() =
        ArbMap.defaults
        |> ArbMap.arbitrary<int>
        |> Arb.filter (fun t -> (t > 1) && (t <= 10))

[<Property(MaxTest = 1000, Arbitrary = [| typeof<SmallInt> |])>]
let testsmallint s =
    (s > 1 && s < 11) |> Prop.label "smallint in (1,11)"

[<Property(MaxTest = 1000, EndSize = 10, Verbose = true)>]
let testendsize s = s < 11 |> Prop.label "endsize < 11"

[<Property(MaxTest = 10, Arbitrary = [| typeof<SmallInt> |])>]
let ``test multivariateNormal satisfy our aimed vols and correlation`` size seed1 seed2 =
    let corr' = generateCorrsMatrix size seed1
    let corr = corr' |> validCorrMatrix
    let nn, criticalValue = (10000, 22.)
    let rs = System.Random(seed2)
    let data = multivariateNormal corr nn rs
    let y = data.ToColumnArrays() // transform the matrix to arrays based on rows
    let m_vec = y |> Array.map (Statistics.Mean) |> vector
    let std_vec = y |> Array.map (Statistics.StandardDeviation) |> vector
    let skw_vec = y |> Array.map (Statistics.Skewness) |> vector
    let krt_vec = y |> Array.map (Statistics.Kurtosis) |> vector
    let corr_mtx = Correlation.PearsonMatrix(y) // initilize a vector to save the corrs between rows
    let diff = corr' - corr_mtx
    let mse (v: Vector<float>) = v |> Statistics.RootMeanSquare
    let mse_corrs = diff |> Matrix.toSeq |> vector |> mse
    let mse_mean_errors = m_vec |> mse // check the difference between data_mean and 0
    let mse_std_errors = std_vec - 1. |> mse // check the difference between data_std and 1
    let mse_skw_errors = skw_vec |> mse // check the difference between data_skw and 1
    let mse_krt_errors = krt_vec |> mse // check the difference between data_skw and 1

    let jbTest =
        let x1 = skw_vec .^ 2.
        let x2 = 1. / 4. * (krt_vec .^ 2.)
        float nn / 6. * (x1 + x2) |> Vector.max

    mse_corrs < 0.02
    |> Prop.label (sprintf "mse of correlation is large %f" mse_corrs)
    .&. (mse_mean_errors < 0.02
         |> Prop.label (sprintf "mse of mean errors is large %f" mse_mean_errors))
    .&. (mse_std_errors < 0.02
         |> Prop.label (sprintf "mse of std errors is large %f" mse_std_errors))
    .&. (mse_skw_errors < 0.1
         |> Prop.label (sprintf "mse of skw errors is large %f" mse_skw_errors))
    .&. (mse_krt_errors < 0.2
         |> Prop.label (sprintf "mse of krt errors is large %f" mse_krt_errors))
    .&. (jbTest < criticalValue
         |> Prop.label (sprintf "Jarque Bera statistics is large: %f" jbTest))

let testAsianChoivsMCFun nf k fstart tstart sstart npath maxfixing tol =
    let num = npath
    let nf = min (nf + 1) maxfixing
    let sstart = min sstart 0.5 // limit vol to 50%
    let tstart = min tstart 4.0
    let fpp = DenseVector.create nf fstart
    let tt = DenseVector.init nf (fun i -> float (i + 1) / 12. + tstart)
    let fww = DenseVector.create nf 1. / (float nf)
    let vol = DenseVector.create nf sstart
    let sigma = getSigma2 vol tt vol tt 1.0 |> fixCov
    let v0, _ = asianoptionChoi fpp fww k sigma Call
    let v1, std = asianMC fpp tt vol k Call num
    let v2 = asianoption fpp fww tt vol k Call 0.

    nearstr v0 v1 (std * 3.0 + 1E-4) "Choi vs mc"
    .&. nearstr v1 v2 (std * 3.0 + tol) "MM vs mc"
    .&. nearstr v0 v2 tol "Choi vs MM"


[<Property(MaxTest = 30, EndSize = 100, Arbitrary = [| typeof<PositiveFloat> |])>]
let testAsianChoivsMC (PositiveInt nf) k fstart tstart sstart =
    testAsianChoivsMCFun nf k fstart tstart sstart (int 1E6) 6 0.08

/// <summary></summary>
/// <param name="v"></param>
/// <returns></returns>
[<Property(MaxTest = 30, Verbose = true, EndSize = 100, Arbitrary = [| typeof<PositiveFloat>; typeof<MyGenerator> |])>]
let ``test spread option Choi vs MM vs MC``
    fa
    fb
    (NormalFloat k)
    t1
    t2
    v1
    v2
    (Corr rho)
    (PositiveInt nf1)
    (PositiveInt nf2)
    callput
    =
    let t1 = min t1 4.0
    let t2 = min t2 4.0
    let num = int 1E6
    let nf1 = min nf1 3
    let nf2 = min nf2 3
    let v1 = min v1 0.5 |> DenseVector.create nf1
    let v2 = min v2 0.5 |> DenseVector.create nf2
    let f1 = DenseVector.create nf1 fa
    let f2 = DenseVector.create nf2 fb
    let t1 = DenseVector.init nf1 (fun i -> float (i + 1) / 12. + t1)
    let t2 = DenseVector.init nf2 (fun i -> float (i + 1) / 12. + t2)
    let fw1 = DenseVector.create nf1 1. / (float nf1)
    let fw2 = DenseVector.create nf2 1. / (float nf2)
    let p = vector [ 0. ]
    let c' = spreadoption f1 fw1 t1 v1 f2 fw2 t2 v2 k rho callput p p p p
    let (c, std) = spreadMC f1 fw1 t1 v1 f2 fw2 t2 v2 k rho callput num
    let choi, _ = optionChoi2AssetN f1 fw1 t1 v1 f2 fw2 t2 v2 k rho callput [ 17; 3; 2 ]
    let tol = 1E-3

    nearstr choi c (std * 3.0 + tol) "Choi vs mc"
    .&. nearstr choi c' (std * 3.0 + 0.05) "Choi vs mm"
    .&. nearstr c' c (std * 3.0 + 0.05) "mm vs mc"
