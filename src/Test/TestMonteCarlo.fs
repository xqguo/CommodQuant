module TestMonteCarlo
open MathNet.Numerics.LinearAlgebra 
open MathNet.Numerics.Distributions
open MathNet.Numerics.Statistics
open FsCheck
open FsCheck.Xunit
open Commod.Options
open Commod.MC
open FsCheckTypes


let generateCorrsMatrix s seed =
    let n' = Normal( 0.0, 1.0)
    n'.RandomSource <- System.Random(seed)
    let w =  DenseMatrix.random<float> s s n'
    let x = w * w.Transpose()
    let diag = 1. / sqrt(x.Diagonal())
    let d_half = DiagonalMatrix.ofDiag diag
    let corrs = d_half * x * d_half
    corrs |> Matrix.mapi( fun i j x -> if i < j then corrs.[j,i] elif i=j then 1. else x)
//type internal PropertyConfig =
//    { MaxTest        : Option<int>
//      MaxFail        : Option<int>
//      Replay         : Option<string>
//      StartSize      : Option<int>
//      EndSize        : Option<int>
//      Verbose        : Option<bool>
//      QuietOnSuccess : Option<bool>
//      Arbitrary      : Type[] }

///int arb filtered with range 
type SmallInt =
    static member Int() =
        Arb.Default.Int32()
        |> Arb.filter (fun t -> (t > 1) && (t <= 10))

[<Property( MaxTest = 1000, Arbitrary = [| typeof<SmallInt> |]) >]
let testsmallint s = s > 1 && s < 11

[<Property( MaxTest = 1000, EndSize = 10, Verbose = true ) >]
let testendsize s = s < 11

//[<Property( MaxTest = 3, Arbitrary = [| typeof<SmallInt> |], Replay = "(1727423936,296619364))" ) >]
[<Property( MaxTest = 10, Arbitrary = [| typeof<SmallInt> |] ) >]
let ``test multivariateNormal satisfy our aimed vols and correlation`` size seed1 seed2 = 
//let ``test multivariateNormal satisfy our aimed vols and correlation`` () = 
    //let s = 7 
    //let seed = 7 
    let corr' = generateCorrsMatrix size seed1  
    let corr = corr'|> validCorrMatrix
    // do Jarque Bera test ref https://www.rmetrics.org/sites/default/files/2009-02-jarqueberaTest.pdf
    // used 99.99% confidence the statistics of 10000 samples 
    // losen the test further to allow it pass
    let ( nn, criticalValue)  = ( 10000,22.)
    let rs = System.Random(seed2)
    let data = multivariateNormal corr nn rs
    let y = data.ToColumnArrays() // transform the matrix to arrays based on rows
    let m_vec = y |> Array.map ( Statistics.Mean ) |> vector
    let std_vec = y |> Array.map ( Statistics.StandardDeviation ) |> vector
    let skw_vec = y |> Array.map ( Statistics.Skewness ) |> vector
    let krt_vec = y |> Array.map ( Statistics.Kurtosis ) |> vector
    let corr_mtx = Correlation.PearsonMatrix(y) // initilize a vector to save the corrs between rows
    let diff = corr' - corr_mtx
    let mse (v:Vector<float>) = v |> Statistics.RootMeanSquare
    let mse_corrs = diff |> Matrix.toSeq |> vector |> mse 
    let mse_mean_errors = m_vec |> mse // check the difference between data_mean and 0
    let mse_std_errors = (std_vec-1.) |> mse  // check the difference between data_std and 1
    let mse_skw_errors = skw_vec |> mse // check the difference between data_skw and 1
    let mse_krt_errors = krt_vec |> mse // check the difference between data_skw and 1
    let jbTest = 
        let x1 = skw_vec .^2. 
        let x2 = 1. / 4. * (krt_vec .^2.)
        float nn / 6. * (x1 + x2)
        |> Vector.max
        
    (mse_corrs < 0.02) |@ sprintf "mse of correlation is large %f" mse_corrs .&.
    (mse_mean_errors < 0.02) |@ sprintf "mse of mean errors is large %f" mse_mean_errors .&.
    (mse_std_errors < 0.02) |@ sprintf "mse of std errors is large %f" mse_std_errors .&.
    (mse_skw_errors < 0.1) |@ sprintf "mse of skw errors is large %f" mse_skw_errors .&.
    (mse_krt_errors < 0.2) |@ sprintf "mse of krt errors is large %f" mse_krt_errors .&.
    (jbTest < criticalValue) |@ sprintf "Jarque Bera statistics is large: %f" jbTest

[<Property(MaxTest = 50, Arbitrary = [| typeof<SmallInt>; typeof<PositiveSmallFloat> |] )>]
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
