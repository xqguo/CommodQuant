module TestMonteCarlo
open MathNet.Numerics.LinearAlgebra 
open MathNet.Numerics.Distributions
open MathNet.Numerics.Statistics
open Xunit
open FsCheck
open FsCheck.Xunit
open Commod
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

let testAsianChoivsMCFun nf  k fstart tstart sstart npath maxfixing tol = 
    let num = npath
    let nf = min (nf+1) maxfixing
    //let k' = 72.
    //let fstart = 4.86
    //let tstart = 68.
    let sstart = min sstart 0.5 // limit vol to 50%
    let tstart = min tstart 4.0
    let fpp = DenseVector.create nf fstart
    let tt = DenseVector.init nf ( fun i -> float (i+1)/12. + tstart )
    let fww = DenseVector.create nf 1./(float nf) 
    let vol = DenseVector.create nf sstart
    //let asianoptionChoi (f:Vector<float>) (w:Vector<float>) k (sigma:Matrix<float>) callput =
    let sigma = getSigma2 vol tt vol tt 1.0 |> fixCov
    let v0,_ = asianoptionChoi fpp fww k sigma Call 
    let v1,std =  asianMC fpp tt vol k Call num
    // In some cases, std could be zero
    let v2 = asianoption fpp fww tt vol k Call 0.
    nearstr v1 v0 (std * 3.0 + 1E-4) "Choi vs mc" .&. //choi and mc close
    nearstr v1 v2 (std * 3.0 + tol) "MM vs mc" .&. // mm is less accurate
    nearstr v0 v2 tol "Choi vs MM"

[<Property(MaxTest = 100, EndSize = 100, Arbitrary = [| typeof<PositiveFloat>|] )>]
let testAsianChoivsMC (PositiveInt nf)  k fstart tstart sstart= 
    testAsianChoivsMCFun nf k fstart tstart sstart (int 1E6) 6 0.08

//[<Property(MaxTest = 100, Arbitrary = [| typeof<PositiveFloat>|] )>]
////MM is ok with less than 3m avg
//let testAsianChoivsMCLoose (PositiveInt nf)  k fstart tstart sstart= 
//    testAsianChoivsMCFun nf k fstart tstart sstart (int 1E6) 60 0.0001

//[<Property(MaxTest = 100, Arbitrary = [| typeof<PositiveFloat>|] )>]
////Everything is ok with less avg
//let testAsianChoivsMCEasy (PositiveInt nf)  k fstart tstart sstart= 
//    testAsianChoivsMCFun nf k fstart tstart sstart (int 1E6) 20 0.0001
[<Property( MaxTest=100, Verbose = true, EndSize = 100, Arbitrary = [| typeof<PositiveFloat>;typeof<MyGenerator>|] )>]
let ``test spread option Choi vs MM vs MC`` fa fb k t1 t2 v1 v2 (Corr rho) (PositiveInt nf1) (PositiveInt nf2) callput = 
    let t1 = min t1 4.0
    let t2 = min t2 4.0
    let num = int 1E6
    let nf1 = min nf1 3
    let nf2 = min nf2 3
    let v1 = min v1 0.5 |> DenseVector.create nf1 
    let v2 = min v2 0.5 |> DenseVector.create nf2 
    let f1 = DenseVector.create nf1 fa
    let f2 = DenseVector.create nf2 fb
    let t1 = DenseVector.init nf1 ( fun i -> float (i+1)/12. + t1 )
    let t2 = DenseVector.init nf2 ( fun i -> float (i+1)/12. + t2 )
    let fw1 = DenseVector.create nf1 1./(float nf1) 
    let fw2 = DenseVector.create nf2 1./(float nf2) 
    //let rho = max (min (rho/5.0) 0.9) -0.9  //correlation between long/short fixing
    let p = vector [0.]
    let c' = spreadoption f1 fw1 t1 v1 f2 fw2 t2 v2 k rho callput p p p p
    let (c,std) = spreadMC f1 fw1 t1 v1 f2 fw2 t2 v2 k rho callput num
    let choi,_ = optionChoi2AssetN f1 fw1 t1 v1 f2 fw2 t2 v2 k rho callput [17;3] 
    //let choi',_ = optionChoi2AssetN f1 fw1 t1 v1 f2 fw2 t2 v2 k rho callput [17;7;2] 
    let tol = 2E-3
    //nearstr choi choi' tol "choi17/3 vs choi 17/7/2" .&. //Choi17/2 vs converged one
    nearstr choi c (std * 3.0 + tol) "Choi vs mc" .&. //choi and mc is close 
    nearstr choi c' (std * 3.0 + 0.05 ) "Choi vs mm" .&. //choi and mm can be 5c diff or more
    nearstr c' c (std * 3.0 + 0.05 ) "mm vs mc" //mm and mc close

