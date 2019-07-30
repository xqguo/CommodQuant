module MC
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Distributions
open MathNet.Numerics.Statistics

let SEED = 0

type CorrMatrix = CorrMatrix of Matrix<float>

let validCorrMatrix (corrs:Matrix<float>) =
    //corr should be square
    if corrs.ColumnCount <> corrs.RowCount then 
        invalidOp "Correlation dimension mismatch!"
    if not (corrs.Diagonal().ForAll( fun x -> x = 1.0 )) then 
        invalidOp "Correlation diagonal should be ones"
    //TODO check symmetric 
    if not <| corrs.IsSymmetric() then 
        invalidOp "Correlation matrix should be symmetric."
    //corr'.Diagonal() |> Seq.filter( fun x -> x <> 1.0)
    try 
        let _ = corrs.Cholesky().Factor
        //check chol
        CorrMatrix corrs
    with
    |_ -> invalidOp ("Cholesky factorize failure.")

///generate 1 sample of multivariate normal random numbers using vols and correlations, 
///and a random source for contination and repeatable results
///output is a vector of returns 
let genReturns (corrs:Matrix<float>) (source:System.Random)  =
    if corrs.ColumnCount <> corrs.RowCount then 
        invalidOp "Correlation dimension mismatch!"
    let mapdup = // map dependent indices to first independent index
        corrs 
        |> Matrix.toSeqi 
        |> Seq.filter( fun (i,j,r) -> r = 1.0 && i>j ) 
        |> Seq.map( fun (i,j,_) -> i,j)
        |> dict
    let maporig = //all old index to reduced index
        List.init corrs.ColumnCount ( fun i -> 
            let m = if mapdup.ContainsKey i then mapdup.[i] else i
            i,m)
        |> dict
    let mapnew =  //reduced to new index
        maporig.Values |> Seq.distinct |> Seq.sortDescending |> Seq.mapi( fun i o -> o,i) |> dict
    let dupkeys = mapdup.Keys |> Seq.sortDescending
    let newcorrs = 
        Seq.fold( fun (state:Matrix<float>) i -> 
            let s = state.RemoveRow i
            s.RemoveColumn i
            ) corrs dupkeys
    let k = CreateMatrix.DenseIdentity<float> 1 
    let means = DenseMatrix.create newcorrs.ColumnCount 1 0.
    let mnormal = MatrixNormal(means, newcorrs, k)
    mnormal.RandomSource <- source 
    let reducedpath = mnormal.Sample()
    let sample = reducedpath.Column 0 
    //printfn "generated %i rows" reducedpath.RowCount
    //reconstruct original dimensions and scale the dependent ones using vols
    DenseVector.init corrs.ColumnCount 
        (fun i -> 
            let i' = maporig.[i]
            //printfn "old index %i is derived from index %i" i i'
            let j = mapnew.[i']
            //printfn "old index %i is mapped to row %i" i' j
            sample.[j])

///simulaton multivariate correlated normal random numbers
///mapps fully correlated variable to the independent ones.
let genMultivariateNormal (CorrMatrix corrs) r =
    let mapdup = // map dependent indices to first independent index
        corrs 
        |> Matrix.toSeqi 
        |> Seq.filter( fun (i,j,r) -> r = 1.0 && i>j ) 
        |> Seq.map( fun (i,j,_) -> i,j)
        |> dict
    let maporig = //all old index to reduced index
        List.init corrs.ColumnCount ( fun i -> 
            let m = if mapdup.ContainsKey i then mapdup.[i] else i
            i,m)
        |> dict
    let mapnew =  //reduced to new index
        maporig.Values |> Seq.distinct |> Seq.sort|> Seq.mapi( fun i o -> o,i) |> dict
    let dupkeys = mapdup.Keys |> Seq.sortDescending
    let newcorrs = 
        Seq.fold( fun (state:Matrix<float>) i -> 
            let s = state.RemoveRow i
            s.RemoveColumn i
            ) corrs dupkeys
    let m = newcorrs.ColumnCount
    let ch = newcorrs.Cholesky().Factor
    let normal = Normal()
    normal.RandomSource <- r
    let samples = normal.Samples() //random normal sequence
    Seq.initInfinite ( fun _ -> 
        let s' =  (ch* (samples |> Seq.take m |> vector ) )
        [| for i in 0 .. (corrs.ColumnCount-1) do yield s'.[mapnew.[maporig.[i]]] |])

let multivariateNormal corrs npath r =
    genMultivariateNormal corrs r |> Seq.take npath |> matrix

let callPayoff strike price = max (price - strike) 0.0
let europeanPayoff payoff assetPath = assetPath |> Seq.last |> payoff
let europeanCallPayoff strike assetPath = assetPath |> europeanPayoff (callPayoff strike)
let asianArithmeticMeanCallPayoff strike (assetPath:seq<float>) = assetPath.Mean() |> callPayoff strike

let getAssetPath S0 r deltaT sigma (normal:Normal) numSamples =
    Seq.unfold (fun S -> let sNew = (S * exp(((r - (0.5 * sigma * sigma)) * deltaT) + (sigma * sqrt(deltaT) * normal.Sample())))
                         Some(sNew, sNew)) S0
    |> Seq.take numSamples

// Monte Carlo pricing
let priceAsianArithmeticMeanMC S0 strike r T sigma numTrajectories numSamples =
    let normal = new Normal(0.0, 1.0)
    let deltaT = T / float numSamples
    let payoffs = seq { for n in 1 .. numTrajectories do 
                        let assetPath = getAssetPath S0 r deltaT sigma normal numSamples |> Seq.toList
                        yield assetPath |> asianArithmeticMeanCallPayoff strike
                      }
    let discountFactor = exp(-r * T)
    let priceMC = discountFactor * payoffs.Mean()
    let stddevMC = discountFactor * payoffs.StandardDeviation() / sqrt(float numTrajectories)
    (priceMC, stddevMC)

let asian () =
    let S0 = 100.0
    let strike = 90.0
    let r = 0.05
    let T = 1.0
    let sigma = 0.2
    let numTrajectories = 100000
    let numSamples = 12

    let result = priceAsianArithmeticMeanMC S0 strike r T sigma numTrajectories numSamples
    printfn "Asian arithmetic mean:%f stddev:%f" (fst result) (snd result)

let asianAmavMonteCarloCall (f1:Vector<float>) (t1:Vector<float>) (v1:Vector<float>) k num = 
    let accu1 x y = 
        exp(-0.5 * x * x * y)
    let accu2 (tdiff:Vector<float>) (z:Vector<float>) = 
        let temp = sqrt(tdiff).PointwiseMultiply(z).Sum()
        temp
    let timeDiff (t:Vector<float>) =
        let length = t.Count
        let lag = DenseVector.create length 0.
        if length > 1 then
            lag.[1..length-1] <- t.[0..length-2]
        else
            lag.[0] <- 0.
        (t -  lag)
    let getAccumulator2  (t:Vector<float>) (v1:Vector<float>) (normal:Vector<float>)= 
        let accumulators = DenseVector.create (t.Count) 0. 
        let tdiff = timeDiff t      
        for i in 0..(t.Count-1) do 
            accumulators.[i] <- (accu1 v1.[i] t.[i]) *  exp(v1.[i] * (accu2 tdiff.[0..i] normal.[0 ..i]))
        accumulators
    let payoff = DenseVector.create num 0.
    for i in 0 .. num-1 do
        let n' = Normal( 0.0, 1.0)
        n'.RandomSource <- System.Random(i)
        let normal = DenseVector.random<float> (t1.Count) n'
        let accumulators2 = getAccumulator2 t1 v1 normal
        //let accumulators = getAccumulator t1 v1 normal
        let future_price = f1.PointwiseMultiply(accumulators2)
        payoff.[i] <- max (future_price.Mean() - k) 0.
    (payoff.Mean(), payoff.StandardDeviation()/sqrt(float num))
