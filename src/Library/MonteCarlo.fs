namespace Commod

[<AutoOpen>]
module MC =
    open MathNet.Numerics.LinearAlgebra
    open MathNet.Numerics.Distributions
    open MathNet.Numerics.Statistics

    let SEED = 0

    type CorrMatrix = CorrMatrix of Matrix<float>

    /// <summary>
    /// Validates that a correlation matrix is suitable for Cholesky decomposition and has ones on the diagonal.
    /// </summary>
    /// <param name="corrs">The correlation matrix to validate.</param>
    /// <returns>A CorrMatrix wrapper if valid; otherwise, throws an exception.</returns>
    let validCorrMatrix (corrs: Matrix<float>) =
        //corr should be square
        //if corrs.ColumnCount <> corrs.RowCount then
        //    invalidOp "Correlation dimension mismatch!"
        if not (corrs.Diagonal().ForAll(fun x -> x = 1.0)) then
            invalidOp "Correlation diagonal should be ones"
        //TODO check symmetric
        //if not <| corrs.IsSymmetric() then
        //    invalidOp "Correlation matrix should be symmetric."
        ////corr'.Diagonal() |> Seq.filter( fun x -> x <> 1.0)
        try
            let _ = corrs.Cholesky().Factor
            //check chol
            CorrMatrix corrs
        with _ ->
            invalidOp ("Cholesky factorize failure.")

    /// <summary>
    /// Generates one sample of multivariate normal random numbers using a correlation matrix and a random source.
    /// </summary>
    /// <param name="corrs">The correlation matrix.</param>
    /// <param name="source">The random number generator.</param>
    /// <returns>A vector of correlated normal returns.</returns>
    let genReturns (corrs: Matrix<float>) (source: System.Random) =
        if corrs.ColumnCount <> corrs.RowCount then
            invalidOp "Correlation dimension mismatch!"

        let mapdup = // map dependent indices to first independent index
            corrs
            |> Matrix.toSeqi
            |> Seq.filter (fun (i, j, r) -> r = 1.0 && i > j)
            |> Seq.map (fun (i, j, _) -> i, j)
            |> dict

        let maporig = //all old index to reduced index
            List.init corrs.ColumnCount (fun i ->
                let m = if mapdup.ContainsKey i then mapdup.[i] else i
                i, m)
            |> dict

        let mapnew = //reduced to new index
            maporig.Values
            |> Seq.distinct
            |> Seq.sortDescending
            |> Seq.mapi (fun i o -> o, i)
            |> dict

        let dupkeys = mapdup.Keys |> Seq.sortDescending

        let newcorrs =
            Seq.fold
                (fun (state: Matrix<float>) i ->
                    let s = state.RemoveRow i
                    s.RemoveColumn i)
                corrs
                dupkeys

        let k = CreateMatrix.DenseIdentity<float> 1
        let means = DenseMatrix.create newcorrs.ColumnCount 1 0.
        let mnormal = MatrixNormal(means, newcorrs, k)
        mnormal.RandomSource <- source
        let reducedpath = mnormal.Sample()
        let sample = reducedpath.Column 0
        //printfn "generated %i rows" reducedpath.RowCount
        //reconstruct original dimensions and scale the dependent ones using vols
        DenseVector.init corrs.ColumnCount (fun i ->
            let i' = maporig.[i]
            //printfn "old index %i is derived from index %i" i i'
            let j = mapnew.[i']
            //printfn "old index %i is mapped to row %i" i' j
            sample.[j])

    /// <summary>
    /// Returns an infinite sequence of samples from a multivariate normal distribution with the given correlation matrix.
    /// </summary>
    /// <param name="CorrMatrix corrs">The validated correlation matrix.</param>
    /// <param name="r">The random number generator.</param>
    /// <returns>An infinite sequence of float arrays, each a sample from the distribution.</returns>
    let genMultivariateNormal (CorrMatrix corrs) r =
        let mapdup = // map dependent indices to first independent index
            corrs
            |> Matrix.toSeqi
            |> Seq.filter (fun (i, j, r) -> r = 1.0 && i > j)
            |> Seq.map (fun (i, j, _) -> i, j)
            |> dict

        let maporig = //all old index to reduced index
            List.init corrs.ColumnCount (fun i ->
                let m = if mapdup.ContainsKey i then mapdup.[i] else i
                i, m)
            |> dict

        let mapnew = //reduced to new index
            maporig.Values |> Seq.distinct |> Seq.sort |> Seq.mapi (fun i o -> o, i) |> dict

        let dupkeys = mapdup.Keys |> Seq.sortDescending

        let newcorrs =
            Seq.fold
                (fun (state: Matrix<float>) i ->
                    let s = state.RemoveRow i
                    s.RemoveColumn i)
                corrs
                dupkeys
            |> fixCorr

        let m = newcorrs.ColumnCount
        let ch = newcorrs.Cholesky().Factor
        let normal = Normal()
        normal.RandomSource <- r
        let samples = normal.Samples() //random normal sequence

        Seq.initInfinite (fun _ ->
            let s' = (ch * (samples |> Seq.take m |> vector))

            [| for i in 0 .. (corrs.ColumnCount - 1) do
                   yield s'.[mapnew.[maporig.[i]]] |])

    /// <summary>
    /// Generates a matrix of samples from a multivariate normal distribution.
    /// </summary>
    /// <param name="corrs">The correlation matrix.</param>
    /// <param name="npath">The number of samples (rows).</param>
    /// <param name="r">The random number generator.</param>
    /// <returns>A matrix where each row is a sample.</returns>
    let multivariateNormal corrs npath r =
        genMultivariateNormal corrs r |> Seq.take npath |> matrix

    /// <summary>
    /// Computes the payoff of a European call option.
    /// </summary>
    /// <param name="strike">The strike price.</param>
    /// <param name="price">The underlying price.</param>
    /// <returns>The call option payoff.</returns>
    let callPayoff strike price = max (price - strike) 0.0

    /// <summary>
    /// Computes the payoff of a European put option.
    /// </summary>
    /// <param name="strike">The strike price.</param>
    /// <param name="price">The underlying price.</param>
    /// <returns>The put option payoff.</returns>
    let putPayoff strike price = max (strike - price) 0.0

    /// <summary>
    /// Computes the payoff of a European-style option given a payoff function and an asset path.
    /// </summary>
    /// <param name="payoff">The payoff function (e.g., callPayoff or putPayoff).</param>
    /// <param name="assetPath">The sequence of asset prices.</param>
    /// <returns>The payoff at maturity.</returns>
    let europeanPayoff payoff assetPath = assetPath |> Seq.last |> payoff

    /// <summary>
    /// Computes the payoff of a European call option given a strike and an asset path.
    /// </summary>
    /// <param name="strike">The strike price.</param>
    /// <param name="assetPath">The sequence of asset prices.</param>
    /// <returns>The call option payoff at maturity.</returns>
    let europeanCallPayoff strike assetPath =
        assetPath |> europeanPayoff (callPayoff strike)

    /// <summary>
    /// Computes the payoff of an Asian arithmetic mean call option given a strike and an asset path.
    /// </summary>
    /// <param name="strike">The strike price.</param>
    /// <param name="assetPath">The sequence of asset prices.</param>
    /// <returns>The Asian call option payoff.</returns>
    let asianArithmeticMeanCallPayoff strike (assetPath: seq<float>) = assetPath.Mean() |> callPayoff strike

    /// <summary>
    /// Generates an asset price path using geometric Brownian motion.
    /// </summary>
    /// <param name="S0">The initial asset price.</param>
    /// <param name="r">The risk-free rate.</param>
    /// <param name="deltaT">The time step size.</param>
    /// <param name="sigma">The volatility.</param>
    /// <param name="normal">The normal distribution random source.</param>
    /// <param name="numSamples">The number of time steps.</param>
    /// <returns>A sequence representing the asset price path.</returns>
    let getAssetPath S0 r deltaT sigma (normal: Normal) numSamples =
        Seq.unfold
            (fun S ->
                let sNew =
                    (S
                     * exp (
                         ((r - (0.5 * sigma * sigma)) * deltaT)
                         + (sigma * sqrt (deltaT) * normal.Sample())
                     ))

                Some(sNew, sNew))
            S0
        |> Seq.take numSamples

    /// <summary>
    /// Prices an Asian arithmetic mean call option using Monte Carlo simulation.
    /// </summary>
    /// <param name="S0">The initial asset price.</param>
    /// <param name="strike">The strike price.</param>
    /// <param name="r">The risk-free rate.</param>
    /// <param name="T">The time to maturity.</param>
    /// <param name="sigma">The volatility.</param>
    /// <param name="numTrajectories">The number of Monte Carlo paths.</param>
    /// <param name="numSamples">The number of time steps per path.</param>
    /// <returns>A tuple of (option price, standard deviation of the estimate).</returns>
    let priceAsianArithmeticMeanMC S0 strike r T sigma numTrajectories numSamples =
        let normal = new Normal(0.0, 1.0)
        let deltaT = T / float numSamples

        let payoffs =
            seq {
                for n in 1..numTrajectories do
                    let assetPath = getAssetPath S0 r deltaT sigma normal numSamples |> Seq.toList
                    yield assetPath |> asianArithmeticMeanCallPayoff strike
            }

        let discountFactor = exp (-r * T)
        let priceMC = discountFactor * payoffs.Mean()

        let stddevMC =
            discountFactor * payoffs.StandardDeviation() / sqrt (float numTrajectories)

        priceMC, stddevMC

    /// <summary>
    /// Prices an Asian option using Monte Carlo simulation with antithetic variates.
    /// </summary>
    /// <param name="f1">The forward curve vector.</param>
    /// <param name="t1">The time vector.</param>
    /// <param name="v1">The volatility vector.</param>
    /// <param name="k">The strike price.</param>
    /// <param name="callput">The option type (Call or Put).</param>
    /// <param name="num">The number of Monte Carlo paths.</param>
    /// <returns>A tuple of (option price, standard deviation of the estimate).</returns>
    let asianMC (f1: Vector<float>) (t1: Vector<float>) (v1: Vector<float>) k callput num =
        let length = t1.Count

        let tdiff =
            match length with
            | 1 -> [| t1.[0] |] |> vector
            | _ ->
                let v = t1.[1 .. length - 1] - t1.[0 .. length - 2]
                Array.append [| t1.[0] |] (v.ToArray()) |> vector

        let tdiffsq = sqrt tdiff
        let f = f1 .* exp (-0.5 * v1 .* v1 .* t1)

        let getP (sample: Vector<float>) =
            let p =
                DenseVector.init t1.Count (fun i -> f.[i] * exp (v1.[i] * (tdiffsq.[0..i].DotProduct sample.[0..i])))

            p.Mean()

        let payoff =
            match callput with
            | Call -> callPayoff k
            | Put -> putPayoff k

        let o =
            [| 1 .. num / 2 |]
            |> Array.Parallel.map (fun i ->
                let n' = Normal(0.0, 1.0)
                n'.RandomSource <- System.Random(i * 10) // use separate seed per path
                let normal = DenseVector.random<float> (t1.Count) n'
                //use antithetic
                [| payoff (getP normal); payoff (getP -normal) |])
            |> Array.concat

        o.Mean(), o.StandardDeviation() / sqrt (float num)

    /// <summary>
    /// Prices a spread option using Monte Carlo simulation with antithetic variates.
    /// </summary>
    /// <param name="f1">The forward curve vector for asset 1.</param>
    /// <param name="fw1">The weighting vector for asset 1.</param>
    /// <param name="t1">The time vector for asset 1.</param>
    /// <param name="v1">The volatility vector for asset 1.</param>
    /// <param name="f2">The forward curve vector for asset 2.</param>
    /// <param name="fw2">The weighting vector for asset 2.</param>
    /// <param name="t2">The time vector for asset 2.</param>
    /// <param name="v2">The volatility vector for asset 2.</param>
    /// <param name="k">The strike price.</param>
    /// <param name="rho">The correlation between the two assets.</param>
    /// <param name="callput">The option type (Call or Put).</param>
    /// <param name="num">The number of Monte Carlo paths.</param>
    /// <returns>A tuple of (option price, standard deviation of the estimate).</returns>
    let spreadMC
        (f1: Vector<float>)
        (fw1: Vector<float>)
        (t1: Vector<float>)
        (v1: Vector<float>)
        (f2: Vector<float>)
        (fw2: Vector<float>)
        (t2: Vector<float>)
        v2
        k
        (rho: float)
        callput
        num
        =
        let cov = getCov t1 v1 t2 v2 rho |> fixCov
        let var = cov.Diagonal()
        let ch = cov.Cholesky().Factor
        let f = appendVector (f1 .* fw1) (-f2 .* fw2)
        let f' = f .* exp (-0.5 * var)

        let getP (sample: Vector<float>) =
            let p = f' .* exp (ch * sample)
            p.Sum()

        let payoff =
            match callput with
            | Call -> callPayoff k
            | Put -> putPayoff k

        let n' = Normal(0.0, 1.0)
        n'.RandomSource <- System.Random(SEED)

        let o =
            [| 1 .. num / 2 |]
            |> Seq.map (fun i ->
                let normal = DenseVector.random<float> (f.Count) n'
                //use antithetic
                [| payoff (getP normal); payoff (getP -normal) |])
            |> Array.concat

        o.Mean(), o.StandardDeviation() / sqrt (float num)
