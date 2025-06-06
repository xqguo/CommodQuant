namespace Commod

[<AutoOpen>]
module Gabillon =
    open System
    open MathNet.Numerics
    open MathNet.Numerics.LinearAlgebra
    open MathNet.Numerics.Optimization

    /// <summary>
    /// Checks for eigenvalues and fixes the covariance matrix to be positive definite.
    /// </summary>
    /// <param name="cov">The covariance matrix.</param>
    /// <returns>The positive definite covariance matrix.</returns>
    let fixCov (cov: Matrix<float>) =
        try
            let _ = cov.Cholesky()
            cov
        with _ ->
            let evd = cov.Evd(Symmetricity.Symmetric)
            let l = evd.EigenValues |> Vector.map (fun x -> x.Real)
            let o = evd.EigenVectors
            let l' = l |> Vector.map (fun x -> max x 1E-10)
            // o * (DenseMatrix.ofDiag l') * (o.Inverse())
            o * (DenseMatrix.ofDiag l') * (o.Transpose())

    /// <summary>
    /// Fixes the correlation matrix to be positive definite.
    /// </summary>
    /// <param name="corr">The correlation matrix.</param>
    /// <returns>The positive definite correlation matrix.</returns>
    let fixCorr (corr: Matrix<float>) =
        let cov = fixCov corr
        let vol = cov.Diagonal().PointwiseAbs().PointwiseSqrt()

        cov ./ (vol.OuterProduct vol)
        |> Matrix.mapi (fun i j v -> if i = j then 1.0 else v)

    /// <summary>
    /// Computes the forward variance between tm and tn for a future with maturity Ti,
    /// using constant Gabillon model inputs for this interval.
    /// </summary>
    /// <param name="tm">Start time of the interval.</param>
    /// <param name="tn">End time of the interval.</param>
    /// <param name="Ti">Maturity of the future.</param>
    /// <param name="sigmas">Short-term volatility.</param>
    /// <param name="sigmal">Long-term volatility.</param>
    /// <param name="k">Mean reversion speed.</param>
    /// <param name="rho">Correlation between short-term and long-term factors.</param>
    /// <returns>The forward variance.</returns>
    let fwdVariance tm tn Ti sigmas sigmal k rho =
        let Tn = Ti - tn
        let Tm = Ti - tm
        let en = exp (-k * Tn)
        let e2n = pown en 2
        let em = exp (-k * Tm)
        let e2m = pown em 2
        let enm = en - em
        let e2nm = e2n - e2m
        let s2 = pown sigmas 2
        let l2 = pown sigmal 2
        let p1 = s2 / (2.0 * k) * e2nm
        let p2 = l2 * (tn - tm - 2.0 / k * enm + e2nm / (2.0 * k))
        let p3 = rho * sigmas * sigmal * (2.0 / k * enm - e2nm / k)
        p1 + p2 + p3

    //(fwdVariance 0.0 1.0 1.0 0.551 0.2 1.0 0.5 )/1.0|> sqrt
    //(fwdVariance 0.0 0.9 1.0 0.551 0.2 1.0 0.5 )/0.9|> sqrt

    /// <summary>
    /// Computes the forward covariance between tm and tn for futures with maturity Ti and Tj,
    /// using constant Gabillon model inputs for this interval and allowing different sigmas for i and j.
    /// </summary>
    /// <param name="tm">Start time of the interval.</param>
    /// <param name="tn">End time of the interval.</param>
    /// <param name="Ti">Maturity of the first future.</param>
    /// <param name="Tj">Maturity of the second future.</param>
    /// <param name="sigmasi">Short-term volatility of the first future.</param>
    /// <param name="sigmasj">Short-term volatility of the second future.</param>
    /// <param name="sigmal">Long-term volatility.</param>
    /// <param name="k">Mean reversion speed.</param>
    /// <param name="rho">Correlation between short-term and long-term factors.</param>
    /// <returns>The forward covariance.</returns>
    let fwdCovariance tm tn Ti Tj sigmasi sigmasj sigmal k rho =
        let Tin = Ti - tn
        let Tim = Ti - tm
        let Tjn = Tj - tn
        let Tjm = Tj - tm
        let TTn = Ti + Tj - 2.0 * tn
        let TTm = Ti + Tj - 2.0 * tm
        let ein = exp (-k * Tin)
        let ejn = exp (-k * Tjn)
        let e2n = exp (-k * TTn)
        let eim = exp (-k * Tim)
        let ejm = exp (-k * Tjm)
        let e2m = exp (-k * TTm)
        let einm = ein - eim
        let ejnm = ejn - ejm
        let e2nm = e2n - e2m
        let s2 = sigmasi * sigmasj
        let l2 = pown sigmal 2
        let p1 = s2 / (2.0 * k) * e2nm
        let p2 = l2 * (tn - tm - 1.0 / k * (einm + ejnm) + e2nm / (2.0 * k))
        let p3 = rho * sigmasi * sigmal * (1.0 / k * einm - e2nm / (2.0 * k))
        let p4 = rho * sigmasj * sigmal * (1.0 / k * ejnm - e2nm / (2.0 * k))
        p1 + p2 + p3 + p4

    /// <summary>
    /// Computes the forward cross-covariance between tm and tn for two futures (1 and 2) with maturities Ti and Tj,
    /// using constant Gabillon model inputs for this interval and allowing different sigmas for i and j.
    /// </summary>
    /// <param name="tm">Start time of the interval.</param>
    /// <param name="tn">End time of the interval.</param>
    /// <param name="Ti">Maturity of the first future.</param>
    /// <param name="Tj">Maturity of the second future.</param>
    /// <param name="sigmas1">Short-term volatility of the first future.</param>
    /// <param name="sigmas2">Short-term volatility of the second future.</param>
    /// <param name="sigmal1">Long-term volatility of the first future.</param>
    /// <param name="sigmal2">Long-term volatility of the second future.</param>
    /// <param name="k1">Mean reversion speed of the first future.</param>
    /// <param name="k2">Mean reversion speed of the second future.</param>
    /// <param name="rho11">Correlation between short-term factors of the two futures.</param>
    /// <param name="rho12">Correlation between short-term factor of future 1 and long-term factor of future 2.</param>
    /// <param name="rho21">Correlation between long-term factor of future 1 and short-term factor of future 2.</param>
    /// <param name="rho22">Correlation between long-term factors of the two futures.</param>
    /// <returns>The forward cross-covariance.</returns>
    let fwdXCovariance (tm: float) tn Ti Tj sigmas1 sigmas2 sigmal1 sigmal2 k1 k2 rho11 rho12 rho21 rho22 =
        let k12 = k1 + k2
        let Tin = Ti - tn
        let Tim = Ti - tm
        let Tjn = Tj - tn
        let Tjm = Tj - tm
        let ein = exp (-k1 * Tin)
        let ejn = exp (-k2 * Tjn)
        let e2n = exp (-k1 * Tin - k2 * Tjn)
        let eim = exp (-k1 * Tim)
        let ejm = exp (-k2 * Tjm)
        let e2m = exp (-k1 * Tim - k2 * Tjm)
        let einmk = (ein - eim) / k1
        let ejnmk = (ejn - ejm) / k2
        let e2nmk = (e2n - e2m) / k12
        let ss = sigmas1 * sigmas2 * rho11
        let ll = sigmal1 * sigmal2 * rho22
        let sl = sigmas1 * sigmal2 * rho12
        let ls = sigmal1 * sigmas2 * rho21
        let p1 = ss * e2nmk
        let p2 = ll * (tn - tm - einmk - ejnmk + e2nmk)
        let p3 = sl * (einmk - e2nmk)
        let p4 = ls * (ejnmk - e2nmk)
        p1 + p2 + p3 + p4

    /// <summary>
    /// Computes the forward correlation between tm and tn for futures with maturity Ti and Tj.
    /// </summary>
    /// <param name="tm">Start time of the interval.</param>
    /// <param name="tn">End time of the interval.</param>
    /// <param name="Ti">Maturity of the first future.</param>
    /// <param name="Tj">Maturity of the second future.</param>
    /// <param name="sigmasi">Short-term volatility of the first future.</param>
    /// <param name="sigmasj">Short-term volatility of the second future.</param>
    /// <param name="sigmal">Long-term volatility.</param>
    /// <param name="k">Mean reversion speed.</param>
    /// <param name="rho">Correlation between short-term and long-term factors.</param>
    /// <returns>The forward correlation.</returns>
    let fwdCorr tm tn Ti Tj sigmasi sigmasj sigmal k rho =
        let Vi = fwdVariance tm tn Ti sigmasi sigmal k rho
        let Vj = fwdVariance tm tn Tj sigmasj sigmal k rho
        let cov = fwdCovariance tm tn Ti Tj sigmasi sigmasj sigmal k rho
        cov / sqrt (Vi * Vj)

    /// <summary>
    /// Computes the forward volatility between tm and tn for a future with maturity Ti.
    /// </summary>
    /// <param name="tm">Start time of the interval.</param>
    /// <param name="tn">End time of the interval.</param>
    /// <param name="Ti">Maturity of the future.</param>
    /// <param name="sigmas">Short-term volatility.</param>
    /// <param name="sigmal">Long-term volatility.</param>
    /// <param name="k">Mean reversion speed.</param>
    /// <param name="rho">Correlation between short-term and long-term factors.</param>
    /// <returns>The forward volatility.</returns>
    let fwdVol tm tn Ti sigmas sigmal k rho =
        let vvt = fwdVariance tm tn Ti sigmas sigmal k rho
        sqrt (vvt / (tn - tm))

    /// <summary>
    /// Computes the derivative of forward variance with respect to sigmas.
    /// </summary>
    /// <param name="tm">Start time of the interval.</param>
    /// <param name="tn">End time of the interval.</param>
    /// <param name="Ti">Maturity of the future.</param>
    /// <param name="sigmas">Short-term volatility.</param>
    /// <param name="sigmal">Long-term volatility.</param>
    /// <param name="k">Mean reversion speed.</param>
    /// <param name="rho">Correlation between short-term and long-term factors.</param>
    /// <returns>The derivative of forward variance with respect to sigmas.</returns>
    let dfwdVarianceds tm tn Ti sigmas sigmal k rho =
        let Tn = Ti - tn
        let Tm = Ti - tm
        let en = exp (-k * Tn)
        let e2n = pown en 2
        let em = exp (-k * Tm)
        let e2m = pown em 2
        let enm = en - em
        let e2nm = e2n - e2m
        let s2 = sigmas * 2.0
        let p1 = s2 / (2.0 * k) * e2nm
        let p3 = rho * sigmal * (2.0 / k * enm - e2nm / k)
        p1 + p3

    //dfwdVarianceds 0.0 1.0 1.0 0.4 0.2 1.0 0.5

    /// <summary>
    /// Finds sigmaS using numerical root finding.
    /// </summary>
    /// <param name="vm">Volatility at time tm.</param>
    /// <param name="vn">Volatility at time tn.</param>
    /// <param name="tm">Start time of the interval.</param>
    /// <param name="tn">End time of the interval.</param>
    /// <param name="Ti">Maturity of the future.</param>
    /// <param name="sigmal">Long-term volatility.</param>
    /// <param name="k">Mean reversion speed.</param>
    /// <param name="rho">Correlation between short-term and long-term factors.</param>
    /// <returns>The implied sigmaS.</returns>
    let findSigmaS vm vn tm tn Ti sigmal k rho =
        let vvt = vn * vn * tn - vm * vm * tm

        RootFinding.RobustNewtonRaphson.FindRoot(
            (fun x -> vvt - fwdVariance tm tn Ti x sigmal k rho),
            (fun x -> dfwdVarianceds tm tn Ti x sigmal k rho),
            0.001,
            vm * 1E3
        )

    //findSigmaS 0.4 0.4 0.0 1.0 1.0 0.2 1.0 0.5

    /// <summary>
    /// Analytically computes sigmaS, as it is just a quadratic equation.
    /// </summary>
    /// <param name="vm">Volatility at time tm.</param>
    /// <param name="vn">Volatility at time tn.</param>
    /// <param name="tm">Start time of the interval.</param>
    /// <param name="tn">End time of the interval.</param>
    /// <param name="Ti">Maturity of the future.</param>
    /// <param name="sigmal">Long-term volatility.</param>
    /// <param name="k">Mean reversion speed.</param>
    /// <param name="rho">Correlation between short-term and long-term factors.</param>
    /// <returns>The implied sigmaS.</returns>
    let implySigmaS vm vn tm tn Ti sigmal k rho =
        let vvt = vn * vn * tn - vm * vm * tm
        let Tn = Ti - tn
        let Tm = Ti - tm
        let en = exp (-k * Tn)
        let e2n = pown en 2
        let em = exp (-k * Tm)
        let e2m = pown em 2
        let enm = en - em
        let e2nm = e2n - e2m
        let l2 = pown sigmal 2
        let a = 1.0 / (2.0 * k) * e2nm
        let c = l2 * (tn - tm - 2.0 / k * enm + e2nm / (2.0 * k)) - vvt
        let b = rho * sigmal * (2.0 / k * enm - e2nm / k)
        let s = b * b - 4.0 * a * c
        if s < 0. then 0.001 else (-b + sqrt s) / (2.0 * a) //take the bigger root

    //implySigmaS 0.4 0.4 0.0 1.0 1.0 0.2 1.0 0.5

    /// <summary>
    /// Computes the gradient function for optimization.
    /// </summary>
    /// <param name="sigmas">Short-term volatility.</param>
    /// <param name="sigmal">Long-term volatility.</param>
    /// <param name="k">Mean reversion speed.</param>
    /// <param name="rho">Correlation between short-term and long-term factors.</param>
    /// <param name="ins">The instrument.</param>
    /// <param name="pd">The pricing date.</param>
    /// <returns>A tuple containing the target function value and its gradient.</returns>
    let gradientfunc sigmas sigmal k rho (ins: Instrument) pd =
        let c = getCommod ins
        let vols = getVols ins

        let targetfunc sigmas sigmal k rho =
            vols.Pillars
            |> Set.filter (fun p -> c.Contracts.[p] > pd)
            |> Set.fold
                (fun acc p ->
                    let t = (c.Contracts.[p] - pd).TotalDays / 365.
                    let v = vols.[p] |> float
                    let v' = fwdVol 0.0 t t sigmas sigmal k rho
                    (pown (v' - v) 2) + acc)
                0.

        let eps = 1e-6
        let r0 = targetfunc (sigmas + eps) sigmal k rho
        let r1 = targetfunc sigmas (sigmal + eps) k rho
        let r2 = targetfunc sigmas sigmal (k + eps) rho
        let r3 = targetfunc sigmas sigmal k (rho + eps)
        let r = targetfunc sigmas sigmal k rho
        let df x = (x - r) / eps
        let d' = [ r0; r1; r2; r3 ] |> List.map df |> vector
        struct (r, d')

    /// <summary>
    /// Finds the best fit parameters for the Gabillon model.
    /// </summary>
    /// <param name="ins">The instrument.</param>
    /// <param name="pd">The pricing date.</param>
    /// <returns>The result of the optimization, containing the best fit parameters.</returns>
    let bestfit ins pd =
        let solver = Optimization.BfgsBMinimizer(1e-5, 1e-5, 1e-5, 1000)
        //let solver = Optimization.ConjugateGradientMinimizer(1e-5, 1000)
        let ig = DenseVector.ofList [ 0.5; 0.25; 1.0; 0.5 ] //sigmas sigmal k rho
        let u = DenseVector.ofList [ 1.5; 0.5; 10.0; 0.9 ] //sigmas sigmal k rho
        let l = DenseVector.ofList [ 0.01; 0.01; 0.01; -0.5 ] //sigmas sigmal k rho

        let o =
            ObjectiveFunction.Gradient(fun (iv: Vector<float>) -> gradientfunc iv.[0] iv.[1] iv.[2] iv.[3] ins pd)

        solver.FindMinimum(o, l, u, ig)
    //solver.FindMinimum( o, ig)

    /// <summary>
    /// Gets the covariance matrix for a Gabillon model.
    /// </summary>
    /// <param name="ins">The instrument.</param>
    /// <param name="vols">The volatility curve.</param>
    /// <param name="sl">Long-term volatility.</param>
    /// <param name="k">Mean reversion speed.</param>
    /// <param name="rho">Correlation between short-term and long-term factors.</param>
    /// <param name="fixings">Array of fixing dates and contract names.</param>
    /// <param name="pd">The pricing date.</param>
    /// <returns>The covariance matrix.</returns>
    let getGabillonCov ins (vols: VolCurve) (sl, k, rho) (fixings: (DateTime * string)[]) pd =
        let c = getCommod ins
        let optd = c.Contracts.Opt |> Map.map (fun _ d -> getTTM pd d)
        let futd = c.Contracts.Fut |> Map.map (fun _ d -> getTTM pd d)
        let (ds, cs) = fixings |> Array.unzip
        let ts = ds |> Array.map (getTTM pd)

        //calibrate ss
        let ss =
            cs
            |> set
            |> Set.map (fun c ->
                let t = optd.[c]
                let T = futd.[c]
                let v = vols.[c] |> float
                (c, implySigmaS v v 0. t T sl k rho))
            |> Map.ofSeq

        //compute cov
        let n = fixings.Length

        let lower =
            Array2D.init n n (fun i j ->
                if i <= j then
                    let t = min ts.[i] ts.[j]
                    let ci = cs.[i]
                    let cj = cs.[j]
                    fwdCovariance 0.0 t futd.[ci] futd.[cj] ss.[ci] ss.[cj] sl k rho
                else
                    0.0)

        DenseMatrix.init n n (fun i j -> if i <= j then lower.[i, j] else lower.[j, i])

    /// <summary>
    /// Gets the cross-covariance matrix for two Gabillon models.
    /// </summary>
    /// <param name="ins">The first instrument.</param>
    /// <param name="vols">The volatility curve for the first instrument.</param>
    /// <param name="fixings">Array of fixing dates and contract names for the first instrument.</param>
    /// <param name="ins'">The second instrument.</param>
    /// <param name="vols'">The volatility curve for the second instrument.</param>
    /// <param name="fixings'">Array of fixing dates and contract names for the second instrument.</param>
    /// <param name="l1">Long-term volatility of the first instrument.</param>
    /// <param name="l2">Long-term volatility of the second instrument.</param>
    /// <param name="k1">Mean reversion speed of the first instrument.</param>
    /// <param name="k2">Mean reversion speed of the second instrument.</param>
    /// <param name="rho1">Correlation for the first instrument.</param>
    /// <param name="rho2">Correlation for the second instrument.</param>
    /// <param name="rho11">Correlation between short-term factors of the two instruments.</param>
    /// <param name="rho12">Correlation between short-term factor of instrument 1 and long-term factor of instrument 2.</param>
    /// <param name="rho21">Correlation between long-term factor of instrument 1 and short-term factor of instrument 2.</param>
    /// <param name="rho22">Correlation between long-term factors of the two instruments.</param>
    /// <param name="pd">The pricing date.</param>
    /// <returns>The cross-covariance matrix.</returns>
    let getXGabillonCov
        ins
        (vols: VolCurve)
        (fixings: (DateTime * string)[])
        ins'
        (vols': VolCurve)
        (fixings': (DateTime * string)[])
        (l1, l2, k1, k2, rho1, rho2, rho11, rho12, rho21, rho22)
        pd
        =
        let c = getCommod ins
        let optd = c.Contracts.Opt |> Map.map (fun _ d -> getTTM pd d)
        let futd = c.Contracts.Fut |> Map.map (fun _ d -> getTTM pd d)
        let (ds, cs) = fixings |> Array.unzip
        let ts = ds |> Array.map (getTTM pd)

        //calibrate ss
        let ss =
            cs
            |> set
            |> Set.map (fun c ->
                let t = optd.[c]
                let T = futd.[c]
                let v = vols.[c] |> float
                (c, implySigmaS v v 0. t T l1 k1 rho1))
            |> Map.ofSeq

        let c' = getCommod ins'
        let optd' = c'.Contracts.Opt |> Map.map (fun _ d -> getTTM pd d)
        let futd' = c'.Contracts.Fut |> Map.map (fun _ d -> getTTM pd d)
        let (ds', cs') = fixings' |> Array.unzip
        let ts' = ds' |> Array.map (getTTM pd)

        //calibrate ss
        let ss' =
            cs'
            |> set
            |> Set.map (fun c ->
                let t = optd'.[c]
                let T = futd'.[c]
                let v = vols'.[c] |> float
                (c, implySigmaS v v 0. t T l2 k2 rho2))
            |> Map.ofSeq

        //compute cov, this one is not symmetric in general
        let n = fixings.Length

        DenseMatrix.init n n (fun i j ->
            let t = min ts.[i] ts'.[j]
            let ci = cs.[i]
            let cj = cs'.[j]
            fwdXCovariance 0.0 t futd.[ci] futd'.[cj] ss.[ci] ss'.[cj] l1 l2 k1 k2 rho11 rho12 rho21 rho22)

    /// <summary>
    /// Gets the full cross-covariance matrix for two Gabillon models.
    /// </summary>
    /// <param name="ins">The first instrument.</param>
    /// <param name="vols">The volatility curve for the first instrument.</param>
    /// <param name="fixings">Array of fixing dates and contract names for the first instrument.</param>
    /// <param name="ins'">The second instrument.</param>
    /// <param name="vols'">The volatility curve for the second instrument.</param>
    /// <param name="fixings'">Array of fixing dates and contract names for the second instrument.</param>
    /// <param name="l1">Long-term volatility of the first instrument.</param>
    /// <param name="l2">Long-term volatility of the second instrument.</param>
    /// <param name="k1">Mean reversion speed of the first instrument.</param>
    /// <param name="k2">Mean reversion speed of the second instrument.</param>
    /// <param name="rho1">Correlation for the first instrument.</param>
    /// <param name="rho2">Correlation for the second instrument.</param>
    /// <param name="rho11">Correlation between short-term factors of the two instruments.</param>
    /// <param name="rho12">Correlation between short-term factor of instrument 1 and long-term factor of instrument 2.</param>
    /// <param name="rho21">Correlation between long-term factor of instrument 1 and short-term factor of instrument 2.</param>
    /// <param name="rho22">Correlation between long-term factors of the two instruments.</param>
    /// <param name="pd">The pricing date.</param>
    /// <returns>The full cross-covariance matrix.</returns>
    let getXGabillonCovFull
        ins
        (vols: VolCurve)
        (fixings: (DateTime * string)[])
        ins'
        (vols': VolCurve)
        (fixings': (DateTime * string)[])
        (l1, l2, k1, k2, rho1, rho2, rho11, rho12, rho21, rho22)
        pd
        =
        let c = getCommod ins
        let optd = c.Contracts.Opt |> Map.map (fun _ d -> getTTM pd d)
        let futd = c.Contracts.Fut |> Map.map (fun _ d -> getTTM pd d)
        let (ds, cs) = fixings |> Array.unzip
        let ts = ds |> Array.map (getTTM pd)

        //calibrate ss
        let ss =
            cs
            |> set
            |> Set.map (fun c ->
                let t = optd.[c]
                let T = futd.[c]
                let v = vols.[c] |> float
                (c, implySigmaS v v 0. t T l1 k1 rho1))
            |> Map.ofSeq

        let c' = getCommod ins'
        let optd' = c'.Contracts.Opt |> Map.map (fun _ d -> getTTM pd d)
        let futd' = c'.Contracts.Fut |> Map.map (fun _ d -> getTTM pd d)
        let (ds', cs') = fixings' |> Array.unzip
        let ts' = ds' |> Array.map (getTTM pd)

        //calibrate ss
        let ss' =
            cs'
            |> set
            |> Set.map (fun c ->
                let t = optd'.[c]
                let T = futd'.[c]
                let v = vols'.[c] |> float
                (c, implySigmaS v v 0. t T l2 k2 rho2))
            |> Map.ofSeq

        //compute cov, this one is not symmetric in general
        let n = fixings.Length
        let m = fixings'.Length

        let sigma12 =
            DenseMatrix.init n m (fun i j ->
                let t = min ts.[i] ts'.[j]
                let ci = cs.[i]
                let cj = cs'.[j]
                fwdXCovariance 0.0 t futd.[ci] futd'.[cj] ss.[ci] ss'.[cj] l1 l2 k1 k2 rho11 rho12 rho21 rho22)

        //compute sigma11
        let lower =
            Array2D.init n n (fun i j ->
                if i <= j then
                    let t = min ts.[i] ts.[j]
                    let ci = cs.[i]
                    let cj = cs.[j]
                    fwdCovariance 0.0 t futd.[ci] futd.[cj] ss.[ci] ss.[cj] l1 k1 rho1
                else
                    0.0)

        let sigma11 =
            DenseMatrix.init n n (fun i j -> if i <= j then lower.[i, j] else lower.[j, i])

        //compute sigma22
        let lower' =
            Array2D.init m m (fun i j ->
                if i <= j then
                    let t = min ts'.[i] ts'.[j]
                    let ci = cs'.[i]
                    let cj = cs'.[j]
                    fwdCovariance 0.0 t futd'.[ci] futd'.[cj] ss'.[ci] ss'.[cj] l2 k2 rho2
                else
                    0.0)

        let sigma22 =
            DenseMatrix.init m m (fun i j -> if i <= j then lower'.[i, j] else lower'.[j, i])

        let cov = sigma11.Append(sigma12).Stack((sigma12.Transpose().Append(sigma22)))


        fixCov cov

    /// <summary>
    /// Gets the default Gabillon parameters for a given instrument.
    /// These are hardcoded defaults and could be read from files in the future.
    /// </summary>
    /// <param name="ins">The instrument.</param>
    /// <returns>A tuple containing (sigmal, k, rho).</returns>
    let getGabillonParam ins =
        match ins with
        | TTF
        | NBP
        | JKM -> (0.22, 0.5, 0.32)
        | NG -> (0.17, 0.48, 0.)
        | BRT
        | DBRT -> (0.12, 0.5, 0.7)
        | _ -> (0.001, 0.001, 0.0) //bs equivalent

    /// <summary>
    /// Combines Gabillon parameters for two instruments.
    /// </summary>
    /// <param name="l1">Long-term volatility of the first instrument.</param>
    /// <param name="k1">Mean reversion speed of the first instrument.</param>
    /// <param name="r1">Correlation for the first instrument.</param>
    /// <param name="l2">Long-term volatility of the second instrument.</param>
    /// <param name="k2">Mean reversion speed of the second instrument.</param>
    /// <param name="r2">Correlation for the second instrument.</param>
    /// <param name="rho">List of correlation parameters (1 or 4 elements).</param>
    /// <returns>A tuple of combined parameters.</returns>
    let combineGabillonParam (l1, k1, r1) (l2, k2, r2) rho =
        match rho with
        | [ r ] -> (l1, l2, k1, k2, r1, r2, r, r, r, r)
        | [ r11; r12; r21; r22 ] -> (l1, l2, k1, k2, r1, r2, r11, r12, r21, r22)
        | _ -> failwith "Rho should be a list of float of either 1 or 4 elements"

    /// <summary>
    /// Gets the combined Gabillon parameters for two instruments.
    /// </summary>
    /// <param name="ins1">The first instrument.</param>
    /// <param name="ins2">The second instrument.</param>
    /// <param name="rho">List of correlation parameters (1 or 4 elements).</param>
    /// <returns>A tuple of combined parameters.</returns>
    let getXGabillonParam ins1 ins2 rho =
        let p1 = getGabillonParam ins1
        let p2 = getGabillonParam ins2
        combineGabillonParam p1 p2 rho

    /// <summary>
    /// Gets the implied volatility curve for a Gabillon model.
    /// </summary>
    /// <param name="ins">The instrument.</param>
    /// <param name="sigmas">Short-term volatility.</param>
    /// <param name="sigmal">Long-term volatility.</param>
    /// <param name="k">Mean reversion speed.</param>
    /// <param name="rho">Correlation between short-term and long-term factors.</param>
    /// <param name="pd">The pricing date.</param>
    /// <returns>The implied volatility curve.</returns>
    let getGabillonImpliedVol ins sigmas sigmal k rho pd =
        let c = getCommod ins

        c.Contracts.Opt
        |> Map.filter (fun pillar v -> pd < v)
        |> Map.map (fun pillar v ->
            let t = getTTM pd v
            let T = c.Contracts.Fut.[pillar] |> getTTM pd
            fwdVol 0 t T sigmas sigmal k rho |> decimal |> AbsoluteVol)
        |> Map.toArray
        |> Array.sortBy (fst >> pillarToDate)
        |> Map.ofArray
        |> VolCurve
