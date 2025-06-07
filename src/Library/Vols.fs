namespace Commod

[<AutoOpen>]
module Vols =
    open MathNet.Numerics.LinearAlgebra
    /// <summary>
    /// Constructs a time matrix for VCV using min(T1, T2) for each element.
    /// </summary>
    /// <param name="T1">The first vector of times.</param>
    /// <param name="T2">The second vector of times.</param>
    /// <returns>A matrix where each element is min(T1[i], T2[j]).</returns>
    let getTmatrix (T1: Vector<float>) (T2: Vector<float>) =
        let onesT1 = Vector.Build.Dense(T1.Count, 1.)
        let onesT2 = Vector.Build.Dense(T2.Count, 1.)
        let tmatrix2 = onesT1.OuterProduct T2
        let tmatrix1 = T1.OuterProduct onesT2
        tmatrix1.PointwiseMinimum tmatrix2

    /// <summary>
    /// Computes the first and second moments for a set of forwards and volatilities.
    /// </summary>
    /// <param name="f">The vector of forwards.</param>
    /// <param name="V1">The vector of volatilities.</param>
    /// <param name="T1">The vector of times.</param>
    /// <returns>A tuple (mean, second moment, 0.0).</returns>
    let moments (f: Vector<float>) (V1: Vector<float>) (T1: Vector<float>) =
        let x1 = f.Sum()
        let tmatrix = getTmatrix T1 T1
        let vv = ((V1.OuterProduct V1) .* tmatrix).PointwiseExp() //assuming constant vol per forward, used for com, more generally could sum piece-wise
        let x11 = ((f.OuterProduct f) .* vv) |> Matrix.sum
        (x1, x11, 0.)

    /// <summary>
    /// Computes the cross moments matrix for two sets of forwards, volatilities, times, and correlation.
    /// </summary>
    /// <param name="f1w">The first vector of forwards.</param>
    /// <param name="v1">The first vector of volatilities.</param>
    /// <param name="t1">The first vector of times.</param>
    /// <param name="f2w">The second vector of forwards.</param>
    /// <param name="v2">The second vector of volatilities.</param>
    /// <param name="t2">The second vector of times.</param>
    /// <param name="rho">The correlation coefficient.</param>
    /// <returns>The cross moments matrix as a float.</returns>
    let momentsx
        (f1w: Vector<float>)
        (v1: Vector<float>)
        (t1: Vector<float>)
        (f2w: Vector<float>)
        (v2: Vector<float>)
        (t2: Vector<float>)
        rho
        =
        let ff = f1w.OuterProduct f2w

        if (rho = 0.) then
            ff //all cross terms are 0.
        else
            let tmatrix = getTmatrix t1 t2
            (ff .* ((v1.OuterProduct v2) .* tmatrix * rho).PointwiseExp())
        |> Matrix.sum

    /// <summary>
    /// Computes the third cross moment from forwards and a second moment matrix.
    /// </summary>
    /// <param name="f">The vector of forwards.</param>
    /// <param name="v">The second moment matrix.</param>
    /// <returns>A tuple (y1, y11, x) representing the third moment decomposition.</returns>
    let momentsx3 (f: Vector<float>) (v: Matrix<float>) =
        let n = f.Count - 1
        let m1 = f.Sum()
        let m2 = v |> Matrix.sum

        let m3 =
            seq {
                for i in 0..n do
                    for j in 0..i do
                        for k in 0..j do
                            let f3 = f.[i] * f.[j] * f.[k]
                            let v3 = v.[i, j] * v.[j, k] * v.[k, i]

                            if i = j && j = k then yield (v3 / f3)
                            elif i = j || j = k then yield (v3 / f3) * 3.0
                            else yield (v3 / f3) * 6.0
            }
            |> Seq.sum
        //let m3 =
        //    [|
        //        for i in 0 .. n do
        //        for j in 0 .. n do
        //        for k in 0 .. n do
        //            let f3 = f.[i] * f.[j] * f.[k]
        //            let v3 = v.[i,j] * v.[j,k] * v.[k,i]
        //            yield (v3 / f3)
        //    |] |> Array.sum
        let u1 = m1
        let u2 = m2 - u1 * u1
        let u3 = m3 - 3.0 * u1 * u2 - u1 * u1 * u1
        let z = ((u3 + sqrt (u3 * u3 + 4.0 * (pown u2 3))) / 2.0) ** (1.0 / 3.0)
        let y1 = u2 / (z - u2 / z)
        let y11 = u2 + y1 * y1
        let x = u1 - y1
        (y1, y11, x)

    /// <summary>
    /// Computes the covariance matrix Sigma2 = rho * v_k * v_j * min(t_k, t_j).
    /// </summary>
    /// <param name="v1">The first vector of volatilities.</param>
    /// <param name="t1">The first vector of times.</param>
    /// <param name="v2">The second vector of volatilities.</param>
    /// <param name="t2">The second vector of times.</param>
    /// <param name="rho">The correlation coefficient.</param>
    /// <returns>The covariance matrix.</returns>
    let getSigma2 (v1: Vector<float>) (t1: Vector<float>) (v2: Vector<float>) (t2: Vector<float>) rho =
        let tmatrix = getTmatrix t1 t2
        (v1.OuterProduct v2) .* tmatrix * rho

    /// <summary>
    /// Gets the covariance matrix for two assets with constant correlation.
    /// </summary>
    /// <param name="t1">The first vector of times.</param>
    /// <param name="v1">The first vector of volatilities.</param>
    /// <param name="t2">The second vector of times.</param>
    /// <param name="v2">The second vector of volatilities.</param>
    /// <param name="rho">The correlation coefficient.</param>
    /// <returns>The combined covariance matrix.</returns>
    let getCov (t1: Vector<float>) (v1: Vector<float>) (t2: Vector<float>) (v2: Vector<float>) (rho: float) =
        //assuming constant correlation between 2 assets.
        let sigma12 = getSigma2 v1 t1 v2 t2 rho
        let sigma11 = getSigma2 v1 t1 v1 t1 1.0
        let sigma22 = getSigma2 v2 t2 v2 t2 1.0
        sigma11.Append(sigma12).Stack((sigma12.Transpose().Append(sigma22)))

    /// <summary>
    /// Gets the covariance matrix for two assets with constant correlation, using provided covariance matrices.
    /// </summary>
    /// <param name="t1">The first vector of times.</param>
    /// <param name="sigma11">The covariance matrix for asset 1.</param>
    /// <param name="t2">The second vector of times.</param>
    /// <param name="sigma22">The covariance matrix for asset 2.</param>
    /// <param name="rho">The correlation coefficient.</param>
    /// <returns>The combined covariance matrix.</returns>
    let getCov2 (t1: Vector<float>) (sigma11: Matrix<float>) (t2: Vector<float>) (sigma22: Matrix<float>) (rho: float) =
        //assuming constant correlation between 2 assets.
        let v1 = (sigma11.Diagonal() ./ t1) |> Vector.Sqrt
        let v2 = (sigma22.Diagonal() ./ t2) |> Vector.Sqrt
        let sigma12 = getSigma2 v1 t1 v2 t2 rho
        sigma11.Append(sigma12).Stack((sigma12.Transpose().Append(sigma22)))
