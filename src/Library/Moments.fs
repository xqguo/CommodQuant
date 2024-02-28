namespace Commod

[<AutoOpen>]
module Moments =
    open System
    open MathNet.Numerics.Integration
    open MathNet.Numerics.LinearAlgebra

    //asian fwd price moment matching method
    let asianoption (f1: Vector<float>) (fw1: Vector<float>) t1 v1 k' o p1w =
        let (y1, y11, delta) = moments (f1 .* fw1) v1 t1
        let k = k' - p1w - delta
        let v = sqrt (log (y11 / y1 / y1))
        bs y1 k v 1. o

    //asian fwd price moment matching method
    let asianOptionAndDelta (f1: Vector<float>) (fw1: Vector<float>) t1 v1 k' o p1w =
        let (y1, y11, delta) = moments (f1 .* fw1) v1 t1
        let k = k' - p1w - delta
        let v = sqrt (log (y11 / y1 / y1))
        bs y1 k v 1. o, (bsdelta y1 k v 1. o) * fw1.Sum()

    ///spread option fwd pricing moment matching
    let rec spreadoption
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
        (p1: Vector<float>)
        (pw1: Vector<float>)
        (p2: Vector<float>)
        (pw2: Vector<float>)
        =
        let f1w = f1 .* fw1 //1st asset weighted
        let f2w = f2 .* fw2
        //#1st moments
        let x1 = f1w.Sum()
        let x2 = f2w.Sum()
        let k' = k - (p1 .* pw1).Sum() + (p2 .* pw2).Sum() // adapte K for past fixings

        if k' < 0. then
            let v0 = Vector<float>.Build.Dense(1)

            let callput' =
                match callput with
                | Call -> Put
                | Put -> Call

            spreadoption f2 fw2 t2 v2 f1 fw1 t1 v1 -k' rho callput' v0 v0 v0 v0 //put equivalent
        else
            //#2nd moments
            let (_, x11, _) = moments f1w v1 t1
            let (_, x22, _) = moments f2w v2 t2
            let x12 = momentsx f1w v1 t1 f2w v2 t2 rho
            //#intermediates
            let b1 = sqrt (log (x22 / x2 / x2))
            let b2 = 1. / b1 * log (x12 / (x1 * x2))

            let cp =
                match callput with
                | Call -> 1.0
                | Put -> -1.0

            let g = cp / sqrt (max (log (x11 / x1 / x1) - b2 * b2) 1E-12) //this can be nan?

            let i1 =
                Func<float, float>(fun x ->
                    normpdf (x)
                    * normcdf (
                        g
                        * log (sqrt (x11) * exp (b2 * x) / (k + x2 * x12 / (x1 * sqrt (x22)) * exp (b1 * x)))
                    ))

            let i2 =
                Func<float, float>(fun x ->
                    normpdf (x)
                    * normcdf (
                        g
                        * log (x1 * x12 / (x2 * sqrt (x11)) * exp (b2 * x) / (k + sqrt (x22) * exp (b1 * x)))
                    ))

            let i3 =
                Func<float, float>(fun x ->
                    normpdf (x)
                    * normcdf (
                        g
                        * log (x1 * x1 / sqrt (x11) * exp (b2 * x) / (k + x2 * x2 / sqrt (x22) * exp (b1 * x)))
                    ))

            let stddevs = 6.0
            let partitions = 100000
            let i1' = SimpsonRule.IntegrateComposite(i1, -stddevs, stddevs, partitions)
            let i2' = SimpsonRule.IntegrateComposite(i2, -stddevs, stddevs, partitions)
            let i3' = SimpsonRule.IntegrateComposite(i3, -stddevs, stddevs, partitions)
            cp * (x1 * i1' - x2 * i2' - k * i3')
