namespace Commod

[<AutoOpen>]
module Options =
    open MathNet.Numerics.Differentiation

    ///returns black scholes fwd price
    let bs f k v t (o: Payoff) =
        if (f <= 0. || k <= 0. || v <= 0. || t <= 0.) then
            let iv =
                match o with
                | Call -> f - k
                | Put -> k - f

            (max iv 0.)
        else
            let d1 = (log (f / k) + 0.5 * v * v * t) / (v * sqrt (t))
            let d2 = d1 - v * sqrt (t)

            match o with
            | Call -> f * normcdf (d1) - k * normcdf (d2)
            | Put -> k * normcdf (-d2) - f * normcdf (-d1)

    let kirk f1 f2 k v1 v2 rho t o =
        let v =
            sqrt (v1 * v1 + (v2 * f2 / (f2 + k)) ** 2.0 - 2.0 * rho * v1 * v2 * f2 / (f2 + k))

        bs f1 (f2 + k) v t o

    ///bs fwd delta
    let bsdelta f k v t (o: Payoff) =
        if (f <= 0. || k <= 0. || v <= 0. || t <= 0.) then
            match o with
            | Call -> if f >= k then 1.0 else 0.0
            | Put -> if f <= k then 1.0 else 0.
        else
            let d1 = (log (f / k) + 0.5 * v * v * t) / (v * sqrt (t))
            //let d2 = d1 - v*sqrt(t)
            match o with
            | Call -> normcdf (d1)
            | Put -> - normcdf(-d1)

    ///bs delta to strike
    let bsstrike f v t delta =
        let d = if delta < 0.0 then 1.0 + delta else delta //call delta
        //let d1 = (log(f/k)+ 0.5*v*v*t)/(v*sqrt(t))
        let d1 = norminvcdf d
        f * exp (-d1 * v * sqrt (t) + 0.5 * v * v * t)

    ///bs quick delta in call
    //computed using atm vol, and center at atmf.
    let bsqdelta f k v t =
        let d1 = (log (f / k)) / (v * sqrt (t))
        normcdf (d1)

    ///bs qdelta to strike
    let bsqstrike f v t delta =
        //let d1 = (log(f/k)+ 0.5*v*v*t)/(v*sqrt(t))
        let d1 = norminvcdf delta
        f * exp (-d1 * v * sqrt (t))


    let bsGreeks f k v t o =
        let h = NumericalDerivative()
        let d = [| f; v; t |]
        let g = (fun (d: double[]) -> bs d.[0] k d.[1] d.[2] o)

        [| h.EvaluatePartialDerivative(g, d, 0, 1) //delta
           h.EvaluatePartialDerivative(g, d, 0, 2) //gamma
           h.EvaluatePartialDerivative(g, d, 1, 1) //vega
           - h.EvaluatePartialDerivative(g, d, 2, 1) / 365. |] //1d theta
