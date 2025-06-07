namespace Commod

[<AutoOpen>]
module Options =
    open MathNet.Numerics.Differentiation

    /// <summary>
    /// Returns the Black-Scholes forward price for a given set of parameters and payoff type.
    /// </summary>
    /// <param name="f">The forward price of the underlying asset.</param>
    /// <param name="k">The strike price.</param>
    /// <param name="v">The volatility.</param>
    /// <param name="t">The time to maturity.</param>
    /// <param name="o">The option payoff type (Call or Put).</param>
    /// <returns>The Black-Scholes price.</returns>
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

    /// <summary>
    /// Computes the Kirk approximation for spread option pricing.
    /// </summary>
    /// <param name="f1">The forward price of asset 1.</param>
    /// <param name="f2">The forward price of asset 2.</param>
    /// <param name="k">The strike price.</param>
    /// <param name="v1">The volatility of asset 1.</param>
    /// <param name="v2">The volatility of asset 2.</param>
    /// <param name="rho">The correlation between the two assets.</param>
    /// <param name="t">The time to maturity.</param>
    /// <param name="o">The option payoff type (Call or Put).</param>
    /// <returns>The Kirk approximation price.</returns>
    let kirk f1 f2 k v1 v2 rho t o =
        let v =
            sqrt (v1 * v1 + (v2 * f2 / (f2 + k)) ** 2.0 - 2.0 * rho * v1 * v2 * f2 / (f2 + k))

        bs f1 (f2 + k) v t o

    /// <summary>
    /// Computes the Black-Scholes delta for a given set of parameters and payoff type.
    /// </summary>
    /// <param name="f">The forward price of the underlying asset.</param>
    /// <param name="k">The strike price.</param>
    /// <param name="v">The volatility.</param>
    /// <param name="t">The time to maturity.</param>
    /// <param name="o">The option payoff type (Call or Put).</param>
    /// <returns>The Black-Scholes delta.</returns>
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

    /// <summary>
    /// Computes the strike corresponding to a given Black-Scholes delta.
    /// </summary>
    /// <param name="f">The forward price of the underlying asset.</param>
    /// <param name="v">The volatility.</param>
    /// <param name="t">The time to maturity.</param>
    /// <param name="delta">The target delta.</param>
    /// <returns>The strike corresponding to the given delta.</returns>
    let bsstrike f v t delta =
        let d = if delta < 0.0 then 1.0 + delta else delta //call delta
        //let d1 = (log(f/k)+ 0.5*v*v*t)/(v*sqrt(t))
        let d1 = norminvcdf d
        f * exp (-d1 * v * sqrt (t) + 0.5 * v * v * t)

    /// <summary>
    /// Computes the Black-Scholes quick delta for a call option using ATM volatility.
    /// </summary>
    /// <param name="f">The forward price of the underlying asset.</param>
    /// <param name="k">The strike price.</param>
    /// <param name="v">The volatility.</param>
    /// <param name="t">The time to maturity.</param>
    /// <returns>The quick delta for a call option.</returns>
    let bsqdelta f k v t =
        let d1 = (log (f / k)) / (v * sqrt (t))
        normcdf (d1)

    /// <summary>
    /// Computes the strike corresponding to a given Black-Scholes quick delta.
    /// </summary>
    /// <param name="f">The forward price of the underlying asset.</param>
    /// <param name="v">The volatility.</param>
    /// <param name="t">The time to maturity.</param>
    /// <param name="delta">The target delta.</param>
    /// <returns>The strike corresponding to the given quick delta.</returns>
    let bsqstrike f v t delta =
        //let d1 = (log(f/k)+ 0.5*v*v*t)/(v*sqrt(t))
        let d1 = norminvcdf delta
        f * exp (-d1 * v * sqrt (t))


    /// <summary>
    /// Computes the Black-Scholes Greeks (delta, gamma, vega, theta) using numerical differentiation.
    /// </summary>
    /// <param name="f">The forward price of the underlying asset.</param>
    /// <param name="k">The strike price.</param>
    /// <param name="v">The volatility.</param>
    /// <param name="t">The time to maturity.</param>
    /// <param name="o">The option payoff type (Call or Put).</param>
    /// <returns>An array containing delta, gamma, vega, and theta.</returns>
    let bsGreeks f k v t o =
        let h = NumericalDerivative()
        let d = [| f; v; t |]
        let g = (fun (d: double[]) -> bs d.[0] k d.[1] d.[2] o)

        [| h.EvaluatePartialDerivative(g, d, 0, 1) //delta
           h.EvaluatePartialDerivative(g, d, 0, 2) //gamma
           h.EvaluatePartialDerivative(g, d, 1, 1) //vega
           - h.EvaluatePartialDerivative(g, d, 2, 1) / 365. |] //1d theta
