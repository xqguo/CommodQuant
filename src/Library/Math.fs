﻿namespace Commod

[<AutoOpen>]
module Math =
    open MathNet.Numerics.Distributions
    open MathNet.Numerics.LinearAlgebra

    let normcdf = fun x -> Normal.CDF(0., 1., x)
    let normpdf = fun x -> Normal.PDF(0., 1., x)
    let norminvcdf = fun x -> Normal.InvCDF(0., 1., x)

    /// append 2 vectors
    let appendVector (v1: Vector<float>) (v2: Vector<float>) =
        v1.ToColumnMatrix().Stack(v2.ToColumnMatrix()).Column(0)

    ///householder reflection matrix
    let householderR (q: Vector<float>) =
        let n = q.Count
        let e = DenseVector.init n (fun n -> if n = 0 then 1.0 else 0.0)
        let q' = (q - e)
        let v = q' / q'.L2Norm()
        (DiagonalMatrix.identity n) - 2.0 * v.OuterProduct(v) //formua 24

    /// helper function to get multi-dim GH zs
    /// permulate x y into x*y entries of zs.
    let private permutate x y =
        let xi = Array.length x //x is the new head
        let yi = Array.length y //y is the exisiting tail

        [| for i in 0 .. (xi - 1) do
               for j in 0 .. (yi - 1) do
                   yield Array.append [| x.[i] |] y.[j] |]

    ///helper function to get multi-dim GH weigths.
    ///cumulative prod of normalized weights
    let private permprod x y =
        let xi = Array.length x //x is the new head
        let yi = Array.length y //y is the exisiting tail

        [| for i in 0 .. (xi - 1) do
               for j in 0 .. (yi - 1) do
                   yield x.[i] * y.[j] |]

    //recursive version with dim input
    //https://en.wikipedia.org/wiki/Gauss%E2%80%93Hermite_quadrature
    // 1 <= dim
    let rec ghzn (o: int list) =
        let s2 = sqrt 2.0

        let z n =
            //https://keisan.casio.com/exec/system/1281195844
            match n with
            | 2 -> [| -0.7071067811865475244008; 0.7071067811865475244008 |]
            | 3 -> [| -1.2247448714; 0.; 1.2247448714 |]
            | 5 ->
                [| -2.020182870456085632929
                   -0.9585724646138185071128
                   0.
                   0.9585724646138185071128
                   2.020182870456085632929 |]
            | 7 ->
                [| -2.651961356835233492447
                   -1.673551628767471445032
                   -0.8162878828589646630387
                   0.
                   0.8162878828589646630387
                   1.673551628767471445032
                   2.651961356835233492447 |]
            | 17 ->
                [| -4.871345193674403088349
                   -4.061946675875474306892
                   -3.378932091141494083383
                   -2.757762915703888730926
                   -2.173502826666620819275
                   -1.612924314221231333113
                   -1.067648725743450553631
                   -0.5316330013426547313491
                   0.
                   0.5316330013426547313491
                   1.067648725743450553631
                   1.612924314221231333113
                   2.173502826666620819275
                   2.757762915703888730926
                   3.378932091141494083383
                   4.061946675875474306892
                   4.871345193674403088349 |]
            | 42 ->
                [| -8.325809389566931216288
                   -7.644783295704741859325
                   -7.078867873049108721921
                   -6.57201717138747510262
                   -6.10285233438152662139
                   -5.660357581283057698747
                   -5.237915885017649930593
                   -4.831153629128275966268
                   -4.4369817058810309607
                   -4.053107744424767000616
                   -3.677763316388556876407
                   -3.30954009651092198804
                   -2.947285782305479233738
                   -2.590034870617126460575
                   -2.23696078705431805169
                   -1.887341620543484820361
                   -1.540534800915545895967
                   -1.195957794377809835979
                   -0.8530729091605537180465
                   -0.5113749183154693494509
                   -0.170380585561816973074
                   0.170380585561816973074
                   0.511374918315469349451
                   0.8530729091605537180465
                   1.195957794377809835979
                   1.540534800915545895967
                   1.887341620543484820361
                   2.23696078705431805169
                   2.590034870617126460575
                   2.947285782305479233738
                   3.30954009651092198804
                   3.677763316388556876407
                   4.053107744424767000616
                   4.4369817058810309607
                   4.831153629128275966268
                   5.237915885017649930593
                   5.660357581283057698747
                   6.10285233438152662139
                   6.57201717138747510262
                   7.078867873049108721921
                   7.644783295704741859325
                   8.325809389566931216288 |]
            | _ -> failwith "Not implemented"
            |> Array.map (fun x -> x * s2)

        match o with
        | [] -> invalidArg "o" "order should not be empty list"
        | [ h ] ->
            [| for x in (z h) do
                   yield [| x |] |]
        | h :: t -> permutate (z h) (ghzn t) //previous layer)

    let rec ghwn (o: int list) =
        let cons = sqrt (System.Math.PI)

        let w n =
            //https://keisan.casio.com/exec/system/1281195844
            match n with
            | 2 -> [| 0.8862269254527580136491; 0.8862269254527580136491 |]
            | 3 -> [| 0.2954089752; 1.1816359006; 0.2954089752 |]
            | 5 ->
                [| 0.01995324205904591320774
                   0.3936193231522411598285
                   0.9453087204829418812257
                   0.393619323152241159828
                   0.01995324205904591320774 |]
            | 7 ->
                [| 9.71781245099519154149E-4
                   0.05451558281912703059218
                   0.4256072526101278005203
                   0.810264617556807326765
                   0.4256072526101278005203
                   0.0545155828191270305922
                   9.71781245099519154149E-4 |]
            | 17 ->
                [| 4.58057893079863330581E-11
                   4.97707898163079405228E-8
                   7.11228914002130958353E-6
                   2.986432866977530411513E-4
                   0.0050673499576275379117
                   0.0409200341497562798095
                   0.1726482976700970792177
                   0.4018264694704119565776
                   0.5309179376248635603319
                   0.4018264694704119565776
                   0.1726482976700970792177
                   0.0409200341497562798095
                   0.005067349957627537911701
                   2.98643286697753041151E-4
                   7.11228914002130958353E-6
                   4.97707898163079405228E-8
                   4.58057893079863330581E-11 |]
            | 42 ->
                [| 6.1678589258107142561E-31
                   2.5278698640535659933E-26
                   9.1778906956924076508E-23
                   8.4821520800862769186E-20
                   3.03589034781071776352E-17
                   5.2533377155685611014E-15
                   5.03270558218403059708E-13
                   2.92172883723335694663E-11
                   1.09580522880784158623E-9
                   2.7834715265490756444E-8
                   4.96365939357983527576E-7
                   6.3902459677354268946E-6
                   6.0719621077883351208E-5
                   4.334122717212549994714E-4
                   0.00235716139459631447181
                   0.00987952405318850910393
                   0.03220210128890782981979
                   0.0822112693032937689871
                   0.1652880012746674607125
                   0.262738906782294764379
                   0.331048913890856797403
                   0.3310489138908567974027
                   0.2627389067822947643795
                   0.165288001274667460713
                   0.0822112693032937689871
                   0.03220210128890782981979
                   0.00987952405318850910393
                   0.00235716139459631447181
                   4.33412271721254999471E-4
                   6.07196210778833512076E-5
                   6.39024596773542689462E-6
                   4.9636593935798352758E-7
                   2.78347152654907564443E-8
                   1.09580522880784158623E-9
                   2.92172883723335694663E-11
                   5.03270558218403059708E-13
                   5.2533377155685611014E-15
                   3.03589034781071776352E-17
                   8.4821520800862769186E-20
                   9.1778906956924076508E-23
                   2.52786986405356599327E-26
                   6.16785892581071425612E-31 |]
            | _ -> failwith "Not implemented"
            |> Array.map (fun x -> x / cons)

        match o with
        | [] -> invalidArg "o" "order should not be empty"
        | [ h ] -> w h
        | h :: t -> permprod (w h) (ghwn t)

    let ghz5 d = ghzn (List.replicate d 5)
    let ghw5 d = ghwn (List.replicate d 5)
    let ghz3 d = ghzn (List.replicate d 3)
    let ghw3 d = ghwn (List.replicate d 3)
    let gh o = ghzn o, ghwn o

    // o is a list of order of Ghass Hermite for each dim of integration
    /// <summary>
    /// Performs numerical integration over a normal distribution using Gauss-Hermite quadrature.
    /// </summary>
    /// <param name="o">A list of integers specifying the Gauss-Hermite quadrature order for each dimension. Higher orders provide more accurate results.</param>
    /// <param name="f">A function that takes an array of floats (representing points in the integration space) and returns a float (the value of the function to integrate).</param>
    /// <returns>A single float representing the approximate value of the integral.</returns>
    /// <example>
    /// // Example: Integrate f(x) = x^2 over a normal distribution in 1D
    /// let f (xs: float[]) = xs.[0] * xs.[0]
    /// let orders = [5] // Use 5 nodes for 1D integration
    /// let result = ghint orders f
    /// printfn "The integral is approximately: %f" result
    /// </example>
    let ghint (o: int list) (f: (float[] -> float)) =
        let zs, ws = gh o
        let os = zs |> Array.map f
        (os, ws) ||> Array.map2 (*) |> Array.sum
