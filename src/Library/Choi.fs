namespace Commod
[<AutoOpen>]
module Choi =
    open MathNet.Numerics
    open MathNet.Numerics.LinearAlgebra
    ///get V with Choi's method with forward weights and cov, general case
    let getVChoi (f:Vector<float>) (w:Vector<float>) (sigma:Matrix<float>) = 
            let v = sigma.Diagonal().PointwiseSqrt()
            let n = f.Count
            let g' = f.*w
            let g = (g'/ (g'.L2Norm()))
            let c = sigma.Cholesky().Factor //cholesky
            let den = sqrt(( g.ToRowMatrix() * sigma * g.ToColumnMatrix()).Item(0,0)) 
            let Q1 = (c.Transpose() * g.ToColumnMatrix() ) /den
            let V1 = (c * Q1)// need to adjust so that w_k V_k1 > 0
            //let eps = 0.01 //choose this following Choi          
            let eps = 1E-10 //increase eps to check if high corr case improves          
            let V1' = DenseVector.init n ( fun n -> 
                if V1.[n,0] * w.[n] > 0. 
                then V1.[n,0]
                else
                   let s = if w.[n] > 0. then 1.0 else -1.0
                   eps * s )       
                   //eps * s * v.[n] )       
            let mu = (c.Inverse() * V1').L2Norm()
            let V1'' = V1' / mu
            let Q1' = c.Inverse() * V1''
            let R = householderR Q1'
            let CR = c * R 
            let CR' = CR.SubMatrix(0, n, 1, n-1) //drop 1st column.
            let svd = CR'.Svd()      
            let V = V1''.ToColumnMatrix().Append(svd.U * svd.W)
            V

    ///get V with Choi's method for 2 assets with constant correlation.
    let getVChoi2Asset (f1:Vector<float>) (fw1:Vector<float>) (t1:Vector<float>) (v1:Vector<float>) 
        (f2:Vector<float>) (fw2:Vector<float>) (t2:Vector<float>) (v2:Vector<float>) (rho:float) = 
            let f = appendVector f1 f2
            let w = appendVector fw1 (fw2 * -1.)
            let sigma = getCov t1 v1 t2 v2 rho
            getVChoi f w sigma

    let consolidateInputs (f1:Vector<float>) (fw1:Vector<float>) (t1:Vector<float>) (v1:Vector<float>) =
        //consolidate future details to group same fixing dates
        //but retain the same weights, so that the delta still works
        let (f, t, v ) = 
            Array.zip3  ((f1 .* fw1).ToArray()) (t1.ToArray())  (v1.ToArray())              
            |> Array.groupBy(fun (_,t,_) -> t) 
            |> Array.map( fun (t0,r) -> 
                let f,t,v = Array.unzip3 r
                let (y1, y11, _) = moments (vector f)  (vector v) (vector t  )
                y1, t0, sqrt( log(y11/y1/y1) / t0 ))     
            |> Array.unzip3
        let fv = vector f
        let tv = vector t
        let vv = vector v
        //let w = DenseVector.create fv.Count 1.
        let w = 
            Array.zip  (fw1.ToArray()) (t1.ToArray())              
            |> Array.groupBy( snd ) 
            |> Array.map(fun (_, r ) -> r |> Array.sumBy fst )
            |> vector
        let fv' = fv / w //TODO need to handle case where w is 0??? e.g. two forward fixing opposite weight, different value?
        fv', w, tv, vv

    //Choi's general method using cov inputs
    let rec optionChoiG (f:Vector<float>) (w:Vector<float>) (sigma:Matrix<float>) strike callput (o:int list)=
        //validate inputs
        if strike < 0. then 
            let callput' = match callput with | Call -> Put | Put -> Call
            let (opt,delta) = optionChoiG f (w * -1.) sigma -strike callput' o //put equivalent
            opt, delta
        else
            if f.Count = 1 then // use bs
                let o = bs (f.[0]*w.[0]) strike (sqrt sigma.[0,0]) 1.0 callput 
                let delta = ( bsdelta (f.[0]*w.[0]) strike (sqrt sigma.[0,0]) 1.0 callput ) * w.[0]
                o, [|delta|]
            else 
            let V = getVChoi f w (fixCov sigma)
            //for each z_dot, find z1 and then C_bs, and then sum them using GH
            let n = f.Count 
            let fk k (z:Vector<float>) = //formula 7 
                let vk = V.Row(k) //kth row vector
                let vksum = vk * vk  - vk.[0] * vk.[0]
                exp( -0.5 * vksum + vk.SubVector(1,z.Count) * z ) //fk for some z
            //let e = V.Svd().S
            //let et = e.Sum()
            //let m = 
            //    e 
            //    |> Vector.toArray 
            //    |> Array.scan (fun t x -> t + x ) 0. 
            //    |> Array.map( fun x -> x / et) 
            //    |> Array.findIndex( fun x -> x > 0.9 ) 
            //let dim = min (n-1) ( min m (o.Length )) //cap at l levels
            let dim = min (n-1) ( o.Length ) //cap at l levels
            let zs,ws = gh o.[ 0 .. (dim - 1)] 
            let wf z = 
                        w  //for each kth fixing
                        |> Vector.mapi( fun k w -> 
                           let fki = fk k z 
                           w *  fki 
                           )
            let wff z = 
                        wf z //for each kth fixing
                        |> Vector.mapi( fun k x -> 
                           x * f.[k]  
                           )
            let fn z1 z = //for each risk factor scenario
                        (wff z //for each kth fixing
                        |> Vector.mapi( fun k w -> 
                           w * exp(-0.5* V.[k,0]*V.[k,0] + V.[k,0]*z1)                
                           )
                        |> Vector.sum) - strike
            let difffn z1 z =
                        wff z  //for each kth fixing
                        |> Vector.mapi( fun k w -> 
                            w * exp( - 0.5 * V.[k,0]*V.[k,0] + V.[k,0]*z1)                
                            *V.[k,0])
                        |> Vector.sum 
            let roots = 
                zs 
                |> Array.map( fun x ->
                    // fn is increasing in z1, linked to fixing0
                    // adding cases for trivial k.
                    let z = vector x
                    let u = 10.
                    let l = -10.
                    (if ( fn u z <= 0. ) then u
                        elif ( fn l z >= 0.) then l
                        else 
                            RootFinding.RobustNewtonRaphson.FindRoot(
                                ( fun z1 -> fn z1 z),
                                ( fun z1 -> difffn z1 z),
                                l, u)) * -1.)
            let opt = 
                roots
                |> Array.mapi( fun i d -> 
                    let z = vector zs.[i]
                    let o = 
                        match callput with
                        | Call -> 
                            (wff z
                            |> Vector.mapi( fun k w -> w * normcdf( d + V.[k,0]))
                            |> Vector.sum )- strike * normcdf(d)                       
                        | Put ->
                            strike * normcdf(-d) - 
                                (wff z
                                |> Vector.mapi( fun k w -> w * normcdf( -d - V.[k,0])) 
                                |> Vector.sum )
                    o * ws.[i])
                |> Array.sum
            let deltas' = 
                w
                |> Vector.mapi( fun k w -> 
                    roots
                    |> Array.mapi( fun i d -> 
                        let z = vector zs.[i] 
                        let wf = fk k z
                        ws.[i] * w * wf * normcdf ( d + V.[k,0] ) )   
                    |> Array.sum )            
            //just return total long/short delta, because after consolidatation, the current implementation did not 
            //trace back to original contracts. 
            let deltas = (match callput with | Call -> deltas' |Put -> deltas' - w) |> Vector.toArray
            let fwderr = 
                w
                |> Vector.mapi( fun k _ -> 
                    zs
                    |> Array.mapi( fun i z' -> 
                        let z = vector z' 
                        let wf = fk k z
                        ws.[i] * (wf - 1.0))
                    |> Array.sum )
            let adj =
                let calladj = 
                    deltas' 
                    |> Vector.mapi( fun k d -> 
                        d * f.[k] * fwderr.[k])
                    |> Vector.sum
                match callput with
                | Call  -> calladj
                | Put ->
                    calladj - 
                        (w
                        |> Vector.mapi( fun k w -> w * f.[k] *fwderr.[k])
                        |> Vector.sum )
            max 0. (opt - adj), deltas  //return deltas in input vector
    
    //Choi model with 4 dim and descretize 17/2
    //This is generally accurate within 0.1c err
    let optionChoi (f:Vector<float>) (w:Vector<float>) (sigma:Matrix<float>) strike callput =
        optionChoiG f w sigma strike callput [7;3;2]

    ///using cov inputs
    let optionChoi2AssetCov (f1:Vector<float>) (fw1:Vector<float>)  
        (f2:Vector<float>) (fw2:Vector<float>) k (sigma:Matrix<float>) callput o =
        //validate inputs
        //call general case
        let f = appendVector f1 f2
        let w = appendVector fw1 (fw2 * -1.) 
        let rho = sigma.[0,f.Count-1] / sqrt ( sigma.[0,0] * sigma.[f.Count-1,f.Count-1])
        let (opt,deltas) = 
            if rho > 0.7 then 
                let n = if k >= 0. then 0 else f.Count-1 
                //let n = if (sigma.[0,0] > sigma.[f.Count-1,f.Count-1]) then 0 else f.Count-1 
                //if k is on the same side of long or short.
                let k' =  -w.[n] * f.[n]
                let fn = f.[n]
                let sigma' = 
                    sigma |> Matrix.mapi ( fun i j v -> 
                        if i = n && j = n then
                            v
                        elif i = n || j = n then
                            -v + sigma.[n,n]
                        else
                            v - sigma.[i,n] - sigma.[j,n] + sigma.[n,n]) 
                //asset price is positive, weights can be either way
                if k = 0.0 then 
                    f.[n] <- 1E-10 //should really remove the factor when the price is 0 or too small, to avoid getV numerical error.
                    sigma' |> Matrix.mapiInPlace ( fun i j v -> 
                        if i = n && j = n then
                            1E-12 
                        elif i = n || j = n then
                            0.
                        else
                            v )
                elif k > 0. then
                    f.[n] <- k
                    w.[n] <- -1.0
                else
                    f.[n] <- -k
                    w.[n] <- 1.0
                let (opt,deltas) = optionChoiG f w sigma' k' callput o
                //todo check delta conversion
                let dn = ((vector deltas) * f  - opt ) / fn 
                deltas.[n] <- dn
                (opt,deltas) 
            else 
                optionChoiG f w sigma k callput o
        let delta1,delta2 = deltas |> Array.splitAt f1.Count
        let delta1sum = Array.sum delta1
        let delta2sum = Array.sum delta2
        let delta = [|delta1sum ; delta2sum |]
        opt, delta  //return deltas in long/short 2 elem array

    //spread option using Choi, without change of numeraire 
    let optionChoi2Asset (f1:Vector<float>) (fw1:Vector<float>) (t1:Vector<float>) (v1:Vector<float>) 
        (f2:Vector<float>) (fw2:Vector<float>) (t2:Vector<float>) v2 k (rho:float) callput =
        let f = appendVector f1 f2
        let w = appendVector fw1 (fw2 * -1.) 
        let sigma = getCov t1 v1 t2 v2 rho
        let (opt,deltas) = optionChoi f w sigma k callput 
        let delta1,delta2 = deltas |> Array.splitAt f1.Count
        let delta1sum = Array.sum delta1
        let delta2sum = Array.sum delta2
        let delta = [|delta1sum ; delta2sum |]
        opt, delta  //return deltas in long/short 2 elem array

    ///assuming perferect correalation in asset
    let optionChoi2AssetG (f1:Vector<float>) (fw1:Vector<float>) (t1:Vector<float>) (v1:Vector<float>) 
        (f2:Vector<float>) (fw2:Vector<float>) (t2:Vector<float>) v2 k (rho:float) callput o =
        let f = appendVector f1 f2
        let w = appendVector fw1 (fw2 * -1.) 
        let sigma = getCov t1 v1 t2 v2 rho
        let (opt,deltas) = optionChoiG f w sigma k callput o
        let delta1,delta2 = deltas |> Array.splitAt f1.Count
        let delta1sum = Array.sum delta1
        let delta2sum = Array.sum delta2
        let delta = [|delta1sum ; delta2sum |]
        opt, delta  //return deltas in long/short 2 elem array

    ///assuming perferect correalation in asset
    ///change of Numeraire to work with high correlation > 0.75 precision issue
    let optionChoi2AssetN (f1:Vector<float>) (fw1:Vector<float>) (t1:Vector<float>) (v1:Vector<float>) 
        (f2:Vector<float>) (fw2:Vector<float>) (t2:Vector<float>) v2 k (rho:float) callput o =
        //validate inputs
        if Vector.exists (fun x -> x <= 0. ) t1 then invalidArg "t1'" "time to matuirty needs to be positive values"
        if Vector.exists (fun x -> x <= 0. ) t2 then invalidArg "t1'" "time to matuirty needs to be positive values"
        if Vector.exists (fun x -> x <= 0. ) v1 then invalidArg "v1'" "vol needs to be positive values"
        if Vector.exists (fun x -> x <= 0. ) v2 then invalidArg "v2'" "vol needs to be positive values"
        //call general case
        let sigma = getCov t1 v1 t2 v2 rho
        optionChoi2AssetCov f1 fw1 f2 fw2 k sigma callput o
        
    //asian Choi method
    let asianoptionChoi (f:Vector<float>) (w:Vector<float>) k (sigma:Matrix<float>) callput =
        let (opt,deltas) = optionChoi f w sigma k callput 
        let delta= Array.sum deltas
        opt, delta  //return deltas in long/short 2 elem array

