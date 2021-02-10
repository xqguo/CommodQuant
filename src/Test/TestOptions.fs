module TestOptions
open Xunit
open FsCheck.Xunit
open Commod
open FsCheck
open FsCheckTypes
open MathNet.Numerics.LinearAlgebra


[<Property( Arbitrary = [| typeof<PositiveFloat>|] )>]
let ``generated positive floats should be positive`` x = x > 0.0
//Check.Verbose ``generated positive floats should be positive``

[<Property( Arbitrary = [| typeof<PositiveFloat>|] )>]
let ``test bs`` f  k v t = 
    let c = (bs f k v  t Call)
    let p = (bs f k v  t Put)
    let y = f - k 
    let diff = c - p - y
    let c2 = (bs (f+0.001) k v  t Call)
    let delta = (c2 - c) / 0.001
    (p >= 0.0 ) |@ sprintf "Option value is non-negative" .&.
    (abs(diff) <= 0.001)  |@ sprintf "Must satisfy call put parity %f" diff .&. 
    (delta >= 0.0) |@ sprintf "Call option delta is positive"

[<Property( Arbitrary = [| typeof<PositiveFloat>|] )>]
let ``test spread`` f k = 
    let f1 = DenseVector.create 8 f
    let t1 = (( [ 40; 41; 42; 43; 44; 47; 48; 49 ] |> List.map float |> vector ) + 0.01 )/ 365. //fixing dates
    let v1 = Vector.Build.Dense(8, 0.25) // vol for each fixing
    let fw1 = Vector.Build.Dense( 8, 1./8. ) //weights longside

    let f2 = Vector.Build.Dense(8, 0.0001) // forwards short side
    let t2 = t1 //fixing dates
    let v2 = Vector.Build.Dense(8, 0.0001) // vol for each fixing 
    let fw2 = fw1 //weights longside

    let p1 = Vector.Build.Dense(8, 0.0) //  #past fixing longside
    let pw1 = Vector.Build.Dense(8, 0.0)
    let p2 = Vector.Build.Dense(8, 0.0)
    let pw2 = Vector.Build.Dense(8, 0.0)

    let rho = 0. //correlation between long/short fixing
    let callput = Call
    let so = spreadoption f1 fw1 t1 v1 f2 fw2 t2 v2 k rho callput p1 pw1 p2 pw2
    let ao = asianoption f1 fw1 t1 v1 k callput 0.
    abs ( so - ao ) < 0.001 |@ sprintf "spread option and asian option price are close: %f, %f" so ao

[<Property( Arbitrary = [| typeof<PositiveFloat>|] )>]
let ``test choi vs V norm is input vol for long only basket`` f1 f2 v1 v2= 
    let f1 = DenseVector.create 8 f1
    let t1 = (( [ 40; 41; 42; 43; 44; 47; 48; 49 ] |> List.map float |> vector ) + 0.01 )/ 365. //fixing dates
    let V1 = DenseVector.create 8 v1 // vol for each fixing
    let fw1 = DenseVector.create 8 0.125// vol for each fixing
    let f2 = DenseVector.create 8 f2
    let t2 = t1 //fixing dates
    let V2 = DenseVector.create 8 v2 // vol for each fixing
    let fw2 = fw1 * (-1.) //weights longside
    let rho = 0.8 //correlation between long/short fixing
    let V = getVChoi2Asset f1 fw1 t1 V1 f2 fw2 t2 V2 rho 
    Assert.Equal(V.Row(0).L2Norm(), v1 * sqrt t1.[0] ,6) |@ (sprintf "V nomral is input vol %f %f" (V.Row(0).L2Norm()) (v1 * sqrt t1.[0])) .&.
    Assert.Equal(V.Row(1).L2Norm(), v1 * sqrt t1.[1] ,6) |@ (sprintf "V nomral is input vol %f %f" (V.Row(0).L2Norm()) (v1 * sqrt t1.[0]))  .&.
    Assert.Equal(V.Row(2).L2Norm(), v1 * sqrt t1.[2] ,6) |@ (sprintf "V nomral is input vol %f %f" (V.Row(0).L2Norm()) (v1 * sqrt t1.[0]))  .&.
    Assert.Equal(V.Row(3).L2Norm(), v1 * sqrt t1.[3] ,6) |@ (sprintf "V nomral is input vol %f %f" (V.Row(0).L2Norm()) (v1 * sqrt t1.[0]))  .&.
    Assert.Equal(V.Row(8).L2Norm(), v2 * sqrt t1.[0] ,6) |@ (sprintf "V nomral is input vol %f %f" (V.Row(0).L2Norm()) (v1 * sqrt t1.[0]))  .&.
    Assert.Equal(V.Row(9).L2Norm(), v2 * sqrt t1.[1] ,6) |@ (sprintf "V nomral is input vol %f %f" (V.Row(0).L2Norm()) (v1 * sqrt t1.[0]))  .&.
    Assert.Equal(V.Row(10).L2Norm(), v2 * sqrt t1.[2] ,6) |@ (sprintf "V nomral is input vol %f %f" (V.Row(0).L2Norm()) (v1 * sqrt t1.[0]))  .&.
    Assert.Equal(V.Row(11).L2Norm(), v2 * sqrt t1.[3] ,6) |@ (sprintf "V nomral is input vol %f %f" (V.Row(0).L2Norm()) (v1 * sqrt t1.[0]))  

[<Property( Arbitrary = [| typeof<PositiveFloat>|] )>]
let ``test choi vs V norm is input vol for long short spread`` f1 f2 v1 v2= 
    let f1 = DenseVector.create 8 f1
    let t1 = (( [ 40; 41; 42; 43; 44; 47; 48; 49 ] |> List.map float |> vector ) + 0.01 )/ 365. //fixing dates
    let V1 = DenseVector.create 8 v1 // vol for each fixing
    let fw1 = DenseVector.create 8 0.125// vol for each fixing

    let f2 = DenseVector.create 8 f2
    let t2 = t1 //fixing dates
    let V2 = DenseVector.create 8 v2 // vol for each fixing
    let fw2 = fw1 //weights longside

    let rho = 0.8 //correlation between long/short fixing
    let V = getVChoi2Asset f1 fw1 t1 V1 f2 fw2 t2 V2 rho 
    Assert.Equal(V.Row(0).L2Norm(), v1 * sqrt t1.[0] ,6) |@ (sprintf "V nomral is input vol %f %f" (V.Row(0).L2Norm()) (v1 * sqrt t1.[0])) .&.
    Assert.Equal(V.Row(1).L2Norm(), v1 * sqrt t1.[1] ,6) |@ (sprintf "V nomral is input vol %f %f" (V.Row(0).L2Norm()) (v1 * sqrt t1.[0]))  .&.
    Assert.Equal(V.Row(2).L2Norm(), v1 * sqrt t1.[2] ,6) |@ (sprintf "V nomral is input vol %f %f" (V.Row(0).L2Norm()) (v1 * sqrt t1.[0]))  .&.
    Assert.Equal(V.Row(3).L2Norm(), v1 * sqrt t1.[3] ,6) |@ (sprintf "V nomral is input vol %f %f" (V.Row(0).L2Norm()) (v1 * sqrt t1.[0]))  .&.
    Assert.Equal(V.Row(8).L2Norm(), v2 * sqrt t1.[0] ,6) |@ (sprintf "V nomral is input vol %f %f" (V.Row(0).L2Norm()) (v1 * sqrt t1.[0]))  .&.
    Assert.Equal(V.Row(9).L2Norm(), v2 * sqrt t1.[1] ,6) |@ (sprintf "V nomral is input vol %f %f" (V.Row(0).L2Norm()) (v1 * sqrt t1.[0]))  .&.
    Assert.Equal(V.Row(10).L2Norm(), v2 * sqrt t1.[2] ,6) |@ (sprintf "V nomral is input vol %f %f" (V.Row(0).L2Norm()) (v1 * sqrt t1.[0]))  .&.
    Assert.Equal(V.Row(11).L2Norm(), v2 * sqrt t1.[3] ,6) |@ (sprintf "V nomral is input vol %f %f" (V.Row(0).L2Norm()) (v1 * sqrt t1.[0]))  

[<Property( Verbose = true, EndSize = 100, Arbitrary = [| typeof<PositiveFloat>|] )>]
let ``test choi vs moment matching`` f1 f2 (PositiveInt t) v1 v2 k rho callput ( PositiveInt n) = 
    //let f = 1.
    //let callput = Call
    let s = t*30 
    let n = min n 20 //limit array size
    let f1 = DenseVector.create n f1
    let f2 = DenseVector.create n f2
    let t1 = vector [ float s .. float (s + n-1) ] / 365. //fixing dates
    let t2 = t1
    let v1 = DenseVector.create n ( min v1 1.) // vol for each fixing
    let v2 = DenseVector.create n ( min v2 1.) // vol for each fixing
    let fw1 = DenseVector.create n 1.0/float n
    let fw2 = fw1 //weights longside
    let p1 = Vector.Build.Dense(1, 0.0) //  #past fixing longside
    let rho = max (min rho 0.9) -0.9 //correlation between long/short fixing
    let so =        spreadoption f1 fw1 t1 v1 f2 fw2 t2 v2 k rho callput p1 p1 p1 p1
    let choi,_ = optionChoi2Asset f1 fw1 t1 v1 f2 fw2 t2 v2 k rho callput
    //(so = choi ) |@ sprintf "spread option  moment match and choi are not so close: %f, %f" so choi
    near choi so 0.07

[<Property( Arbitrary = [| typeof<PositiveFloat>|] )>]
let ``test choi put call parity`` f1 f2 k = 
    let f1 = DenseVector.create 8 f1
    let t1 = (( [ 40; 41; 42; 43; 44; 47; 48; 49 ] |> List.map float |> vector ) + 0.01 )/ 365. //fixing dates
    let v1 = Vector.Build.Dense(8, 0.25) // vol for each fixing
    let fw1 = Vector.Build.Dense( 8, 1./8. ) //weights longside

    let f2 = DenseVector.create 8 f2 // forwards short side
    let t2 = t1 //fixing dates
    let v2 = Vector.Build.Dense(8, 0.0001) // vol for each fixing 
    let fw2 = fw1 //weights longside

    let rho = 0.5  //correlation between long/short fixing
    let callput = Call
    let choi,_ = optionChoi2Asset f1 fw1 t1 v1 f2 fw2 t2 v2 k rho callput

    let rho = 0.5 //correlation between long/short fixing
    let callput = Put
    let choi',_ = optionChoi2Asset f1 fw1 t1 v1 f2 fw2 t2 v2 k rho callput
    
    Assert.Equal ( choi - choi' , f1 * fw1 - f2 * fw2 - k, 3 )

[<Property( MaxTest = 10, Arbitrary = [| typeof<PositiveFloat>|] )>]
let ``test choi spread put call equivalence `` f1 f2 v1 v2 k = 
    let v1 = min v1 0.5
    let v2 = min v2 0.5
    let f1 = DenseVector.create 8 f1
    let t1 = (( [ 40; 41; 42; 43; 44; 47; 48; 49 ] |> List.map float |> vector ) + 0.01 )/ 365. //fixing dates
    let v1 = DenseVector.create 8 v1 // vol for each fixing
    let fw1 = Vector.Build.Dense( 8, 1./8. ) //weights longside
    let f2 = DenseVector.create 8 f2 // forwards short side
    let t2 = t1 //fixing dates
    let v2 = DenseVector.create 8 v2 // vol for each fixing 
    let fw2 = fw1 //weights longside
    let rho = 0.5  //correlation between long/short fixing
    let callput = Call
    let choi,_ = optionChoi2Asset f1 fw1 t1 v1 f2 fw2 t2 v2 k rho callput
    let callput = Put
    let choi',_ = optionChoi2Asset f2 fw2 t2 v2 f1 fw1 t1 v1 -k rho callput    
    near choi choi' 1E-3

[<Property( Verbose = true,  Arbitrary = [| typeof<PositiveFloat>|] )>]
let ``test choi vs bs`` f k v t callput= 
    //let f = 100. 
    //let k = 100.
    let v = min v 2.
    let f1 = vector [f]
    let t1 = vector [t] //fixing dates
    let v1 = vector [v] // vol for each fixing
    let fw1 = vector [1. ] //weights longside
    let f2 = vector [0.00001] // forwards short side
    let t2 = t1 //fixing dates
    let v2 = vector  [0.00001] // vol for each fixing 
    let fw2 = fw1  //weights longside
    let rho = 0. //correlation between long/short fixing
    let c = bs f k v t callput 
    let delta = bsdelta f k v t callput 
    let choi,delta' = optionChoi2Asset f1 fw1 t1 v1 f2 fw2 t2 v2 k rho callput
    let eps = 1E-4
    nearstr c choi eps "prem:".&.
    nearstr delta delta'.[0] 0.001 "delta:"

[<Property( Verbose = true, EndSize = 100, Arbitrary = [| typeof<PositiveFloat>;typeof<MyGenerator>|] )>]
let ``test spread option with zero strike and single fixing choi vs mm`` fa fb t v1 v2 (Corr rho) = 
    let callput = Call
    let k = 0.
    let f1 = vector [fa]
    let t = min t 4.
    let t1 = vector [t] //fixing dates
    let v1 = vector [min v1 0.5] // vol for each fixing
    let fw1 = vector [1. ] //weights long side

    let f2 = vector [fb] // forwards short side
    let t2 = t1 //fixing dates
    let v2 = vector  [min v2 0.5 ] // vol for each fixing 
    let fw2 = vector [1.]  //weights short side

    let p1 = vector [ 0.0 ] //  #past fixing longside

    //let rho = max (min (rho/5.0) 0.9) -0.9  //correlation between long/short fixing
    let c = spreadoption f1 fw1 t1 v1 f2 fw2 t2 v2 k rho callput p1 p1 p1 p1
    let choi,_ = optionChoi2AssetN f1 fw1 t1 v1 f2 fw2 t2 v2 k rho callput [7;5;3]
    near c choi 0.001

[<Property(MaxTest = 1)>]
let ``test guass hemite weights sum to 1`` () = 
    let dim = 3
    let ws = ghw5 dim |> vector
    let r = ws.Sum()
    Assert.Equal( 1., r, 6 )

type GHTest =
    static member GHList() =
        Gen.elements [3;5;7;17] 
        |> Gen.nonEmptyListOf
        |> Arb.fromGen

[<Property(MaxTest=20, Verbose=true, EndSize = 4, Arbitrary=[|typeof<GHTest>|])>]
let ``test guass hemite expect normal mean is 0 `` (o:int list) = 
        //let o = Array.toList o
        let ws = ghwn o |> vector
        let hs = ghzn o |> Array.map( Array.reduce (+)) |> vector
        let r = ws* hs
        Assert.Equal( 0., r, 6 )

[<Property(MaxTest = 100, Verbose = true, StartSize=8, EndSize = 100, Arbitrary = [|typeof<PositiveFloat>; typeof<MyGenerator> |] )>]
let testSpreadChoivsKirkZeroStrikeNSmallVol fb (Corr rho) callput = 
    //this is a anlytical case that can be used to test Choi convergence
    let k = 0.
    //let fb = max (min fb 1.3) 0.7
    let nf1 = 1
    let nf2 = 1
    let v1 = 0.2
    let v2 = 0.1
    let t = 1.0
    let v1' = DenseVector.create nf1 v1
    let v2' = DenseVector.create nf2 v2
    let f1 = DenseVector.create nf1 1.0
    let f2 = DenseVector.create nf2 fb
    let t1 = DenseVector.create nf1 t
    let t2 = t1
    let fw1 = DenseVector.create nf1 1.
    let fw2 = DenseVector.create nf2 1.
    //let rho = min rho 0.8 //correlation between long/short fixing
    let v17, _ = optionChoi2AssetN f1 fw1 t1 v1' f2 fw2 t2 v2' k rho callput [17] 
    let o = kirk 1.0 fb k v1 v2 rho t callput   
    //good precison for different cases, even with 3 nodes
    nearstr v17 o 1E-7 "Choi17 vs Kirk" 

[<Property(MaxTest = 1000, Verbose = false, EndSize = 100, Arbitrary = [| typeof<PositiveFloat>;typeof<MyGenerator>|] )>]
let testSpreadChoivsKirkZeroStrikeN fa fb t v1 v2 (Corr rho) callput = 
    //this is a anlytical case that can be used to test Choi convergence
    let k = 0.
    let nf1 = 1
    let nf2 = 1
    let v1 = min v1 0.5
    let v2 = min v2 0.5
    let t = min t 4.0
    let v1' = DenseVector.create nf1 v1
    let v2' = DenseVector.create nf2 v2
    let f1 = DenseVector.create nf1 fa
    let f2 = DenseVector.create nf2 fb
    let t1 = DenseVector.create nf1 t
    let t2 = t1
    let fw1 = DenseVector.create nf1 1.
    let fw2 = DenseVector.create nf2 1.
    let v17, _ = optionChoi2AssetN f1 fw1 t1 v1' f2 fw2 t2 v2' k rho callput [17] 
    let o = kirk fa fb k v1 v2 rho t callput   
    //with high vol and long tenor, high correlation especially, need more nodes
    //but here is the worst case, average case is much better
    // 17 is good enough < 0.1c
    let err = List.max [ 1.; fa; fb; k] * 0.005
    nearstr v17 o err "Choi17 vs Kirk" 

[<Property(MaxTest = 100, Verbose = true, EndSize = 100, Arbitrary = [| typeof<PositiveFloat>;typeof<MyGenerator>|] )>]
let testSpreadChoiNConv fa fb k nf1 nf2 t v1 v2 (Corr rho) callput = 
    let nf1 = min (abs nf1 + 10) 60
    let nf2 = min (abs nf2 + 10) 60
    //let nf1 = 3
    //let nf2 = 2
    let v1 = min v1 0.5
    let v2 = min v2 0.5
    let t = min t 4.0
    let v1' = DenseVector.create nf1 v1
    let v2' = DenseVector.create nf2 v2
    let f1 = DenseVector.create nf1 fa
    let f2 = DenseVector.create nf2 fb
    let t1 = DenseVector.init nf1 (fun i -> t + (float i)/250.)
    let t2 = DenseVector.init nf2 (fun i -> t + (float i)/250.)
    let fw1 = DenseVector.create nf1 1.
    let fw2 = DenseVector.create nf2 1.
    let v, _ = optionChoi2AssetN f1 fw1 t1 v1' f2 fw2 t2 v2' k rho callput [17;3]
    let o, _ = optionChoi2AssetN f1 fw1 t1 v1' f2 fw2 t2 v2' k rho callput [17;7;3;2]
    //general case 732 test < 0.5%
    //here is the worst case, average case is much better
    let err = List.max [ 1.; fa; fb; k] * 0.005
    nearstr v o err "Choi 17/3 vs 17/5/3/2/2" 
