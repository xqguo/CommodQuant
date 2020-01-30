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
    let V = getVChoi f1 fw1 t1 V1 f2 fw2 t2 V2 rho 
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
    let V = getVChoi f1 fw1 t1 V1 f2 fw2 t2 V2 rho 
    Assert.Equal(V.Row(0).L2Norm(), v1 * sqrt t1.[0] ,6) |@ (sprintf "V nomral is input vol %f %f" (V.Row(0).L2Norm()) (v1 * sqrt t1.[0])) .&.
    Assert.Equal(V.Row(1).L2Norm(), v1 * sqrt t1.[1] ,6) |@ (sprintf "V nomral is input vol %f %f" (V.Row(0).L2Norm()) (v1 * sqrt t1.[0]))  .&.
    Assert.Equal(V.Row(2).L2Norm(), v1 * sqrt t1.[2] ,6) |@ (sprintf "V nomral is input vol %f %f" (V.Row(0).L2Norm()) (v1 * sqrt t1.[0]))  .&.
    Assert.Equal(V.Row(3).L2Norm(), v1 * sqrt t1.[3] ,6) |@ (sprintf "V nomral is input vol %f %f" (V.Row(0).L2Norm()) (v1 * sqrt t1.[0]))  .&.
    Assert.Equal(V.Row(8).L2Norm(), v2 * sqrt t1.[0] ,6) |@ (sprintf "V nomral is input vol %f %f" (V.Row(0).L2Norm()) (v1 * sqrt t1.[0]))  .&.
    Assert.Equal(V.Row(9).L2Norm(), v2 * sqrt t1.[1] ,6) |@ (sprintf "V nomral is input vol %f %f" (V.Row(0).L2Norm()) (v1 * sqrt t1.[0]))  .&.
    Assert.Equal(V.Row(10).L2Norm(), v2 * sqrt t1.[2] ,6) |@ (sprintf "V nomral is input vol %f %f" (V.Row(0).L2Norm()) (v1 * sqrt t1.[0]))  .&.
    Assert.Equal(V.Row(11).L2Norm(), v2 * sqrt t1.[3] ,6) |@ (sprintf "V nomral is input vol %f %f" (V.Row(0).L2Norm()) (v1 * sqrt t1.[0]))  

[<Property( Verbose = true, Arbitrary = [| typeof<PositiveFloat>|] )>]
let ``test choi vs moment matching``() = 
    let f = 1.
    let callput = Put
    let f1 = DenseVector.create 2 f
    let t1 = vector [ 365.0; 366.0 ] / 365. //fixing dates
    let v1 = vector [ 0.25; 0.25] // vol for each fixing
    let fw1 =vector [ 0.99;  0.01] //weights longside

    let f2 = f1 // forwards short side
    let t2 = t1 //fixing dates
    let v2 = DenseVector.create 2 0.5 // vol for each fixing 
    let fw2 = fw1 //weights longside
    let p1 = Vector.Build.Dense(1, 0.0) //  #past fixing longside
    let k = 0. 
    let rho = 0. //correlation between long/short fixing
    let so =        spreadoption f1 fw1 t1 v1 f2 fw2 t2 v2 k rho callput p1 p1 p1 p1
    let choi,delta' = optionChoi f1 fw1 t1 v1 f2 fw2 t2 v2 k rho callput p1 p1 p1 p1
    (so = choi ) |@ sprintf "spread option  moment match and choi are not so close: %f, %f" so choi
    //Assert.Equal ( (min so choi)/(max so choi), 1.0, 1  )

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

    let p1 = Vector.Build.Dense(8, 0.0) //  #past fixing longside
    let pw1 = Vector.Build.Dense(8, 0.0)
    let p2 = Vector.Build.Dense(8, 0.0)
    let pw2 = Vector.Build.Dense(8, 0.0)

    let rho = 0.5  //correlation between long/short fixing
    let callput = Call
    let choi,_ = optionChoi f1 fw1 t1 v1 f2 fw2 t2 v2 k rho callput p1 pw1 p2 pw2

    let rho = 0.5 //correlation between long/short fixing
    let callput = Put
    let choi',_ = optionChoi f1 fw1 t1 v1 f2 fw2 t2 v2 k rho callput p1 pw1 p2 pw2
    
    Assert.Equal ( choi - choi' , f1 * fw1 - f2 * fw2 - k, 3 )

[<Property( Arbitrary = [| typeof<PositiveFloat>|] )>]
let ``test choi spread put call equivalence `` f1 f2 v1 v2 k = 
    let f1 = DenseVector.create 8 f1
    let t1 = (( [ 40; 41; 42; 43; 44; 47; 48; 49 ] |> List.map float |> vector ) + 0.01 )/ 365. //fixing dates
    let v1 = DenseVector.create 8 v1 // vol for each fixing
    let fw1 = Vector.Build.Dense( 8, 1./8. ) //weights longside

    let f2 = DenseVector.create 8 f2 // forwards short side
    let t2 = t1 //fixing dates
    let v2 = DenseVector.create 8 v2 // vol for each fixing 
    let fw2 = fw1 //weights longside

    let p1 = Vector.Build.Dense(1, 0.0) //  #past fixing longside
    let pw1 = Vector.Build.Dense(1, 0.0)
    let p2 = Vector.Build.Dense(1, 0.0)
    let pw2 = Vector.Build.Dense(1, 0.0)

    let rho = 0.5  //correlation between long/short fixing
    let callput = Call
    let choi,_ = optionChoi f1 fw1 t1 v1 f2 fw2 t2 v2 k rho callput p1 pw1 p2 pw2

    let callput = Put
    let choi',_ = optionChoi f2 fw2 t2 v2 f1 fw1 t1 v1 -k rho callput p1 pw1 p2 pw2
    
    Assert.Equal ( choi , choi' , 3 )
[<Property( Arbitrary = [| typeof<PositiveFloat>|] )>]
let ``test choi vs bs`` f k callput= 
    let f1 = vector [f]
    let t1 = vector [1.] //fixing dates
    let v1 = vector [0.25] // vol for each fixing
    let fw1 = vector [1. ] //weights longside

    let f2 = vector [0.00001] // forwards short side
    let t2 = t1 //fixing dates
    let v2 = vector  [0.00001] // vol for each fixing 
    let fw2 = fw1  //weights longside

    let p1 = vector [ 0.0 ] //  #past fixing longside

    let rho = 0. //correlation between long/short fixing
    let c = bs f k 0.25 1.0 callput 
    let delta = bsdelta f k 0.25 1.0 callput 
    let choi,delta' = optionChoi f1 fw1 t1 v1 f2 fw2 t2 v2 k rho callput p1 p1 p1 p1
    Assert.Equal( c , choi, 2 ) .&.
    Assert.Equal( delta , delta'.[0], 2 )

//[<Property>]
//let ``test choi vs example`` () = 
//    let f1 = DenseVector.create 1 ( exp 0.5)
//    let t1 = DenseVector.create 1 1.0
//    let v1 = DenseVector.create 1 1.0
//    let fw1 = DenseVector.create 1 1.0

//    let f2 = DenseVector.create 1 ( exp 0.5)
//    let t2 = DenseVector.create 1 1.0
//    let v2 = DenseVector.create 1 1.0
//    let fw2 = DenseVector.create 1 1.0

//    let p1 = Vector.Build.Dense(1, 0.0) //  #past fixing longside
//    let pw1 = Vector.Build.Dense(1, 0.0)
//    let p2 = Vector.Build.Dense(1, 0.0)
//    let pw2 = Vector.Build.Dense(1, 0.0)

//    let rho = 0. //correlation between long/short fixing
//    let callput = Call
//    let k = 2.0
//    let choi = optionChoi f1 fw1 t1 v1 f2 fw2 t2 v2 k rho callput p1 pw1 p2 pw2
//    choi > 0.0 |@ sprintf "choi example are close: %f, %f" choi choi
