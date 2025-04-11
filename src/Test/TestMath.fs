namespace Test

open System
open FsCheck
open FsCheck.Xunit
open MathNet.Numerics.LinearAlgebra // For vector creation in tests
open Commod // Access the Math module

module TestMath =

    // Helper to create vectors for testing
    let arbVector (size: int) : Arbitrary<Vector<float>> =
        Arb.generate<float list>
        |> Gen.map (fun l ->
            let limitedList = l |> List.truncate size
            let paddedList =
                if List.length limitedList < size then
                    limitedList @ List.init (size - List.length limitedList) (fun _ -> 0.0)
                else
                    limitedList
            DenseVector.ofList paddedList
        )
        |> Arb.fromGen

    // Test normcdf properties
    [<Property>]
    let ``normcdf is between 0 and 1`` (NormalFloat x) =
        let p = normcdf x
        p >= 0.0 && p <= 1.0

    [<Property>]
    let ``normcdf symmetry`` (x: float) =
        // Avoid NaNs or infinities which might break comparison
        if Double.IsNaN(x) || Double.IsInfinity(x) then true else
        let p1 = normcdf x
        let p2 = normcdf -x
        abs (p1 + p2 - 1.0) < 1e-10 // Use tolerance for float comparison

    // Test normpdf properties
    [<Property>]
    let ``normpdf is non-negative`` (NormalFloat x) =
        let p = normpdf x
        p >= 0.0

    // Test norminvcdf properties
    [<Property(MaxTest = 100)>]
    let ``norminvcdf is inverse of normcdf`` (NormalFloat p) =
        // Ensure p is within the valid range (0, 1) exclusive
        let validP = max 1e-10 (min (1.0 - 1e-10) p)
        if validP <= 0.0 || validP >= 1.0 then true else // Skip invalid inputs for InvCDF
        let x = norminvcdf validP
        let p' = normcdf x
        abs (p' - validP) < 1e-6 // Use tolerance

    // Test appendVector
    [<Property>]
    let ``appendVector length is correct`` (list1: float list, list2: float list) =
        let v1 = DenseVector.ofList list1
        let v2 = DenseVector.ofList list2
        let v = appendVector v1 v2
        v.Count = v1.Count + v2.Count

    [<Property>]
    let ``appendVector elements are correct`` (list1: float list, list2: float list) =
    
        let areListsEqual list1 list2 =
            List.length list1 = List.length list2 &&
            List.forall2 (fun x y -> 
                if System.Double.IsNaN x && System.Double.IsNaN y then true
                else x = y
            ) list1 list2

        let v1 = DenseVector.ofList list1
        let v2 = DenseVector.ofList list2
        let v = appendVector v1 v2
        let expected = list1 @ list2
        let actual = v |> Vector.toList
        areListsEqual expected  actual

    // Helper to generate non-zero vectors for householder tests
    let arbNonZeroVector (size: int) : Arbitrary<Vector<float>> =
        arbVector size
        |> Arb.filter (fun v -> v.L2Norm() > 1e-10 && (v - (DenseVector.init size (fun i -> if i = 0 then 1.0 else 0.0))).L2Norm() > 1e-10)

    // Register the generator for a specific size (e.g., 3)
    type TestGenerators = 
        static member Vector3() = arbNonZeroVector 3

    Arb.register<TestGenerators>() |> ignore

    // Test householderR properties
    [<Property(Arbitrary = [| typeof<TestGenerators> |])>]
    let ``householderR matrix dimensions are correct`` (q: Vector<float>) =
        let n = q.Count
        let h = householderR q
        h.RowCount = n && h.ColumnCount = n

    [<Property(Arbitrary = [| typeof<TestGenerators> |])>]
    let ``householderR matrix is symmetric`` (q: Vector<float>) =
        let h = householderR q
        let hT = h.Transpose()
        let diff = h - hT
        diff.FrobeniusNorm() < 1e-9 // Use tolerance for float comparison

    [<Property(Arbitrary = [| typeof<TestGenerators> |])>]
    let ``householderR matrix is orthogonal`` (q: Vector<float>) =
        let n = q.Count
        let h = householderR q
        let hTh = h.TransposeThisAndMultiply(h)
        let identity = DiagonalMatrix.identity n
        let diff = hTh - identity
        diff.FrobeniusNorm() < 1e-9 // Use tolerance for float comparison

    // [<Property(Arbitrary = [| typeof<TestGenerators> |])>]
    // // This property needs careful checking of the expected reflection target
    // let ``householderR reflects q correctly`` (q: Vector<float>) =
    //     let n = q.Count
    //     let h = householderR q
    //     let hq = h * q
    //     let e1 = DenseVector.init n (fun i -> if i = 0 then 1.0 else 0.0)
    //     let expectedNorm = q.L2Norm()
    //     // Check if hq is proportional to e1 with the correct norm
    //     let diff1 = hq - (expectedNorm * e1)
    //     let diff2 = hq + (expectedNorm * e1) // Reflection might be negative

    // --- Gauss-Hermite Tests ---

    // Generator for valid GH orders used in the implementation
    let genOrder = Gen.elements [ 2; 3; 5; 7; 17; 42 ]

    // Generator for a list of orders (dimension 1 to 3)
    let arbOrderList = 
        Gen.choose (1, 3) 
        |> Gen.map (fun _ -> List.init (Gen.choose (1, 3) |> Gen.sample 1 1 |> List.head) (fun _ -> genOrder |> Gen.sample 1 1 |> List.head))
        |> Arb.fromGen

    // Register the order list generator
    type GHGenerators = 
        static member OrderList() = arbOrderList

    Arb.register<GHGenerators>() |> ignore

    [<Property(Arbitrary = [| typeof<GHGenerators> |])>]
    let ``ghzn returns correct number of nodes`` (orders: int list) =
        let expectedCount = orders |> List.reduce (*) // Total nodes = product of orders
        let nodes = ghzn orders
        nodes.Length = expectedCount

    [<Property(Arbitrary = [| typeof<GHGenerators> |])>]
    let ``ghwn returns correct number of weights`` (orders: int list) =
        let expectedCount = orders |> List.reduce (*) // Total weights = product of orders
        let weights = ghwn orders
        weights.Length = expectedCount

    [<Property(Arbitrary = [| typeof<GHGenerators> |])>]
    let ``gh returns nodes and weights matching ghzn and ghwn`` (orders: int list) =
        let nodesZ, weightsW = gh orders
        let nodesZn = ghzn orders
        let weightsWn = ghwn orders
        nodesZ = nodesZn && weightsW = weightsWn

    // Test ghint with a simple function f(x) = 1
    // Integral of exp(-(x/2)^2) dx from -inf to inf is 1
    [<Property(Arbitrary = [| typeof<GHGenerators> |])>]
    let ``ghint integrates constant function correctly`` (orders: int list) =
        let dim = orders.Length
        let f (xs: float[]) = 1.0 // Constant function
        let integral = ghint orders f
        let expected = 1.0
        abs (integral - expected) < 1e-6

    // Test ghint with f(x) = x (1D case)
    // Integral of x dx from -inf to inf is x
    [<Property>]
    let ``ghint integrates x correctly in 1D`` () =
        // Use a reasonably high order for accuracy
        let orders = [ 42 ] // Increase order for higher precision
        let f (x:float[]) = x.[0]
        let integral = ghint orders f
        let expected = 1.0 // Ensure correct computation of sqrt(pi)/2
        abs (integral - expected) < 1e-6

    [<Property(Arbitrary = [| typeof<GHGenerators> |])>]
    let ``ghint integrates standard normal distribution correctly`` (orders: int list) =
        let dim = orders.Length
        let f (xs: float[]) = 1.0 // Constant function over normal distribution
        let integral = ghint orders f
        let expected = System.Math.PI ** (float dim / 2.0) // (sqrt(pi))^dim
        abs (integral - expected) < 1e-9

    [<Property>]
    let ``ghint integrates x^2 over standard normal distribution in 1D`` () =
        let orders = [5] // Use 5 nodes for 1D integration
        let f (xs: float[]) = xs.[0] * xs.[0] // f(x) = x^2
        let integral = ghint orders f
        let expected = sqrt System.Math.PI / 2.0 // Integral of x^2 * exp(-x^2)
        abs (integral - expected) < 1e-9

    // Test ghz3/ghw3 wrappers
    [<Property(MaxTest = 10)>] // Limit tests as it's redundant
    let ``ghz3 matches ghzn`` (dim: PositiveInt) =
        let d = min dim.Get 4 // Keep dimension small
        let orders = List.replicate d 3
        ghz3 d = ghzn orders

    [<Property(MaxTest = 10)>]
    let ``ghw3 matches ghwn`` (dim: PositiveInt) =
        let d = min dim.Get 4
        let orders = List.replicate d 3
        ghw3 d = ghwn orders

    // Test ghz5/ghw5 wrappers
    [<Property(MaxTest = 10)>]
    let ``ghz5 matches ghzn`` (dim: PositiveInt) =
        let d = min dim.Get 4
        let orders = List.replicate d 5
        ghz5 d = ghzn orders

    [<Property(MaxTest = 10)>]
    let ``ghw5 matches ghwn`` (dim: PositiveInt) =
        let d = min dim.Get 4
        let orders = List.replicate d 5
        ghw5 d = ghwn orders
