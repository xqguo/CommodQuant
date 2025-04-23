module TestPricer

open Xunit
open Commod
open FsCheck
open FsCheck.FSharp
open FsCheck.Xunit
open FsCheckTypes

[<Fact>]
let ``getEqualWeights returns equal weights for array`` () =
    let arr = [|1;2;3|]
    let weights = getEqualWeights arr
    Assert.Equal(3, weights.Length)
    Assert.All(weights, fun w -> Assert.Equal(1.0/3.0, w, 6))

[<Fact>]
let ``getEqualWeights returns empty for empty array`` () =
    let arr : int[] = [||]
    let weights = getEqualWeights arr
    Assert.Empty(weights)

[<Fact>]
let ``getEqualWeights returns 1.0 for single element array`` () =
    let arr = [|42|]
    let weights = getEqualWeights arr
    Assert.Single(weights) |> ignore
    Assert.Equal(1.0, weights.[0], 6)

[<Fact>]
let ``splitDetails splits correctly for future and past`` () =
    let now = System.DateTime(2025, 4, 21)
    let details = [| now.AddDays(-1.0), 1.0, "A"; now.AddDays(1.0), 2.0, "B" |]
    let past, future = splitDetails now details
    Assert.Single(past) |> ignore
    Assert.Single(future) |> ignore
    Assert.Equal("A", (past.[0] |> fun (_,_,c) -> c))
    Assert.Equal("B", (future.[0] |> fun (_,_,c) -> c))

[<Fact>]
let ``splitDetails returns all past if all dates are before pricingDate`` () =
    let now = System.DateTime(2025, 4, 21)
    let details = [| now.AddDays(-3.0), 1.0, "A"; now.AddDays(-1.0), 2.0, "B" |]
    let past, future = splitDetails now details
    Assert.Equal(2, past.Length)
    Assert.Empty(future)

[<Fact>]
let ``splitDetails returns all future if all dates are after pricingDate`` () =
    let now = System.DateTime(2025, 4, 21)
    let details = [| now.AddDays(1.0), 1.0, "A"; now.AddDays(2.0), 2.0, "B" |]
    let past, future = splitDetails now details
    Assert.Empty(past)
    Assert.Equal(2, future.Length)

[<Fact>]
let ``toVector converts array to vector`` () =
    let arr = [|1.0; 2.0; 3.0|]
    let v = toVector arr
    Assert.Equal(3, v.Count)
    Assert.Equal(1.0, v.[0], 6)
    Assert.Equal(2.0, v.[1], 6)
    Assert.Equal(3.0, v.[2], 6)

[<Fact>]
let ``toVector handles empty array`` () =
    let arr : float[] = [||]
    let v = toVector arr
    Assert.Equal(0, v.Count)

[<Property(Verbose= true, Arbitrary = [| typeof<ValidDate>;typeof<IntLessThan100> |])>]
let ``shiftMonth shifts by positive and negative months`` d n =
    // Assuming pillarToDate and formatPillar are available and work as expected
    // These are stubbed for test purposes
    let origPillar = formatPillar d
    let n = if d < System.DateTime(2025, 1, 1) then n else -n // Ensure n is negative for past dates
    let result1 = shiftMonth origPillar n
    let result2 = shiftMonth result1 -n
    let d0 = (pillarToDate result1)
    let d1 = (pillarToDate result2)
    let d2 = d1.AddMonths(n)
    d2 = d0 |> Prop.label (sprintf "Expected %A, got %A" d0 d2 ) .&.
    (result2 = origPillar |> Prop.label (sprintf "Expected %A, got %A" origPillar result2))
    
[<Fact>]
let ``applyFormula throws on invalid formula`` () =
    let dt = System.DateTime(2025, 4, 1)
    Assert.Throws<System.Exception>(fun () -> applyFormula "abc" dt |> ignore) |> ignore
    Assert.Throws<System.Exception>(fun () -> applyFormula "1234" dt |> ignore) |> ignore

[<Fact>]
let ``applyFormula returns correct lags for 601 formula`` () =
    let dt = System.DateTime(2025, 4, 1)
    let lags = applyFormula "601" dt
    Assert.Equal<int[]>([| -6 .. -1 |], lags)

// Property: getEqualWeights returns array of same length, sum is 1.0 for non-empty
[<Property>]
let ``getEqualWeights sum is 1.0 for non-empty array`` (xs: int[]) =
    (xs.Length = 0) ||
        let ws = getEqualWeights xs
        ws.Length = xs.Length && abs (Array.sum ws - 1.0) < 1e-10

// Property: toVector preserves length and values
[<Property>]
let ``toVector preserves length and values`` (xs: NormalFloat[]) =
    let xs = Array.map float xs
    let v = xs |> toVector
    v.Count = xs.Length && Array.forall2 (fun a b -> abs(a-b) < 1e-10) xs (v.ToArray())

// Property: splitDetails past/future partitioning
[<Property>]
let ``splitDetails partitions correctly`` (now: System.DateTime) (triples: (System.DateTime * NormalFloat * string)[]) =
    let details = triples |> Array.sortBy (fun (d,_,_) -> d) // ensure order
    let past, future = splitDetails now details
    Array.forall (fun (d,_,_) -> d <= now) past && Array.forall (fun (d,_,_) -> d > now) future && Array.append past future = details

//test SwapPricer
[<Property(MaxTest = 10)>]
let ``SwapPricer returns finite value for random price curves`` (NormalFloat price) =
    // Arrange
    let inst = DBRT // Use a standard instrument, e.g., NG (Natural Gas)
    let d1 = System.DateTime(2025, 5, 1)
    let d2 = System.DateTime(2025, 5, 31)
    let p = decimal price
    // Construct a simple PriceCurve with one pillar and price
    let priceMap = Map.ofList [ ("MAY-25", USDBBL(p*1.0M<USD/bbl>))]
    let priceCurve = PriceCurve priceMap
    let pricingDate = System.DateTime(2025, 4, 21)
    // Act
    let result = SwapPricer inst d1 d2 priceCurve pricingDate
    // Assert: result should be a float and not throw
    nearstr result price 1E-8 "Expected value for price curve" // Adjust tolerance as needed
