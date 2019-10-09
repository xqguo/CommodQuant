(*** hide ***)
#load "../packages/FsLab/Themes/DefaultWhite.fsx"
#load "../packages/FsLab/FsLab.fsx"
(**

Testing code with FsCheck
========================

Testing code is an important part of writing code. Randomized domain testing is a
good way to test the code throughly and identify issues from a different perspective. 
For this, [FsCheck](https://fscheck.github.io/FsCheck/) is a very good tool. Read this good introduction for [Property based testing ]( https://fsharpforfunandprofit.com/posts/property-based-testing/).

A quick example
-----------------

If we want to a test that works for all small int bewteen 1 and 100, we could define a random test domain
that test only the necessary range, as shown in the following code. 
The test would often fail as we are testing for a smaller range.
*)

#I "../.paket/load/"
#load "FsCheck.fsx"
open FsCheck
///int arb filtered with range 
type SmallInt =
    static member Int() =
        Arb.Default.Int32()
        |> Arb.filter (fun t -> (t > 1) && (t <= 100))

let testsmallint s = 
    (s > 1 && s < 11) |@ "Input is between 1 and 11"

(**
The test can be ran using Check.One with arguments to 
 shows the passed tests inputs as well as failed cases with shrink
*)
(*** define-output:loading ***)
Check.One({ Config.Verbose with MaxTest = 5; Arbitrary = [ typeof<SmallInt> ] }, testsmallint)
(*** include-output:loading ***)
