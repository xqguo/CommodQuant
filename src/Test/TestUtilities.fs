module TestUtils

open System
open System.IO
open Xunit
open FsCheck
open FsCheck.Xunit
open Commod.Utils
open System.Globalization

// Test for toString function
[<Property>]
let ``test toString function`` (x: Result<int, string>) =
    let result = toString x
    match x with
    | Ok _ -> result = "Ok"
    | Error _ -> result = "Error"

// Test for fromString function
[<Property>]
let ``test fromString function`` (s: string) =
    let result = fromString<Result<int, string>> s
    match s with
    | "Ok" -> result = Some (Ok 0)
    | "Error" -> result = Some (Error "")
    | _ -> result = None

// Test for tryParseWith function
[<Property>]
let ``test tryParseWith function`` (x: string) =
    let result = tryParseWith Int32.TryParse x
    match Int32.TryParse x with
    | true, v -> result = Some v
    | false, _ -> result = None

// Test for parseDate function
[<Property>]
let ``test parseDate function`` (x: string) =
    let result = parseDate x
    match DateTime.TryParse x with
    | true, v -> result = Some v
    | false, _ -> result = None

// Test for parseInt function
[<Property>]
let ``test parseInt function`` (x: string) =
    let result = parseInt x
    match Int32.TryParse x with
    | true, v -> result = Some v
    | false, _ -> result = None

// Test for parseSingle function
[<Property>]
let ``test parseSingle function`` (x: string) =
    let result = parseSingle x
    match Single.TryParse x with
    | true, v -> result = Some v
    | false, _ -> result = None

// Test for parseDouble function
[<Property>]
let ``test parseDouble function`` (x: string) =
    let result = parseDouble x
    match Double.TryParse x with
    | true, v -> result = Some v
    | false, _ -> result = None

// Test for parseDouble10 function
[<Property>]
let ``test parseDouble10 function`` (x: string) =
    let result = parseDouble10 x
    match Double.TryParse x with
    | true, v -> result = Some (sprintf "%.10g" v)
    | false, _ -> result = None

// Test for parseMMddyy function
[<Property>]
let ``test parseMMddyy function random string`` (x: string) =
    try
        let expected = DateTime.ParseExact(x, "MM/dd/yy", culture)
        //only test result if the string can be passed.
        let result = parseMMddyy x
        result = expected
    with
    | _ -> true

[<Property>]
let ``test parseMMddyy function expected string`` (d: DateTime) =
    let x = d.ToString("MM/dd/yy")
    try
        let expected = DateTime.ParseExact(x, "MM/dd/yy", culture)
        //only test result if the string can be passed.
        let result = parseMMddyy x
        result = expected
    with
    | _ -> true

// Test for datestr function
[<Property>]
let ``test datestr function`` (NonEmptyString(x)) =
    let result = datestr x
    result = (x.ToUpper() |> String.filter Char.IsLetterOrDigit)

let getCultureFormats (culture: CultureInfo) =
    let dtfi = culture.DateTimeFormat
    [ dtfi.ShortDatePattern; dtfi.LongDatePattern; dtfi.ShortTimePattern; dtfi.LongTimePattern ]

let cultureFormats = getCultureFormats culture
let genFormat = Gen.elements cultureFormats

let arbFormat = Arb.fromGen genFormat
// Test for parseDateExact function
type CustomGenerators =
    static member FormatString() = arbFormat

[<Property(Arbitrary = [| typeof<CustomGenerators> |])>]
let ``test parseDateExact function`` (format: string) (d: DateTime) =
    try
        let x = d.ToString(format)
        let result = parseDateExact format x
        let (s, d) = DateTime.TryParseExact((datestr x), format, culture, DateTimeStyles.None)
        if s then result = Some d else result = None
    with
    | _ -> true

// Test for tryFile function
[<Property>]
let ``test tryFile function`` (f: string) =
    let result = tryFile f
    if File.Exists(f) then result = Some f else result = None

// Test for copyToAsync function
[<Fact>]
let ``test copyToAsync function`` () =
    let source = "source.txt"
    let dest = "dest.txt"
    File.WriteAllText(source, "test content")
    copyToAsync source dest |> Async.RunSynchronously
    let result = File.ReadAllText(dest)
    File.Delete(source)
    File.Delete(dest)
    result = "test content"

// Test for readLines function
[<Fact>]
let ``test readLines function`` () =
    let path = "test.txt"
    File.WriteAllLines(path, [| "line1"; "line2"; "line3" |])
    let result = readLines path |> Seq.toList
    File.Delete(path)
    result = [ "line1"; "line2"; "line3" ]

// Test for writeFile function
[<Fact>]
let ``test writeFile function`` () =
    let path = "test.txt"
    let lines = [ "line1"; "line2"; "line3" ]
    writeFile path (Array.ofList lines)
    let result = File.ReadAllLines(path) |> Seq.toList
    File.Delete(path)
    result = lines

// Test for moveFile function
[<Fact>]
let ``test moveFile function`` () =
    let outputdir = "output"
    Directory.CreateDirectory(outputdir) |> ignore
    let file = "test.txt"
    File.WriteAllText(file, "test content")
    let result = moveFile outputdir file
    let movedContent = File.ReadAllText(result)
    Directory.Delete(outputdir, true)
    movedContent = "test content"

// Test for updatefileAsync function
[<Fact>]
let ``test updatefileAsync function`` () =
    let source = "source.txt"
    let dest = "dest.txt"
    File.WriteAllText(source, "test content")
    updatefileAsync (FileInfo(source)) (FileInfo(dest)) |> Async.RunSynchronously
    let result = File.ReadAllText(dest)
    File.Delete(source)
    File.Delete(dest)
    result = "test content"

// Test for updatedirAsync function
[<Fact>]
let ``test updatedirAsync function`` () =
    let sourceDir = "sourceDir"
    let destDir = "destDir"
    Directory.CreateDirectory(sourceDir) |> ignore
    Directory.CreateDirectory(destDir) |> ignore
    File.WriteAllText(Path.Combine(sourceDir, "test.txt"), "test content")
    updatedirAsync sourceDir destDir |> Async.RunSynchronously
    let result = File.ReadAllText(Path.Combine(destDir, "test.txt"))
    Directory.Delete(sourceDir, true)
    Directory.Delete(destDir, true)
    result = "test content"
