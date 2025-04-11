namespace Test

open System
open System.IO
open Xunit
open FsCheck
open FsCheck.Xunit
open Commod // Access the IOcsv module

module TestIO =

    // Helper function to run tests within a temporary directory
    let withTempDir (testFunc: string -> unit) =
        let tempDir = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName())
        Directory.CreateDirectory(tempDir) |> ignore
        let originalRoot = IOcsv.ROOT // Backup original ROOT
        IOcsv.ROOT <- tempDir // Set ROOT to temp dir for test isolation
        // Clear holiday cache before each test that uses it
        IOcsv.hols.Clear()
        try
            testFunc tempDir
        finally
            IOcsv.ROOT <- originalRoot // Restore original ROOT
            try
                Directory.Delete(tempDir, true) // Clean up temp dir
            with _ -> () // Ignore cleanup errors

    // Helper to create dummy holiday files
    let createDummyHolidayFile (dir: string) (code: HolidayCode) (dates: string list) =
        let holidayDir = Path.Combine(dir, "holidays")
        Directory.CreateDirectory(holidayDir) |> ignore
        let filePath = Path.Combine(holidayDir, $"{code}.txt")
        File.WriteAllLines(filePath, dates)
        filePath

    // Helper to create dummy CSV files
    let createDummyCsvFile (dir: string) (fileName: string) (content: string) =
        let csvDir = Path.Combine(dir, "csv")
        Directory.CreateDirectory(csvDir) |> ignore
        let filePath = Path.Combine(csvDir, fileName)
        File.WriteAllText(filePath, content)
        filePath

    [<Fact>]
    let ``getCalendarbyCode reads correct dates`` () =
        withTempDir (fun tempDir ->
            let code = HolidayCode.ICE
            let dates = ["2024Jan01"; "2024Dec25"; "InvalidDate"]
            createDummyHolidayFile tempDir code dates |> ignore

            let calendar = getCalendarbyCode code
            let expected = set [DateTime(2024, 1, 1); DateTime(2024, 12, 25)]

            Assert.Equal(expected, calendar)
        )

    [<Fact>]
    let ``getCalendarbyCode returns empty for non-existent file`` () =
         withTempDir (fun _ -> // tempDir is created but no file inside
            let code = HolidayCode.CME
            let calendar = getCalendarbyCode code
            Assert.True(Set.isEmpty calendar)
         )

    [<Fact>]
    let ``getCalendarbyCode caches results`` () =
        withTempDir (fun tempDir ->
            let code = HolidayCode.PLTLDN
            let dates = ["2024May06"]
            let filePath = createDummyHolidayFile tempDir code dates

            let calendar1 = getCalendarbyCode code // First call - reads file
            Assert.Equal(set [DateTime(2024, 5, 6)], calendar1)

            // Delete the file - subsequent call should use cache
            File.Delete(filePath)

            let calendar2 = getCalendarbyCode code // Second call - should hit cache
            Assert.Equal(set [DateTime(2024, 5, 6)], calendar2)
        )

    // Example test for getCalendar - combines multiple holiday sets
    [<Fact>]
    let ``getCalendar combines calendars correctly for NBP`` () =
        withTempDir (fun tempDir ->
            createDummyHolidayFile tempDir HolidayCode.ICE ["2024Jan01"] |> ignore
            createDummyHolidayFile tempDir HolidayCode.UK ["2024Dec25"; "2024Dec26"] |> ignore

            let calendar = getCalendar Instrument.NBP
            let expected = set [DateTime(2024, 1, 1); DateTime(2024, 12, 25); DateTime(2024, 12, 26)]

            Assert.Equal(expected, calendar)
        )

    // Test try*File functions
    [<Fact>]
    let ``tryPriceFile finds existing file`` () =
        withTempDir (fun tempDir ->
            let instrument = Instrument.BRT
            let fileName = $"{instrument}_Price.csv"
            let expectedPath = createDummyCsvFile tempDir fileName "dummy content"

            let result = tryPriceFile instrument
            match result with
            | Some path -> Assert.Equal(expectedPath, path)
            | None -> Assert.Fail("File not found")
        )

    [<Fact>]
    let ``tryPriceFile returns None for missing file`` () =
        withTempDir (fun _ ->
            let instrument = Instrument.GO
            let result = tryPriceFile instrument
            Assert.True(result.IsNone)
        )

    // Similar tests can be added for tryVolsFile, trySmileFile, tryFutExpFile, tryOptExpFile

    // TODO: Add tests for getfixing and getfixings (requires Deedle setup/mocking)

    // Property-based test for getfixing
    [<Property>]
    let ``getfixing returns expected value for valid input`` (date: DateTime, value: float) =
        withTempDir (fun tempDir ->
            // Arrange: Create a dummy fixing file
            let instrument = Instrument.BRT
            let fileName = $"{instrument}_Fixing.csv"
            let content = $"{date:yyyyMMdd},{value}"
            createDummyCsvFile tempDir fileName content |> ignore

            // Act: Call getfixing
            let result = getfixing instrument date

            // Assert: Verify the result matches the expected value
            match result with
            | Some v -> v = value
            | None -> false // Should not return None for valid input
        )

    // Property-based test for getfixings
    [<Property>]
    let ``getfixings returns expected series for valid input`` (datesAndValues: (DateTime * float) list) =
        withTempDir (fun tempDir ->
            // Arrange: Create a dummy fixings file
            let instrument = Instrument.BRT
            let fileName = $"{instrument}_Fixings.csv"
            let content =
                datesAndValues
                |> List.map (fun (date, value) -> $"{date:yyyyMMdd},{value}")
                |> String.concat "\n"
            createDummyCsvFile tempDir fileName content |> ignore

            // Act: Call getfixings
            let result = getfixings instrument

            // Assert: Verify the result matches the expected series
            let expected =
                datesAndValues
                |> List.map (fun (date, value) -> date, value)
                |> Map.ofList

            result = expected
        )

