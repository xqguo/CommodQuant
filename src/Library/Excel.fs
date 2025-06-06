module Excel

open System.IO
open NetOffice

NetOffice.Settings.Default.MessageFilter.Enabled <- true
NetOffice.Settings.Default.MessageFilter.RetryMode <- RetryMessageFilterMode.Delayed

/// <summary>
/// Helper function to represent string or floating point cell content as a string.
/// If the cell value is not a string or double, an empty string is returned.
/// </summary>
/// <param name="range">The Excel cell range.</param>
/// <returns>The string representation of the cell content.</returns>
let cellContent (range: ExcelApi.Range) =
    match range.Value2 with
    | :? string as _string -> _string
    | :? double as _double -> sprintf "%f" _double
    | _ -> ""

/// <summary>
/// Helper function to return cell content as float if possible, if not as 0.0.
/// </summary>
/// <param name="range">The Excel cell range.</param>
/// <returns>The double representation of the cell content, or 0.0 if conversion is not possible.</returns>
let cellDouble (range: ExcelApi.Range) =
    match range.Value2 with
    | :? double as _double -> _double
    | _ -> 0.0

/// <summary>
/// Returns the specified worksheet range as a sequence of individual cell ranges.
/// The iteration is row by row, then column by column.
/// </summary>
/// <param name="range">The Excel range to convert to a sequence.</param>
/// <returns>A sequence of ExcelApi.Range objects, each representing a single cell.</returns>
let toSeq (range: ExcelApi.Range) =
    seq {
        for r in 1 .. range.Rows.Count do
            for c in 1 .. range.Columns.Count do
                let cell = range.Item(r, c)
                yield cell
    }

/// <summary>
/// Returns the specified worksheet range as a sequence of individual cell ranges,
/// together with a 1-based row-index and column-index for each cell.
/// </summary>
/// <param name="range">The Excel range to convert to a sequence.</param>
/// <returns>A sequence of tuples (row, column, cell), where row and column are 1-based indices.</returns>
let toSeqrc (range: ExcelApi.Range) =
    seq {
        for r in 1 .. range.Rows.Count do
            for c in 1 .. range.Columns.Count do
                let cell = range.Item(r, c)
                yield r, c, cell
    }

/// <summary>
/// Takes a sequence of individual cell-ranges and returns an Excel range representation of the cells.
/// This uses Excel's 'union' representation (e.g., "R1C1,R2C1,R5C4").
/// </summary>
/// <param name="workSheet">The worksheet to which the ranges belong.</param>
/// <param name="rangeSeq">A sequence of ExcelApi.Range objects, each representing a single cell.</param>
/// <returns>An ExcelApi.Range object representing the union of the input cell ranges.</returns>
let toRange (workSheet: ExcelApi.Worksheet) (rangeSeq: seq<ExcelApi.Range>) =
    let csvSeq sequence =
        let result = sequence |> Seq.fold (fun acc x -> acc + x + ",") ""
        result.Remove(result.Length - 1)

    let rangeName = rangeSeq |> Seq.map (fun cell -> cell.Address) |> csvSeq
    workSheet.Range(rangeName)

/// <summary>
/// Takes a function and an Excel range, and returns a sequence of the results
/// of applying the function to each individual cell.
/// </summary>
/// <typeparam name="'T">The return type of the mapping function.</typeparam>
/// <param name="f">The function to apply to each cell.</param>
/// <param name="range">The Excel range.</param>
/// <returns>A sequence of results of type 'T.</returns>
let map (f: ExcelApi.Range -> 'T) (range: ExcelApi.Range) = range |> toSeq |> Seq.map f

/// <summary>
/// Takes a function and an Excel range, and returns a sequence of the results
/// of applying the function to each individual cell.
/// The function is provided with the 1-based row-index and column-index for each cell.
/// </summary>
/// <typeparam name="'T">The return type of the mapping function.</typeparam>
/// <param name="f">The function to apply to each cell, taking row index, column index, and cell.</param>
/// <param name="range">The Excel range.</param>
/// <returns>A sequence of results of type 'T.</returns>
let maprc (f: int -> int -> ExcelApi.Range -> 'T) (range: ExcelApi.Range) =
    range
    |> toSeqrc
    |> Seq.map (fun item ->
        match item with
        | (r, c, cell) -> f r c cell)

/// <summary>
/// Takes a function and an Excel range, and applies the function to each individual cell for side effects.
/// </summary>
/// <param name="f">The function to apply to each cell.</param>
/// <param name="range">The Excel range.</param>
let iter (f: ExcelApi.Range -> unit) (range: ExcelApi.Range) =
    range |> toSeq |> Seq.iter (fun cell -> f cell)

/// <summary>
/// Takes a function and an Excel range, and applies the function to each individual cell for side effects.
/// The function is provided with the 1-based row-index and column-index for each cell.
/// </summary>
/// <param name="f">The function to apply to each cell, taking row index, column index, and cell.</param>
/// <param name="range">The Excel range.</param>
let iterrc (f: int -> int -> ExcelApi.Range -> unit) (range: ExcelApi.Range) =
    range
    |> toSeqrc
    |> Seq.iter (fun item ->
        match item with
        | (r, c, cell) -> f r c cell)

/// <summary>
/// Takes a predicate function and an Excel range, and returns a sequence of individual cell ranges
/// where the result of applying the function to the cell is true.
/// </summary>
/// <param name="f">The predicate function to apply to each cell.</param>
/// <param name="range">The Excel range.</param>
/// <returns>A sequence of ExcelApi.Range objects that satisfy the predicate.</returns>
let filter (f: ExcelApi.Range -> bool) (range: ExcelApi.Range) =
    range |> toSeq |> Seq.filter (fun cell -> f cell)

/// <summary>
/// Tries to connect to an existing, active Excel application instance.
/// </summary>
/// <returns>
/// A Result containing the ExcelApi.Application instance if successful,
/// or an error message string if it cannot get an existing instance.
/// </returns>
let tryExcel () =
    try
        //try to get current excel
        let excelApp = ExcelApi.Application.GetActiveInstance()
        excelApp.Visible <- true
        Ok excelApp
    with _ ->
        Error "Cannot get existing excel instance"

/// <summary>
/// Tries to find an existing workbook by its filename within a given Excel application instance.
/// </summary>
/// <param name="filename">The name of the workbook file (e.g., "MyWorkbook.xlsx").</param>
/// <param name="excel">The Excel application instance to search within.</param>
/// <returns>
/// A Result containing the ExcelApi.Workbook instance if found,
/// or an error message string if the workbook cannot be found.
/// </returns>
let tryBook filename (excel: ExcelApi.Application) =
    try
        Ok(excel.Workbooks.[filename])
    with _ ->
        Error("Cannot find workbook  " + filename)

/// <summary>
/// Tries to open a workbook from a specified directory and filename using a given Excel application instance.
/// </summary>
/// <param name="workbookDir">The directory where the workbook is located.</param>
/// <param name="filename">The name of the workbook file.</param>
/// <param name="excel">The Excel application instance to use for opening the workbook.</param>
/// <returns>
/// A Result containing the ExcelApi.Workbook instance if successfully opened,
/// or an error message string if the file doesn't exist or cannot be opened.
/// </returns>
let tryOpenBook workbookDir filename (excel: ExcelApi.Application) =
    let f = Path.Combine(workbookDir, filename)

    try
        //check path and file exists
        if (File.Exists(f)) then
            Ok(excel.Workbooks.Open f)
        else
            Error("Cannot find workbook  " + f)
    with _ ->
        Error("Cannot open workbook  " + f)

/// <summary>
/// Tries to get a reference to a worksheet by its name from a given workbook.
/// </summary>
/// <param name="sheet">The name of the worksheet.</param>
/// <param name="book">The workbook to search within.</param>
/// <returns>
/// A Result containing the ExcelApi.Worksheet instance if found,
/// or an error message string if the sheet cannot be found.
/// </returns>
let trySheet sheet (book: ExcelApi.Workbook) =
    try
        Ok(book.Sheets.[sheet] :?> ExcelApi.Worksheet)
    with _ ->
        Error("Cannot find sheet " + sheet)

/// <summary>
/// Tries to get a reference to a named range from a given worksheet.
/// </summary>
/// <param name="rangeName">The name of the range (e.g., "A1:B5" or a defined name).</param>
/// <param name="sheet">The worksheet to search within.</param>
/// <returns>
/// A Result containing the ExcelApi.Range instance if found,
/// or an error message string if the range cannot be found.
/// </returns>
let tryRange rangeName (sheet: ExcelApi.Worksheet) =
    try
        Ok(sheet.Range(rangeName))
    with _ ->
        Error("Cannot find range " + rangeName)

/// <summary>
/// A composite function that attempts to get an Excel range by connecting to Excel,
/// finding a workbook, finding a sheet, and then finding the range.
/// </summary>
/// <param name="file">The filename of the workbook.</param>
/// <param name="sheet">The name of the worksheet.</param>
/// <param name="range">The name of the range.</param>
/// <returns>
/// A Result containing the ExcelApi.Range if all steps are successful,
/// or an error message from the first failed step.
/// </returns>
let tryRangeFull file sheet range =
    tryExcel ()
    |> Result.bind (tryBook file)
    |> Result.bind (trySheet sheet)
    |> Result.bind (tryRange range)

/// <summary>
/// Attempts to kill all running excel.exe processes using taskkill.exe.
/// This is a forceful way to close Excel instances.
/// </summary>
let killExcel () =
    let info = new System.Diagnostics.ProcessStartInfo()
    info.UseShellExecute <- false
    info.FileName <- "taskkill.exe"
    info.Arguments <- "/F /IM excel.exe"
    let p = System.Diagnostics.Process.Start(info)
    p.Close()

/// <summary>
/// Tries to start a new Excel application instance.
/// Note: Exiting the GUI of an Excel instance started this way might leave a hidden process.
/// </summary>
/// <returns>
/// A Result containing the new ExcelApi.Application instance if successful,
/// or an error message string if Excel cannot be started.
/// </returns>
let tryStartExcel () =
    try
        let excel = new ExcelApi.Application()
        Ok excel
    with _ ->
        Error "Cannot start excel instance"

/// <summary>
/// Quits a given Excel application instance and disposes of COM proxies.
/// </summary>
/// <param name="excel">The ExcelApi.Application instance to quit.</param>
let quitExcel (excel: ExcelApi.Application) =
    excel.Quit()
    excel.Dispose()
    NetOffice.Core.Default.DisposeAllCOMProxies()

/// <summary>
/// Takes a 2D continuous Excel range and dumps its cell content (as strings) into a CSV file.
/// </summary>
/// <param name="f">The path to the CSV file to be created/overwritten.</param>
/// <param name="range">The ExcelApi.Range to export.</param>
let toCSV f (range: ExcelApi.Range) =
    //com call sometimes choke.
    let r = range.Rows.Count
    let data = map cellContent range |> Seq.splitInto r

    let content =
        data
        |> Seq.map (fun s ->
            let csvRow = Array.fold (fun acc x -> acc + x + ",") "" s
            csvRow.TrimEnd ',')
    //if File.Exists f then File.Delete f
    //let tmp = f+".tmp"
    //printfn "writing to %s" tmp
    File.WriteAllLines(f, content)
//printfn "moving to %s" f
//File.Move(tmp, f)
