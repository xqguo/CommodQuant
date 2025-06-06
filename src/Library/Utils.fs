namespace Commod

open System.Globalization
open System
open System.IO
open System.Threading.Tasks

[<AutoOpen>]
module Utils =
    open FSharp.Data
    // open FSharpx.Control
    open Microsoft.FSharp.Reflection

    /// <summary>Converts a union case to its string representation.</summary>
    let toString (x: 'a) =
        let (case, _) = FSharpValue.GetUnionFields(x, typeof<'a>)
        case.Name

    /// <summary>Converts a string to a union case of type 'a.</summary>
    let fromString<'a> (s: string) =
        match FSharpType.GetUnionCases typeof<'a> |> Array.filter (fun case -> case.Name = s) with
        | [| case |] -> Some(FSharpValue.MakeUnion(case, [||]) :?> 'a)
        | _ -> None

    // let culture = CultureInfo("en-GB")
    /// <summary>The culture used for parsing and formatting.</summary>
    let culture =
        System.Globalization.CultureInfo.InvariantCulture.Clone() :?> CultureInfo

    culture.Calendar.TwoDigitYearMax <- 2080
    /// <summary>Alias for Result.Error to avoid name clash with FSharpx.</summary>
    let Error = Result.Error //avoid name clash with FSharpx
    /// <summary>Concatenates two paths.</summary>
    let (+/) path1 path2 = Path.Combine(path1, path2)

    /// <summary>Tries to parse a string using the provided tryParse function.</summary>
    let tryParseWith tryParseFunc x =
        match x with
        | ""
        | "NaN"
        | "N/A" -> None
        | _ ->
            match tryParseFunc x with
            | true, v -> Some v
            | false, _ -> None

    /// <summary>Tries to parse a string as a DateTime.</summary>
    let parseDate = tryParseWith System.DateTime.TryParse
    /// <summary>Tries to parse a string as an Int32.</summary>
    let parseInt = tryParseWith System.Int32.TryParse
    /// <summary>Tries to parse a string as a Single.</summary>
    let parseSingle = tryParseWith System.Single.TryParse
    /// <summary>Tries to parse a string as a Double.</summary>
    let parseDouble = tryParseWith System.Double.TryParse

    /// <summary>Tries to parse a string as a Double and formats it to 10 significant figures.</summary>
    let parseDouble10 =
        tryParseWith System.Double.TryParse >> Option.map (sprintf "%.10g")

    /// <summary>Parses a string in MM/dd/yy format to a DateTime.</summary>
    let parseMMddyy s =
        DateTime.ParseExact(s, "MM/dd/yy", culture)
    //DateTime.Parse("12/1/2021", CultureInfo.InvariantCulture, DateTimeStyles.None)
    //DateTime.Parse("13/1/2021", culture)
    //DateTime.Parse("Sep21", culture)
    //DateTime.Parse("2011-02", culture)

    /// <summary>Converts a string to uppercase and removes all non-alphanumeric characters.</summary>
    let datestr (str: string) =
        (str.ToUpper()) |> String.filter Char.IsLetterOrDigit //ignores separators like - /

    //let parseDateExact format str=
    //    try
    //        Some( System.DateTime.ParseExact( (datestr str), format, culture) )
    //    with
    //    | _ -> None

    /// <summary>Tries to parse a string as a DateTime using the exact format string.</summary>
    let parseDateExact (format: string) str =
        let (s, d) =
            System.DateTime.TryParseExact((datestr str), format, culture, DateTimeStyles.None)

        if s then Some d else None

    // active patterns for try-parsing strings
    /// <summary>Active pattern for parsing strings in yyyyMMdd format.</summary>
    let (|YYYYMMDD|_|) = parseDateExact "yyyyMMdd"
    /// <summary>Active pattern for parsing strings in MMMyy format.</summary>
    let (|MMMYY|_|) = parseDateExact "MMMyy"
    /// <summary>Active pattern for parsing strings in ddMMMyy format.</summary>
    let (|DDMMMYY|_|) = parseDateExact "ddMMMyy"
    /// <summary>Active pattern for parsing strings in MMddyy format.</summary>
    let (|MMDDYY|_|) = parseDateExact "MMddyy"
    /// <summary>Active pattern for parsing strings as a DateTime.</summary>
    let (|Date|_|) = parseDate
    /// <summary>Active pattern for parsing strings as an Int32.</summary>
    let (|Int|_|) = parseInt
    /// <summary>Active pattern for parsing strings as a Single.</summary>
    let (|Single|_|) = parseSingle
    /// <summary>Active pattern for parsing strings as a Double formatted to 10 significant figures.</summary>
    let (|Double10|_|) = parseDouble10
    /// <summary>Active pattern for parsing strings as a Double.</summary>
    let (|Double|_|) = parseDouble

    type Async with

        /// <summary>Awaits a plain Task without returning a result.</summary>
        static member AwaitPlainTask(task: Task) =
            task.ContinueWith(ignore) |> Async.AwaitTask

    /// <summary>The default buffer size for file operations.</summary>
    [<Literal>]
    let DEFAULT_BUFFER_SIZE = 4096

    /// <summary>Checks if a file exists and returns its path if it does.</summary>
    let tryFile f = if File.Exists(f) then Some f else None

    /// <summary>Asynchronously copies a file from source to destination.</summary>
    let copyToAsync source dest =
        async {
            use sourceFile =
                new FileStream(source, FileMode.Open, FileAccess.Read, FileShare.Read, DEFAULT_BUFFER_SIZE, true)

            use destFile =
                new FileStream(dest, FileMode.OpenOrCreate, FileAccess.Write, FileShare.None, DEFAULT_BUFFER_SIZE, true)

            do! sourceFile.CopyToAsync(destFile) |> Async.AwaitPlainTask
        }

    // let writeTextAsync f str = File.AsyncWriteAllText(f, str)

    // let writeLinesAsync (f: string) l = File.AsyncWriteAllLines(f, l)

    /// <summary>Reads all lines from a file without locking it.</summary>
    let readLines (path: string) =
        seq {
            use fs = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
            use sr = new StreamReader(fs)

            while not sr.EndOfStream do
                yield sr.ReadLine()
        }

    /// <summary>Writes a sequence of strings to a file.</summary>
    let writeFile f lines = File.WriteAllLines(f, lines)

    /// <summary>Moves a file to the specified output directory.</summary>
    let moveFile outputdir (file: string) =
        let f = FileInfo(file)

        try
            if outputdir <> f.DirectoryName then
                let dstfile = outputdir +/ f.Name

                if File.Exists(dstfile) then
                    File.Delete(dstfile)

                f.MoveTo(dstfile)
                dstfile
            else
                f.FullName
        with e ->
            failwithf "cannot move %s, %O" file e

    /// <summary>Asynchronously updates a file if the source file is newer.</summary>
    let updatefileAsync (file: FileInfo) (destFile: FileInfo) =
        async {
            let destname = destFile.FullName
            let backup = destname + "~"

            let rb () =
                try
                    if File.Exists backup then
                        File.Delete backup
                with _ ->
                    printfn "Cannot remove backup %s " backup

            do rb ()

            if destFile.Exists && file.LastWriteTime > destFile.LastWriteTime then
                try
                    File.Move(destname, backup)
                with _ ->
                    printfn "Cannot backup %s " destname

            do rb ()

            if not destFile.Exists then
                printfn "updating %s" destFile.FullName
                do! copyToAsync file.FullName destFile.FullName
                destFile.LastWriteTimeUtc <- file.LastWriteTimeUtc
        }

    /// <summary>Asynchronously updates a directory by copying files from a source path to a destination path.</summary>
    let updatedirAsync sourcePath destinationPath =
        //Create all of the directories
        for dirPath in Directory.GetDirectories(sourcePath, "*", SearchOption.AllDirectories) do
            Directory.CreateDirectory(dirPath.Replace(sourcePath, destinationPath))
            |> ignore

        //Copy all the files & Replaces any files with the same name, except some files
        Directory.GetFiles(sourcePath, "*.*", SearchOption.AllDirectories)
        //|> Seq.filter( fun x -> not <| x.Contains(@"\csv\")  && not <| x.Contains("currentuser.txt"))
        |> Seq.filter (fun x -> not <| x.Contains("currentuser.txt"))
        |> Seq.map (fun newPath ->
            let file = FileInfo(newPath)
            let destFile = FileInfo(newPath.Replace(sourcePath, destinationPath))
            updatefileAsync file destFile)
        |> Async.Parallel
        |> Async.Ignore

    /// <summary>Reads price data from a CSV file into a sequence of (string, float) tuples.</summary>
    let getPrice (f: string) =
        use s = new FileStream(f, FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
        use fs = new StreamReader(s)
        let tsk = fs.ReadToEndAsync()
        let price = CsvFile.Parse(tsk.Result)

        match price.NumberOfColumns with
        | 2 ->
            seq {
                for row in price.Rows do
                    //validate price is a number, skip empty pillar
                    match row.[0], row.[1] with
                    | "", _ -> ignore ()
                    | p, Double v -> yield (p.ToUpper(), v)
                    | p, v -> failwithf "Wrong data format in file %s, pillar %s, value %s!" f p v
            }
        | _ ->
            failwithf "Input data should have exactly 2 columns: %s" f
            Seq.empty

[<AutoOpen>]
module DateUtils =
    open System.Text.RegularExpressions

    /// <summary>Generates an infinite sequence of dates by adding or subtracting days from a starting date.</summary>
    let generateDay (d: DateTime) (dir: bool) =
        let addday i =
            match dir with
            | true -> d.AddDays i
            | false -> d.AddDays(-i)

        Seq.initInfinite float // 0.0 , 1.0, ...
        |> Seq.map addday

    /// <summary>Generates an infinite sequence of dates by adding or subtracting months from a starting date.</summary>
    let generateMonth (d: DateTime) (dir: bool) = //not recurves and therefore it keeps roll date
        let nums = Seq.initInfinite int // 0.0 , 1.0, ...

        match dir with
        | true -> Seq.map d.AddMonths nums
        | false -> Seq.map ((-) 0 >> d.AddMonths) nums // equiv to: Seq.map( fun i -> d.AddMonths -i ) nums

    /// <summary>Generates an array of dates within a given range.</summary>
    let dateRange (startDate: DateTime) endDate =
        let n = abs ((endDate - startDate).Days) + 1

        if startDate <= endDate then
            Array.init n startDate.AddDays
        else
            Array.init n (fun i -> startDate.AddDays(-i))

    /// <summary>Checks if a given date is a holiday.</summary>
    let isHoliday (hol: Set<DateTime>) d = hol.Contains d

    /// <summary>Checks if a given date is a weekend.</summary>
    let isWeekend (d: DateTime) =
        d.DayOfWeek = DayOfWeek.Saturday || d.DayOfWeek = DayOfWeek.Sunday

    /// <summary>Checks if a given date is a business day (not a holiday or weekend).</summary>
    let isBusinessDay hol d = not (isHoliday hol d || isWeekend d)

    /// <summary>Generates an infinite sequence of business days by adding or subtracting days from a starting date.</summary>
    let generateBusinessDay hol (d: DateTime) (dir: bool) =
        generateDay d dir |> Seq.filter (isBusinessDay hol)

    /// <summary>Gets the previous business day.</summary>
    let prevBusinessDay hol dt =
        generateBusinessDay hol dt false |> Seq.head

    /// <summary>Gets the next business day.</summary>
    let nextBusinessDay hol dt =
        generateBusinessDay hol dt true |> Seq.head

    /// <summary>Adds or subtracts a specified number of business days to a date. 0 bd means next bd.</summary>
    let addBusinessDay n hol (dt: DateTime) =
        match n with
        | x when x > 0 -> generateBusinessDay hol (dt.AddDays 1.) true |> Seq.skip (abs x - 1) |> Seq.head
        | x when x < 0 ->
            generateBusinessDay hol (dt.AddDays -1.) false
            |> Seq.skip (abs x - 1)
            |> Seq.head
        | _ -> nextBusinessDay hol dt //1 case

    /// <summary>Gets the next business day unless it's in a different month, in which case it gets the previous business day.</summary>
    let modifiedFollowing hol dt =
        let d0 = nextBusinessDay hol dt
        if d0.Month <> dt.Month then prevBusinessDay hol dt else d0

    /// <summary>Generates an array of business days within a given date range.</summary>
    let bdRange hol d1 d2 =
        dateRange d1 d2 |> Array.filter (isBusinessDay hol)

    /// <summary>Calculates the number of business days between two dates (exclusive of d1).</summary>
    let numBizdays hol (d1: DateTime) d2 = //exclusive of d1
        if d1 > d2 then
            invalidArg "d1 d2" "d1 should not be greater than d2"

        if d1.Date <> d1 then
            invalidArg "d1" "d1 should not have time fractions"

        if d2.Date <> d2 then
            invalidArg "d1" "d2 should not have time fractions"

        if d1 = d2 then
            0
        else
            bdRange hol (d1.AddDays 1.0) d2 |> Array.length

    /// <summary>Calculates the number of business years between two dates.</summary>
    let getBizYears hol d1 d2 = float (numBizdays hol d1 d2) / 252.

    /// <summary>Calculates the number of business years between two dates, assuming no holidays.</summary>
    let getBizYears' d1 d2 = getBizYears Set.empty d1 d2

    /// <summary>
    /// Goes to the specified day of the week.
    /// Monday is 1, Sunday is 7.
    /// Positive n for next occurrence, negative n for previous.
    /// Example: 3 for current or next Wednesday, -3 for current or previous Wednesday.
    /// Use -7 for previous Sunday.
    /// </summary>
    /// <param name="d0">The starting date.</param>
    /// <param name="n">The target day of the week (1-7), positive or negative for direction.</param>
    let dayOfWeek (d0: DateTime) n =
        let t =
            if n >= 0 then
                (-(int d0.DayOfWeek) + n + 7) % 7
            else
                (-(int d0.DayOfWeek) - (n % 7) - 7) % 7

        d0.AddDays(float t)

    /// <summary>
    /// Goes to the specified month of the year.
    /// Positive n for next occurrence, negative n for previous.
    /// </summary>
    /// <param name="d0">The starting date.</param>
    /// <param name="n">The target month (1-12), positive or negative for direction. Must be between -12 and 12.</param>
    let monthOfYear (d0: DateTime) n =
        if abs (n) > 12 then
            invalidArg "n" " n should be between -12 and 12."

        match n with
        | n when abs (n) = d0.Month -> d0
        | n when n >= 0 -> (n + 12 - d0.Month) % 12 |> d0.AddMonths
        | _ -> -(d0.Month + n + 12) % 12 |> d0.AddMonths

    /// <summary>
    /// Computes the beginning of the current calendar period.
    /// </summary>
    /// <param name="d0">The starting date.</param>
    /// <param name="n">The period number of months (0 or 1 for month, 3 for quarter, 6 for half-year, 12 for year).</param>
    let beginOfCalendarPeriod (d0: DateTime) (n: int) =
        match n with
        | 1
        | 0 -> DateTime(d0.Year, d0.Month, 1)
        | 3
        | 6
        | 12 ->
            let adj = (d0.Month - 1) % n
            DateTime(d0.Year, d0.Month - adj, 1)
        | _ -> invalidArg "n" "number of month should be in (1,3,6,12)"

    /// <summary>
    /// Adjusts a date based on a string of actions.
    /// Each token in the string is composed of:
    /// 1. Optional +/- for direction.
    /// 2. Optional number for repeats.
    /// 3. An action character (case sensitive).
    /// Throws an error if the input string is not fully decomposable into tokens.
    /// Example: "2b-10d3Q+2ye" or "+3m+0F"
    ///
    /// Action characters:
    /// <list type="bullet">
    /// <item><term>d</term><description>Calendar days (ignores holidays).</description></item>
    /// <item><term>w</term><description>Calendar weeks (ignores holidays).</description></item>
    /// <item><term>m</term><description>Calendar months (ignores holidays).</description></item>
    /// <item><term>y</term><description>Calendar years (ignores holidays).</description></item>
    /// <item><term>b</term><description>Business days.</description></item>
    /// <item><term>n</term><description>Next business day (ignores repeat and direction).</description></item>
    /// <item><term>p</term><description>Previous business day (ignores repeat and direction).</description></item>
    /// <item><term>f</term><description>Modified following: next business day unless it crosses a month boundary, then previous business day (ignores repeat and direction).</description></item>
    /// <item><term>a</term><description>Beginning of the month (ignores repeat and direction).</description></item>
    /// <item><term>e</term><description>End of the month (ignores repeat and direction).</description></item>
    /// <item><term>A</term><description>Beginning of the current calendar year (ignores repeat and direction).</description></item>
    /// <item><term>Z</term><description>End of the current calendar year (ignores repeat and direction).</description></item>
    /// <item><term>Q</term><description>Beginning of the current calendar quarter (ignores repeat and direction).</description></item>
    /// <item><term>H</term><description>Beginning of the current calendar half-year (ignores repeat and direction).</description></item>
    /// <item><term>W</term><description>Day of the week (1 for Monday, 7 for Sunday). Number specifies the day. Example: "3W" for Wednesday, "-7W" for previous Sunday.</description></item>
    /// <item><term>C</term><description>Calendar month of the year (1 for January). Number specifies the month. Example: "1C" for next January, "-1C" for previous January. Keeps the day of the month.</description></item>
    /// </list>
    /// </summary>
    /// <param name="holidays">A set of holiday dates.</param>
    /// <param name="str">The action string.</param>
    /// <param name="d0">The starting date.</param>
    let dateAdjust (holidays: Set<DateTime>) (str: string) (d0: DateTime) =
        let pattern = "([+-]?)(\d*)([a-zA-Z])"
        let patternfull = "^(([+-]?)(\d*)([a-zA-Z]))*$"

        if not (Regex.IsMatch(str, patternfull)) then
            failwithf "Invalid input: %s" str

        Regex.Matches(str, pattern)
        |> Seq.cast
        |> Seq.fold
            (fun (d: DateTime) (m: Match) ->
                let direction =
                    match Regex.Replace(m.Value, pattern, "$1") with
                    | "-" -> -1
                    | _ -> 1

                let b, i = Int32.TryParse(Regex.Replace(m.Value, pattern, "$2"))
                let repeat = if b then i else 0

                match System.Text.RegularExpressions.Regex.Replace(m.Value, pattern, "$3") with
                | "d" -> d.AddDays(float (direction * repeat)) //calendar days ignore holiday
                | "w" -> d.AddDays(float (direction * repeat * 7)) //calendar weeks ignore holiday
                | "m" -> d.AddMonths(direction * repeat) //calendar month ignore holiday
                | "y" -> d.AddYears(direction * repeat) //calendar year ignore holiday
                | "b" -> addBusinessDay (direction * repeat) holidays d //move by business day
                //the followings are adjusts that ignores direction and repeat.
                | "n" -> nextBusinessDay holidays d //next if not bd
                | "p" -> prevBusinessDay holidays d //previous if non bd
                | "f" -> modifiedFollowing holidays d //next if not bd unless cross month then previous
                | "a" -> DateTime(d.Year, d.Month, 1) //beginning of month ignores repeat and direction
                | "e" -> DateTime(d.Year, d.Month, DateTime.DaysInMonth(d.Year, d.Month)) //end of month ignores repeat and direction
                | "A" -> DateTime(d.Year, 1, 1)
                | "Z" -> DateTime(d.Year, 12, 31) //end of yr, and use A for beginning of yr
                | "Q" -> beginOfCalendarPeriod d 3
                | "H" -> beginOfCalendarPeriod d 6
                //number is used to define days of week instead of repeat, direction is kept
                | "W" -> dayOfWeek d (direction * repeat) //3W to go to wedneday, -7W to goes to previous Sunday
                | "C" -> monthOfYear d (direction * repeat) //1C to go to next Jan, -1C to goes to previous Jan
                | x -> failwith ("unknown str:" + x))
            d0

    /// <summary>Shortcut for `dateAdjust` with no holidays (still checks for weekends).</summary>
    let dateAdjust' = dateAdjust Set.empty //shortcut for no holiday checking, still check for weekends
    // create an active pattern to match time tenor
    /// <summary>Active pattern for matching time tenors like "ON", "TN", "SPOT", "WEEKEND", "BOM*", etc.</summary>
    let (|Tenor|_|) input =
        let tenors = [ "ON"; "TN"; "SN"; "SPOT"; "TODAY"; "DAYAHEAD"; "WEEKEND" ] |> set

        if tenors.Contains input || input.StartsWith "BOM" then
            Some input
        else
            None

    /// <summary>Active pattern for matching periods like "3D", "2W", "1M", "1Y".</summary>
    let (|Period|_|) input =
        let m = Regex.Match(input, "^(\d+)(D|W|M|Y)$")

        if (m.Success) then
            Some(int (m.Groups.[1].Value), m.Groups.[2].Value)
        else
            None

    /// <summary>Active pattern for matching FRA periods like "3M6M", "1Y2Y". Captures the second period part.</summary>
    let (|FraPeriod|_|) input = //9m-12m etc
        let m = Regex.Match(input, "^(\d+)(D|W|M|Y)(\d+)(D|W|M|Y)$")

        if (m.Success) then
            Some(int (m.Groups.[3].Value), m.Groups.[4].Value)
        else
            None

    /// <summary>
    /// Converts a pillar string to a DateTime.
    /// Recognized formats:
    /// <list type="bullet">
    /// <item><description>yyyyMMdd (e.g., "20231225")</description></item>
    /// <item><description>MMMyy (e.g., "DEC23")</description></item>
    /// <item><description>ddMMMyy (e.g., "25DEC23")</description></item>
    /// <item><description>MMddyy (e.g., "122523")</description></item>
    /// <item><description>Common date formats parsable by DateTime.TryParse.</description></item>
    /// <item><description>Periods like "3D", "2W", "1M", "1Y" (relative to today).</description></item>
    /// <item><description>FRA periods like "3M6M", "1Y2Y" (uses the second period part, relative to today).</description></item>
    /// <item><description>Tenors: "ON", "DAYAHEAD" (today + 1 day), "TN", "SPOT" (today + 2 days), "SN" (today + 3 days), "TODAY", "WEEKEND" (next Saturday), "BOM*" (end of current month).</description></item>
    /// </list>
    /// If the format is not recognized, it defaults to DateTime.Today.
    /// </summary>
    let pillarToDate (dStr: string) =
        match datestr dStr with
        | YYYYMMDD d -> d
        | MMMYY d -> d
        | DDMMMYY d -> d
        | MMDDYY d -> d
        | Date d -> d
        | Period(n, p)
        | FraPeriod(n, p) ->
            match p with
            | "D" -> DateTime.Today.AddDays(float n)
            | "W" -> DateTime.Today.AddDays(float n * 7.0)
            | "M" -> DateTime.Today.AddMonths(n)
            | "Y" -> DateTime.Today.AddYears(n)
            | _ -> invalidOp "Unknown period type, expect D/W/M/Y"
        | Tenor d ->
            match d with
            | "ON"
            | "DAYAHEAD" -> DateTime.Today.AddDays(1.0)
            | "TN"
            | "SPOT" -> DateTime.Today.AddDays(2.0)
            | "SN" -> DateTime.Today.AddDays(3.0)
            | "TODAY" -> DateTime.Today
            | x when x.StartsWith "BOM" -> DateTime.Today |> dateAdjust' "e"
            | "WEEKEND" -> DateTime.Today |> dateAdjust' "7W"
            | _ -> invalidOp (sprintf "Unknown tenor string %s" d)
        | _ ->
            //printfn "Warning: Unknown pillar %s, using today." dStr
            DateTime.Today

    /// <summary>
    /// Generates a calendar month schedule between two dates.
    /// Allows for broken periods at both ends (d1 to month end, then whole months, then month start to d2).
    /// </summary>
    /// <param name="d1">The start date.</param>
    /// <param name="d2">The end date.</param>
    let generateCalMonthSchedule d1 d2 =
        if d1 > d2 then
            failwith "d1 > d2, invalid inputs"

        generateMonth (dateAdjust' "a" d1) true
        |> Seq.takeWhile (fun d -> d <= d2)
        |> Array.ofSeq
        |> Array.map (fun ms ->
            let me = dateAdjust' "e" ms
            max ms d1, min me d2)

    /// <summary>
    /// Gets the start and end dates for a period string.
    /// Recognized formats:
    /// <list type="bullet">
    /// <item><description>MMMyy (e.g., "JAN19") - Represents the full month.</description></item>
    /// <item><description>NQYY (e.g., "1Q19") - Represents the Nth quarter of year YY. (e.g. "1Q19" for Q1 2019)</description></item>
    /// <item><description>CALYY (e.g., "CAL19") - Represents the full calendar year YY. (e.g. "CAL19" for 2019)</description></item>
    /// </list>
    /// </summary>
    let getPeriod (str: string) = //get period like Jan19, 1Q19, Cal19 etc
        let str = datestr str
        // create an active pattern to match time tenor
        let (|Quarter|_|) input =
            let m = Regex.Match(input, "^([1-9])Q(\d\d)$") //2Q19

            if (m.Success) then
                Some(int (m.Groups.Item 1).Value, int (m.Groups.Item 2).Value)
            else
                None

        let (|Year|_|) input =
            let m = Regex.Match(input, "^CAL(\d\d)$") //CAL19

            if (m.Success) then
                Some(int (m.Groups.Item 1).Value)
            else
                None

        match str.ToUpper() with
        | MMMYY d -> d, (dateAdjust' "e" d)
        | Quarter(q, y) ->
            let s = DateTime(y + 2000, 1, 1) |> dateAdjust' (sprintf "%im" ((q - 1) * 3))
            s, s |> dateAdjust' "2me"
        | Year y ->
            let s = DateTime(y + 2000, 1, 1)
            s, s |> dateAdjust' "Z"
        | _ -> failwithf "Invalid period %s" str

    /// <summary>Formats a DateTime as an uppercase pillar string (e.g., "DEC-20").</summary>
    let formatPillar (x: DateTime) = x.ToString("MMM-yy").ToUpper() //e.g. DEC-20
