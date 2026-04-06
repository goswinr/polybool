// uses https://github.com/luizbarboza/polyclip-ts/tree/main/test/end-to-end
//
//
#r "C:/Program Files/Rhino 8/System/RhinoCommon.dll"
#r "nuget: Rhino.Scripting.FSharp"
#r "nuget: Newtonsoft.Json"
#r "nuget: ResizeArrayT"
#r "nuget: Fesher"
#r "nuget: Euclid.Rhino"
#r "D:/Git/_Clip_/polybool/fsharp/PolyBool/bin/Debug/netstandard2.0/PolyBool.dll"

//#r "../PolyBool/bin/Debug/netstandard2.0/PolyBool.dll" // relative path fails in Rhino

open System
open System.IO
open Euclid
open Rhino.Scripting
open Rhino.Scripting.FSharp

open ResizeArrayT

type rs = RhinoScriptSyntax

let mutable shiftX = 0.0
let mutable shiftXNext = 0.0
let isRH = Rhino.Runtime.HostUtils.RunningInRhino
let draw layer (p:ResizeArray<float>) =
    if isRH then
        let pl = Polyline2D.createEmpty(p.Count/2)
        let mutable i = 0
        while i < p.LastIndex do
            pl.Points.Add <| Pt(p[i] + shiftX, p[i+1])
            i <- i + 2

        let plr = pl.RhPolyline
        if plr.IsValid then
            plr
            |> rs.AddPolyline
            |> rs.setLayer layer
            // if pl.IsClosed then
            //     rs.AddHatch pl.RhPolylineCurve
            //     |> rs.setLayer lay
        else
            for i, p in Seq.indexed pl.Points do
                rs.AddTextDot($"{i}", p.RhPt)
                |> rs.setLayer layer

        for p in pl.Points do
            shiftXNext <- max shiftXNext p.X

module RhinoDrawing =

    let private sanitizeLayerName (name: string) : string =
        name.Replace("\\", "_").Replace("/", "_").Replace(":", "_")

    let private drawRegion (layer: string) (region: float[]) : unit =
        draw layer (ResizeArray<float>(region))

    let private drawRegions (layerPrefix: string) (regions: float[][]) : unit =
        regions
        |> Array.iteri (fun regionIndex region ->
            drawRegion $"{layerPrefix}::Region{regionIndex + 1}" region)

    let resetLayout () : unit =
        shiftX <- 0.0
        shiftXNext <- 0.0

    let drawTestCase (testName: string) (arguments: float[][][]) (actual: float[][] option) (expected: float[][] option) : unit =
        let hasArguments = arguments |> Array.exists (fun regions -> regions.Length > 0)
        let hasActual = actual |> Option.exists (fun regions -> regions.Length > 0)
        let hasExpected = expected |> Option.exists (fun regions -> regions.Length > 0)

        if isRH && (hasArguments || hasActual || hasExpected) then
            let caseLayer = sanitizeLayerName testName

            arguments
            |> Array.iteri (fun inputIndex regions ->
                drawRegions $"PolyBool::{caseLayer}::Input::{inputIndex + 1}" regions)

            actual
            |> Option.iter (fun regions ->
                drawRegions $"PolyBool::{caseLayer}::Output" regions)

            expected
            |> Option.iter (fun regions ->
                drawRegions $"PolyBool::{caseLayer}::Expected" regions)

            shiftX <- shiftXNext + 200.0

module Types =
    type Point = float * float
    type Edge = Point * Point

    type Operation =
        | Union
        | Intersection
        | Difference
        | Xor

    type TestCase =
        {
            Name: string
            Operation: Operation option
            ArgsPath: string
            ExpectedPath: string
        }

    type ExpectedData =
        {
            Regions: float[][]
            Tolerance: float
            Precision: float option
        }

    type GeometrySummary =
        {
            PointCount: int
            MinX: float option
            MaxX: float option
            MinY: float option
            MaxY: float option
        }

    type TestResult =
        {
            Name: string
            Passed: bool
            Precision: float option
            ExpectedSummary: GeometrySummary option
            ActualSummary: GeometrySummary option
            ErrorMessage: string option
        }

module Config =
    open PolyBool

    let polybool = PolyBool()
    let emptyRegions: float[][] = [||]
    let defaultTolerance = 1e-9

    let endToEndRoot =
        Path.GetFullPath(
            Path.Combine(
                __SOURCE_DIRECTORY__,
                "..",
                "..",
                "..",
                "polyclip-ts",
                "test",
                "end-to-end"
            )
        )

module Geometry =
    open System
    open Types

    let pointCompare ((ax, ay): Point) ((bx, by): Point) : int =
        let xCompare = compare ax bx

        if xCompare <> 0 then
            xCompare
        else
            compare ay by

    let pointsClose (tolerance: float) ((ax, ay): Point) ((bx, by): Point) : bool =
        abs (ax - bx) <= tolerance && abs (ay - by) <= tolerance

    let stripClosingPoint (tolerance: float) (points: Point[]) : Point[] =
        if points.Length > 1 && pointsClose tolerance points.[0] points.[points.Length - 1] then
            points.[0 .. points.Length - 2]
        else
            points

    let quantize (tolerance: float) (value: float) : float =
        if tolerance <= 0.0 then
            value
        else
            Math.Round(value / tolerance)

    let tolerantPointCompare (tolerance: float) (left: Point) (right: Point) : int =
        let leftX, leftY = left
        let rightX, rightY = right

        let xCompare = compare (quantize tolerance leftX) (quantize tolerance rightX)

        if xCompare <> 0 then
            xCompare
        else
            let yCompare = compare (quantize tolerance leftY) (quantize tolerance rightY)

            if yCompare <> 0 then
                yCompare
            else
                pointCompare left right

    let normalizeEdge (tolerance: float) (startPoint: Point) (endPoint: Point) : Edge =
        if tolerantPointCompare tolerance startPoint endPoint <= 0 then
            startPoint, endPoint
        else
            endPoint, startPoint

    let edgeCompare (tolerance: float) ((leftStart, leftEnd): Edge) ((rightStart, rightEnd): Edge) : int =
        let startCompare = tolerantPointCompare tolerance leftStart rightStart

        if startCompare <> 0 then
            startCompare
        else
            tolerantPointCompare tolerance leftEnd rightEnd

    let regionToPoints (tolerance: float) (region: float[]) : Point[] =
        if region.Length % 2 <> 0 then
            failwith $"Expected an even number of values in a flat region but found {region.Length}"

        [| for index in 0 .. 2 .. region.Length - 2 -> region.[index], region.[index + 1] |]
        |> stripClosingPoint tolerance

    let geometryToEdges (tolerance: float) (regions: float[][]) : Edge[] =
        regions
        |> Array.collect (fun region ->
            let points = regionToPoints tolerance region

            if points.Length < 2 then
                [||]
            else
                [|
                    for index in 0 .. points.Length - 1 do
                        let startPoint = points.[index]
                        let endPoint = points.[(index + 1) % points.Length]
                        yield normalizeEdge tolerance startPoint endPoint
                |])
        |> Array.sortWith (edgeCompare tolerance)

    let edgesClose (tolerance: float) ((leftStart, leftEnd): Edge) ((rightStart, rightEnd): Edge) : bool =
        pointsClose tolerance leftStart rightStart && pointsClose tolerance leftEnd rightEnd

    let matchesExpected (tolerance: float) (actual: float[][]) (expected: float[][]) : bool =
        let actualEdges = geometryToEdges tolerance actual
        let expectedEdges = geometryToEdges tolerance expected

        actualEdges.Length = expectedEdges.Length
        && Array.forall2 (edgesClose tolerance) actualEdges expectedEdges

    let summarizeGeometry (regions: float[][]) : GeometrySummary =
        let points =
            regions
            |> Array.collect (fun region ->
                if region.Length % 2 <> 0 then
                    failwith $"Expected an even number of values in a flat region but found {region.Length}"

                [| for index in 0 .. 2 .. region.Length - 2 -> region.[index], region.[index + 1] |])

        if points.Length = 0 then
            {
                PointCount = 0
                MinX = None
                MaxX = None
                MinY = None
                MaxY = None
            }
        else
            let xs = points |> Array.map fst
            let ys = points |> Array.map snd

            {
                PointCount = points.Length
                MinX = Some(Array.min xs)
                MaxX = Some(Array.max xs)
                MinY = Some(Array.min ys)
                MaxY = Some(Array.max ys)
            }

module Parsing =
    open Newtonsoft.Json.Linq
    open Types
    open Config
    open Geometry

    let tryGetProperty (name: string) (token: JToken) : JToken option =
        match token with
        | :? JObject as obj ->
            let mutable value = Unchecked.defaultof<JToken>

            if obj.TryGetValue(name, &value) then
                Some value
            else
                None
        | _ -> None

    let getRequiredProperty (name: string) (token: JToken) : JToken =
        match tryGetProperty name token with
        | Some value -> value
        | None -> failwith $"Expected property '{name}'"

    let parsePoint (token: JToken) : Point =
        match token with
        | :? JArray as values ->
            if values.Count <> 2 then
                failwith $"Expected a 2D coordinate pair but found {values.Count} values"

            values.[0].Value<float>(), values.[1].Value<float>()
        | _ ->
            failwith "Expected a 2D coordinate pair array"

    let flattenPoints (points: Point[]) : float[] =
        points |> Array.collect (fun (x, y) -> [| x; y |])

    let parseRing (token: JToken) : float[] =
        token.Children()
        |> Seq.map parsePoint
        |> Seq.toArray
        |> stripClosingPoint defaultTolerance
        |> flattenPoints

    let parseGeometry (token: JToken) : float[][] =
        let geometryType = token |> getRequiredProperty "type" |> fun value -> value.Value<string>()
        let coordinates = getRequiredProperty "coordinates" token

        match geometryType with
        | "Polygon" ->
            coordinates.Children()
            |> Seq.map parseRing
            |> Seq.filter (fun region -> region.Length > 0)
            |> Array.ofSeq
        | "MultiPolygon" ->
            coordinates.Children()
            |> Seq.collect (fun polygon -> polygon.Children() |> Seq.map parseRing)
            |> Seq.filter (fun region -> region.Length > 0)
            |> Array.ofSeq
        | unsupported ->
            failwith $"Unsupported GeoJSON geometry type '{unsupported}'"

module FixtureIO =
    open System
    open System.IO
    open Newtonsoft.Json.Linq
    open Types
    open Config
    open Parsing

    let loadArguments (path: string) : float[][][] =
        let root = JToken.Parse(File.ReadAllText(path))

        root
        |> getRequiredProperty "features"
        |> fun features ->
            features.Children()
            |> Seq.map (fun feature -> feature |> getRequiredProperty "geometry" |> parseGeometry)
            |> Array.ofSeq

    let loadExpected (path: string) : ExpectedData =
        let root = JToken.Parse(File.ReadAllText(path))
        let expected = root |> getRequiredProperty "geometry" |> parseGeometry

        let precision =
            root
            |> tryGetProperty "properties"
            |> Option.bind (tryGetProperty "options")
            |> Option.bind (tryGetProperty "precision")
            |> Option.map (fun value -> value.Value<float>())

        let tolerance =
            precision
            |> Option.map (max defaultTolerance)
            |> Option.defaultValue defaultTolerance

        {
            Regions = expected
            Tolerance = tolerance
            Precision = precision
        }

    let operationName (operation: Operation) : string =
        match operation with
        | Union -> "union"
        | Intersection -> "intersection"
        | Difference -> "difference"
        | Xor -> "xor"

    let operationsForFile (path: string) : (Operation option * string)[] =
        match Path.GetFileNameWithoutExtension(path).ToLowerInvariant() with
        | "all" ->
            [|
                Some Union, operationName Union
                Some Intersection, operationName Intersection
                Some Xor, operationName Xor
                Some Difference, operationName Difference
            |]
        | "union" -> [| Some Union, "union" |]
        | "intersection" -> [| Some Intersection, "intersection" |]
        | "difference" -> [| Some Difference, "difference" |]
        | "xor" -> [| Some Xor, "xor" |]
        | unknown -> [| None, unknown |]

    let discoverTests () : TestCase[] =
        Directory.GetDirectories(endToEndRoot)
        |> Array.filter (fun path -> not (Path.GetFileName(path).StartsWith(".")))
        |> Array.sort
        |> Array.collect (fun targetDir ->
            let targetName = Path.GetFileName(targetDir)
            let argsPath = Path.Combine(targetDir, "args.geojson")

            Directory.GetFiles(targetDir, "*.geojson")
            |> Array.filter (fun path -> not (String.Equals(Path.GetFileName(path), "args.geojson", StringComparison.OrdinalIgnoreCase)))
            |> Array.sort
            |> Array.collect (fun expectedPath ->
                operationsForFile expectedPath
                |> Array.map (fun (operation, opName) ->
                    {
                        Name = $"{targetName}/{opName}"
                        Operation = operation
                        ArgsPath = argsPath
                        ExpectedPath = expectedPath
                    }))
        )

module Reporting =
    open System.Globalization
    open Types

    let formatNumber (value: float) : string =
        value.ToString("G17", CultureInfo.InvariantCulture)

    let formatOptionalNumber (value: float option) : string =
        value |> Option.map formatNumber |> Option.defaultValue "n/a"

    let formatSummary (label: string) (summary: GeometrySummary) : string =
        //$"{label}: pointcount={summary.PointCount} x=[{formatOptionalNumber summary.MinX}, {formatOptionalNumber summary.MaxX}] y=[{formatOptionalNumber summary.MinY}, {formatOptionalNumber summary.MaxY}]"
        $"{label}: {summary.PointCount}"

    let printFailure (result: TestResult) : unit =
        let precisionText =
            match result.Precision with
            | Some precision -> $" precision={formatNumber precision}"
            | None -> ""

        let expectedText =
            result.ExpectedSummary
            |> Option.map (formatSummary "expected")
            |> Option.defaultValue "expected: unavailable"

        let actualText =
            result.ActualSummary
            |> Option.map (formatSummary "actual")
            |> Option.defaultValue "actual: unavailable"

        let errorText =
            match result.ErrorMessage with
            | Some error -> $" error={error}"
            | None -> ""

        printfn $"FAIL {result.Name}{precisionText} {expectedText} {actualText}{errorText}"

module TestRunner =
    open Types
    open Config
    open FixtureIO
    open Geometry

    let normalizeRegions (regions: float[][]) : float[][] =
        polybool.Union(regions, emptyRegions)

    let applyOperation (operation: Operation) (left: float[][]) (right: float[][]) : float[][] =
        match operation with
        | Union -> polybool.Union(left, right)
        | Intersection -> polybool.Intersect(left, right)
        | Difference -> polybool.Difference(left, right)
        | Xor -> polybool.Xor(left, right)

    let runOperation (operation: Operation) (arguments: float[][][]) : float[][] =
        match arguments with
        | [||] -> emptyRegions
        | [| single |] -> normalizeRegions single
        | _ -> arguments.[1..] |> Array.fold (fun current next -> applyOperation operation current next) arguments.[0]

    let runTestCase (testCase: TestCase) : TestResult =
        let mutable arguments: float[][][] = [||]
        let mutable precision: float option = None
        let mutable expectedSummary: GeometrySummary option = None
        let mutable actualSummary: GeometrySummary option = None
        let mutable expectedRegions: float[][] option = None
        let mutable actualRegions: float[][] option = None
        printfn $"doing {testCase.Name}"
        try
            match testCase.Operation with
            | None ->
                {
                    Name = testCase.Name
                    Passed = false
                    Precision = None
                    ExpectedSummary = None
                    ActualSummary = None
                    ErrorMessage = Some "Unknown operation name derived from fixture filename"
                }
            | Some operation ->
                arguments <- loadArguments testCase.ArgsPath
                let expectedData = loadExpected testCase.ExpectedPath
                expectedRegions <- Some expectedData.Regions
                precision <- expectedData.Precision
                expectedSummary <- Some(summarizeGeometry expectedData.Regions)
                let actual = runOperation operation arguments
                actualRegions <- Some actual
                actualSummary <- Some(summarizeGeometry actual)
                let passed = matchesExpected expectedData.Tolerance actual expectedData.Regions

                RhinoDrawing.drawTestCase testCase.Name arguments actualRegions (if passed then None else expectedRegions)

                {
                    Name = testCase.Name
                    Passed = passed
                    Precision = precision
                    ExpectedSummary = expectedSummary
                    ActualSummary = actualSummary
                    ErrorMessage = None
                }
        with ex ->
            RhinoDrawing.drawTestCase testCase.Name arguments actualRegions expectedRegions

            {
                Name = testCase.Name
                Passed = false
                Precision = precision
                ExpectedSummary = expectedSummary
                ActualSummary = actualSummary
                ErrorMessage = Some ex.Message
            }

module Main =

    let run () : unit =
        RhinoDrawing.resetLayout ()
        let testCases = FixtureIO.discoverTests ()
        let results = testCases |> Array.map TestRunner.runTestCase

        results
        |> Array.iter (fun result ->
            if not result.Passed then
                Reporting.printFailure result)

        let passCount = results |> Array.sumBy (fun result -> if result.Passed then 1 else 0)
        let failCount = results.Length - passCount

        printfn $"Total: {testCases.Length}"
        printfn $"Pass: {passCount}"
        printfn $"Fail: {failCount}"

        if failCount > 0 then
            Environment.ExitCode <- 1

Main.run ()
printfn "Done"



