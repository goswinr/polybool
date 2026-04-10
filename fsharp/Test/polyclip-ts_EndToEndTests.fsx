// uses https://github.com/luizbarboza/polyclip-ts/tree/main/test/end-to-end

#r "C:/Program Files/Rhino 8/System/RhinoCommon.dll"
#r "nuget: Rhino.Scripting.FSharp"
#r "nuget: ResizeArrayT"
#r "nuget: Euclid.Rhino"
#r "nuget: Str"
#r "D:/Git/_Clip_/polybool/fsharp/PolyBool/bin/Debug/netstandard2.0/PolyBool.dll" // relative path fails in Rhino

open System
open System.Text
open System.IO
open System.Text.Json
open ResizeArrayT
open Euclid
open Rhino.Scripting
open Rhino.Scripting.FSharp
open Str

type rs = RhinoScriptSyntax

type MultiPolygon = {
    polys: ResizeArray<Polyline2D>
}

type Operation =
    | Union
    | Intersection
    | Difference
    | Xor

type TestCase = {
    folderName: string
    operation: Operation
    subjects: MultiPolygon
    objects: ResizeArray<MultiPolygon>
    expected: MultiPolygon
    }


module Parse =

    let root = "D:/Git/_Clip_/polyclip-ts/test/end-to-end"


    let inline headAndTail (resizeArray: ResizeArray<'T>) : 'T * ResizeArray<'T> =
        if resizeArray.Count = 0 then
            failwith "headAndTail: input ResizeArray is empty"
        resizeArray.[0], resizeArray.GetRange(1, resizeArray.Count - 1)


    let getArrayDepth (element: JsonElement) : int =
        let rec depth (elem: JsonElement) (currentDepth: int) : int =
            if elem.ValueKind = JsonValueKind.Array then
                // if elem.GetArrayLength() = 0 then
                //     currentDepth + 1
                // else
                    depth elem.[0] (currentDepth + 1)
            else
                currentDepth

        depth element 0

    let getPolygon (coordinates: JsonElement) : Polyline2D =
        let pts = ResizeArray<Pt>()
        for coord in coordinates.EnumerateArray() do
            let x = coord.[0].GetDouble()
            let y = coord.[1].GetDouble()
            pts.Add(Pt(x, y))
        Polyline2D(pts)


    let getFeature (feature: JsonElement) : MultiPolygon =
        let geometry = feature.GetProperty("geometry")
        let coordinates = geometry.GetProperty("coordinates")
        if coordinates.GetArrayLength() = 0 then
            { polys = ResizeArray() }
        else
            let geomType = geometry.GetProperty("type").GetString()
            let polys = ResizeArray<Polyline2D>()
            let depth = getArrayDepth coordinates
            match depth with
            | 4 ->
                //if geomType <> "MultiPolygon" then eprintfn $"Ignored geojson bug: array depth 4 should be a MultiPolygon but got: {geomType}"
                for polygon in coordinates.EnumerateArray() do
                    for ring in polygon.EnumerateArray() do
                        polys.Add(getPolygon ring)
            | 3 ->
                //if geomType <> "Polygon" then eprintfn $"Ignored geojson bug: array depth 3 should be a Polygon but got: {geomType}"
                for ring in coordinates.EnumerateArray() do
                    polys.Add(getPolygon ring)
            | t ->
                failwith $"Unsupported geometry depth {depth} geomType: {t}"

            { polys = polys }


    let parseFeature (geoJson: string) : MultiPolygon =
        use json = JsonDocument.Parse(geoJson)

        match json.RootElement.GetProperty("type").GetString() with
        | "Feature" -> getFeature json.RootElement
        | t -> failwith $"Expected GeoJSON Feature but got: {t}"


    let parseFeatureCollection (geoJson: string) : MultiPolygon * ResizeArray<MultiPolygon> =
        use json = JsonDocument.Parse(geoJson)
        match json.RootElement.GetProperty("type").GetString() with
        | "FeatureCollection" ->
            resizeArray {
                for feature in json.RootElement.GetProperty("features").EnumerateArray() do
                    getFeature feature
            }
            |> headAndTail

        | t ->
            failwith $"Expected GeoJSON FeatureCollection but got: {t}"


    let operationsForFile (fileName: string) : Operation[] =
        match Path.GetFileNameWithoutExtension(fileName).ToLowerInvariant() with
        | "all" -> [| Union; Intersection; Xor; Difference |]
        | "union" -> [| Union |]
        | "intersection" -> [| Intersection |]
        | "difference" -> [| Difference |]
        | "xor" -> [| Xor |]
        | name -> failwith $"Unsupported operation file: {name}"

    let getCases() : TestCase[] =
        [|
        for folder in Directory.GetDirectories(root) do
            let folderName = Path.GetFileName(folder)
            let mutable currentFile = "??"
            try
                let mutable subject, objects = Unchecked.defaultof<MultiPolygon>, null
                for file in Directory.GetFiles(folder) do
                    currentFile <- file
                    let fileName = Path.GetFileName(file)
                    match fileName with
                    | "args.geojson" ->
                        let (s, o) = parseFeatureCollection (File.ReadAllText(file))
                        subject <- s
                        objects <- o
                    | _ -> ()
                if objects = null then
                    failwith $"Test case {folderName} does not contain args.geojson"
                for file in Directory.GetFiles(folder) do
                    currentFile <- file
                    let fileName = Path.GetFileName(file)
                    let txt = File.ReadAllText(file)
                    match fileName with
                    | "args.geojson" -> () // already parsed
                    | "xor.geojson" ->          yield {folderName = folderName; operation = Xor;subjects = subject; objects = objects; expected = parseFeature txt}
                    | "union.geojson" ->        yield {folderName = folderName; operation = Union;subjects = subject; objects = objects; expected = parseFeature txt}
                    | "intersection.geojson" -> yield {folderName = folderName; operation = Intersection;subjects = subject; objects = objects; expected = parseFeature txt}
                    | "difference.geojson" ->   yield {folderName = folderName; operation = Difference;subjects = subject; objects = objects; expected = parseFeature txt}
                    | "all.geojson" ->
                                yield {folderName = folderName; operation = Xor;subjects = subject; objects = objects; expected = parseFeature txt}
                                yield {folderName = folderName; operation = Union;subjects = subject; objects = objects; expected = parseFeature txt}
                                yield {folderName = folderName; operation = Intersection;subjects = subject; objects = objects; expected = parseFeature txt}
                                yield {folderName = folderName; operation = Difference;subjects = subject; objects = objects; expected = parseFeature txt}
                    | "README" -> () // ignore
                    | name -> failwith $"Unsupported file in test case {folderName}: {name}"
            with ex ->
                eprintfn $"Error parsing test case {currentFile}: {ex.Message}"
                //raise ex
        |]

module Geo =


    let getBRect (mp: TestCase) : BRect =
        if mp.subjects.polys.IsEmpty then
            Unchecked.defaultof<BRect>
        else
            let mutable box = mp.subjects.polys.[0].BoundingRectangle
            for i=1 to mp.subjects.polys.LastIndex do
                box <- BRect.union box mp.subjects.polys.[i].BoundingRectangle
            for ob in mp.objects do
                for p in ob.polys do
                    box <- BRect.union box p.BoundingRectangle
            for p in mp.expected.polys do
                box <- BRect.union box p.BoundingRectangle

            box

    let boxLayer = "_bounding-boxes"
    let failedLayer = "_rs.AddPolyline failed"
    
    let draw layer (b:BRect) (v:Vc) name (pl0:Polyline2D) =
        let pl = pl0 |> Polyline2D.translate v
        if pl.PointCount > 4 && b.SizeX > 1e-5 && b.SizeY > 1e-5 then
            try
                pl
                |>  Polyline2D.removeDuplicatePoints (rs.UnitAbsoluteTolerance() * 2.1)
                |>  Polyline2D.toRhPolyline
                |>  rs.AddPolyline
                // |>! rs.setName name
                |>  rs.setLayer layer

            with ex ->
                //eprintfn $"Error drawing polyline {name} with {pl.PointCount}"
                rs.AddTextDot ("e",  pl.BoundingRectangle.Center.RhPt) |> rs.setLayer failedLayer
                for i, ln in pl.Segments |> Seq.indexed do
                    if ln.Length > 1e-6 then
                        ln.RhLine  |> rs.Ot.AddLine
                    else
                        rs.AddTextDot($"{i}", ln.Mid.RhPt)
                    |> rs.setLayer layer
        else
            for i, ln in pl.Segments |> Seq.indexed do
                if ln.Length > 1e-6 then
                    ln.RhLine  |> rs.Ot.AddLine
                else
                    rs.AddTextDot($"{i}", ln.Mid.RhPt)
                |> rs.setLayer layer


    
    let mutable cen = Pt(0, 0)
    let drawMultiPolygon v (br:BRect) name layer (mp: MultiPolygon) =
        if mp.polys.IsEmpty then
            rs.AddTextDot("Empty MP", cen.RhPt)
            |>! rs.setLayer layer
            |>  rs.setName name
        else
            for pl in mp.polys do
                draw layer br v name pl

        if br.SizeY > 1e-4 && br.SizeX > 1e-4 then
            rs.AddPolyline (br|> BRect.translate v).RhPolyline
            |> rs.setLayer boxLayer


    let isRH = Rhino.Runtime.HostUtils.RunningInRhino
    
    let mutable orig = Pt(0, 0)
    let mutable lastBr = Unchecked.defaultof<BRect>

    let drawTestCase (tc: TestCase) =
        if isRH then
            if tc.subjects.polys.Count = 0 then
                cen <- orig + lastBr.Diagonal * 0.5
                rs.AddTextDot("Empty Subject", cen.RhPt)
                |> rs.setLayer $"Empty-subjects::{tc.folderName}"
            else
                // move the polygons right, and align on X axis via bbox
                let br = getBRect tc
                lastBr <- br

                cen <- orig + br.Diagonal * 0.5
                let mutable v = orig - br.MinPt
                let shift() =
                    v <- v |> Vc.moveY ( br.SizeY * 1.2)


                // draw all in one place
                rs.AddTextDot($"{tc.operation}\n{tc.folderName}", cen |> Pt.moveY ( br.SizeY * (-1.3 - 0.8)) |> Pt.toRhPt)|> rs.setLayer boxLayer
                
                v <- v |> Vc.moveY ( br.SizeY * -1.3)
                drawMultiPolygon v br tc.folderName $"{tc.operation}::subjects" tc.subjects
                for ob in tc.objects do
                    drawMultiPolygon v br tc.folderName $"{tc.operation}::object" ob
                drawMultiPolygon v br tc.folderName $"{tc.operation}::expected" tc.expected
                v <- v |> Vc.moveY ( br.SizeY * 1.6) // move back up more than down


                // draw stagged
                drawMultiPolygon v br tc.folderName $"{tc.operation}::subjects" tc.subjects
                shift()
                for ob in tc.objects do
                    drawMultiPolygon v br tc.folderName $"{tc.operation}::object" ob
                    shift()
                drawMultiPolygon v br tc.folderName $"{tc.operation}::expected" tc.expected

            orig <- orig |> Pt.moveX( (max lastBr.SizeX lastBr.SizeY) * 1.4)

    let initRhino() =
        rs.DisableRedraw()
        rs.AddLayer boxLayer |> ignore
        rs.LayerLock boxLayer
        rs.LayerColor (boxLayer, Drawing.Color.Gray)
        rs.AddLayer failedLayer |> ignore
        rs.LayerLock failedLayer
        rs.LayerColor (failedLayer, Drawing.Color.Orange)
        

Geo.initRhino()
Parse.getCases()
|> Array.sortBy ( fun t -> (t.operation, (Geo.getBRect t).SizeY ))
|> Array.iter Geo.drawTestCase

printfn $"Done"

