namespace PolyBool

//
// polybool - Boolean operations on polygons (union, intersection, etc)
// by Sean Connelly (@velipso), https://sean.fun
// Project Home: https://github.com/velipso/polybool
// SPDX-License-Identifier: 0BSD
//

type internal Polygon =
    {
        regions: float[][]
        /// Inverted Polygons means the concept of an "infinite plane minus this polygon".
        inverted: bool
    }

type internal Segments =
    {
        shape: Shape
        /// Inverted Polygons means the concept of an "infinite plane minus this polygon".
        inverted: bool
    }

type internal CombinedSegments =
    {
        shape: ShapeCombined
        inverted1: bool
        inverted2: bool
    }

type PolyBool(? log: BuildLog) =

    let mutable log: BuildLog option = log

    let polygonFromRegions(regions: float[][]) : Polygon =
        {
            regions = regions
            inverted = false
        }

    member this.BuildLog(enable: bool) : ResizeArray<BuildLogEntry> option =
        log <- if enable then Some(BuildLog()) else None
        log |> Option.map (fun log -> log.List)

    member private this.SegmentsOfPolygon(poly: Polygon) : Segments =
        let shape: Shape = Shape(None, log)
        shape.BeginPath() |> ignore

        for region: float[] in poly.regions do
            let pointCount: int = region.Length / 2

            if pointCount <= 0 then
                failwith "PolyBool: Regions must contain at least one vertex"

            let lastIdx: int = (pointCount - 1) * 2
            shape.MoveTo(region.[lastIdx], region.[lastIdx + 1]) |> ignore

            for i = 0 to pointCount - 1 do
                shape.LineTo(region.[i * 2], region.[i * 2 + 1]) |> ignore

            shape.ClosePath() |> ignore

        {
            shape = shape
            inverted = poly.inverted
        }

    member private this.CombineSegments(segments1: Segments, segments2: Segments) : CombinedSegments =
        {
            shape = segments1.shape.Combine(segments2.shape)
            inverted1 = segments1.inverted
            inverted2 = segments2.inverted
        }

    member private this.SelectUnionSegments(combined: CombinedSegments) : Segments =
        {
            shape =
                if combined.inverted1 then
                    if combined.inverted2 then
                        combined.shape.Intersect()
                    else
                        combined.shape.Difference()
                elif combined.inverted2 then
                    combined.shape.DifferenceRev()
                else
                    combined.shape.Union()
            inverted = combined.inverted1 || combined.inverted2
        }

    member private this.SelectIntersectSegments(combined: CombinedSegments) : Segments =
        {
            shape =
                if combined.inverted1 then
                    if combined.inverted2 then
                        combined.shape.Union()
                    else
                        combined.shape.DifferenceRev()
                elif combined.inverted2 then
                    combined.shape.Difference()
                else
                    combined.shape.Intersect()
            inverted = combined.inverted1 && combined.inverted2
        }

    member private this.SelectDifferenceSegments(combined: CombinedSegments) : Segments =
        {
            shape =
                if combined.inverted1 then
                    if combined.inverted2 then
                        combined.shape.DifferenceRev()
                    else
                        combined.shape.Union()
                elif combined.inverted2 then
                    combined.shape.Intersect()
                else
                    combined.shape.Difference()
            inverted = combined.inverted1 && not combined.inverted2
        }

    member private this.SelectDifferenceRevSegments(combined: CombinedSegments) : Segments =
        {
            shape =
                if combined.inverted1 then
                    if combined.inverted2 then
                        combined.shape.Difference()
                    else
                        combined.shape.Intersect()
                elif combined.inverted2 then
                    combined.shape.Union()
                else
                    combined.shape.DifferenceRev()
            inverted = not combined.inverted1 && combined.inverted2
        }

    member private this.SelectXorSegments(combined: CombinedSegments) : Segments =
        {
            shape = combined.shape.Xor()
            inverted = combined.inverted1 <> combined.inverted2
        }

    member private this.PolygonOfSegments(segments: Segments) : Polygon =
        let regions: ResizeArray<ResizeArray<float>> = ResizeArray<ResizeArray<float>>()

        let receiver =
            PolyBoolReceiver(
                moveTo = (fun (_: float, _: float) -> regions.Add(ResizeArray<float>())),
                lineTo = (fun (x: float, y: float) ->
                    let r: ResizeArray<float> = regions.[regions.Count - 1]
                    r.Add(x)
                    r.Add(y))
            )

        segments.shape.Output(receiver) |> ignore

        {
            regions = regions |> Seq.map (fun region -> region.ToArray()) |> Array.ofSeq
            inverted = segments.inverted
        }

    member private this.UnionPolygon(poly1: Polygon, poly2: Polygon) : Polygon =
        let seg1: Segments = this.SegmentsOfPolygon(poly1)
        let seg2: Segments = this.SegmentsOfPolygon(poly2)
        let comb: CombinedSegments = this.CombineSegments(seg1, seg2)
        let seg3: Segments = this.SelectUnionSegments(comb)
        this.PolygonOfSegments(seg3)

    member private this.IntersectPolygon(poly1: Polygon, poly2: Polygon) : Polygon =
        let seg1: Segments = this.SegmentsOfPolygon(poly1)
        let seg2: Segments = this.SegmentsOfPolygon(poly2)
        let comb: CombinedSegments = this.CombineSegments(seg1, seg2)
        let seg3: Segments = this.SelectIntersectSegments(comb)
        this.PolygonOfSegments(seg3)

    member private this.DifferencePolygon(poly1: Polygon, poly2: Polygon) : Polygon =
        let seg1: Segments = this.SegmentsOfPolygon(poly1)
        let seg2: Segments = this.SegmentsOfPolygon(poly2)
        let comb: CombinedSegments = this.CombineSegments(seg1, seg2)
        let seg3: Segments = this.SelectDifferenceSegments(comb)
        this.PolygonOfSegments(seg3)

    member private this.DifferenceRevPolygon(poly1: Polygon, poly2: Polygon) : Polygon =
        let seg1: Segments = this.SegmentsOfPolygon(poly1)
        let seg2: Segments = this.SegmentsOfPolygon(poly2)
        let comb: CombinedSegments = this.CombineSegments(seg1, seg2)
        let seg3: Segments = this.SelectDifferenceRevSegments(comb)
        this.PolygonOfSegments(seg3)

    member private this.XorPolygon(poly1: Polygon, poly2: Polygon) : Polygon =
        let seg1: Segments = this.SegmentsOfPolygon(poly1)
        let seg2: Segments = this.SegmentsOfPolygon(poly2)
        let comb: CombinedSegments = this.CombineSegments(seg1, seg2)
        let seg3: Segments = this.SelectXorSegments(comb)
        this.PolygonOfSegments(seg3)

    member this.Union(regions1: float[][], regions2: float[][]) : float[][] =
        let poly1: Polygon = polygonFromRegions regions1
        let poly2: Polygon = polygonFromRegions regions2
        let result: Polygon = this.UnionPolygon(poly1, poly2)
        result.regions

    member this.Intersect(regions1: float[][], regions2: float[][]) : float[][] =
        let poly1: Polygon = polygonFromRegions regions1
        let poly2: Polygon = polygonFromRegions regions2
        let result: Polygon = this.IntersectPolygon(poly1, poly2)
        result.regions

    member this.Difference(regions1: float[][], regions2: float[][]) : float[][] =
        let poly1: Polygon = polygonFromRegions regions1
        let poly2: Polygon = polygonFromRegions regions2
        let result: Polygon = this.DifferencePolygon(poly1, poly2)
        result.regions

    member this.DifferenceRev(regions1: float[][], regions2: float[][]) : float[][] =
        let poly1: Polygon = polygonFromRegions regions1
        let poly2: Polygon = polygonFromRegions regions2
        let result: Polygon = this.DifferenceRevPolygon(poly1, poly2)
        result.regions

    member this.Xor(regions1: float[][], regions2: float[][]) : float[][] =
        let poly1: Polygon = polygonFromRegions regions1
        let poly2: Polygon = polygonFromRegions regions2
        let result: Polygon = this.XorPolygon(poly1, poly2)
        result.regions
