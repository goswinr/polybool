namespace PolyBool

//
// polybool - Boolean operations on polygons (union, intersection, etc)
// by Sean Connelly (@velipso), https://sean.fun
// Project Home: https://github.com/velipso/polybool
// SPDX-License-Identifier: 0BSD
//

type Polygon =
    {
        regions: float[][][]
        inverted: bool
    }

type Segments =
    {
        shape: Shape
        inverted: bool
    }

type CombinedSegments =
    {
        shape: ShapeCombined
        inverted1: bool
        inverted2: bool
    }

type PolyBool(?geo: Geometry, ?log: BuildLog) =
    let geoValue: Geometry = defaultArg geo (GeometryEpsilon() :> Geometry)
    let mutable log: BuildLog option = log

    member this.geo: Geometry = geoValue

    member this.shape() : Shape =
        Shape(this.geo, None, log)

    member this.buildLog(enable: bool) : ResizeArray<BuildLogEntry> option =
        log <- if enable then Some(BuildLog()) else None
        log |> Option.map (fun log -> log.list)

    member this.segments(poly: Polygon) : Segments =
        let shape: Shape = this.shape()
        shape.beginPath() |> ignore

        for region: float[][] in poly.regions do
            let lastPoint: float[] = region.[region.Length - 1]
            shape.moveTo(lastPoint.[lastPoint.Length - 2], lastPoint.[lastPoint.Length - 1]) |> ignore

            for p: float[] in region do
                if p.Length = 2 then
                    shape.lineTo(p.[0], p.[1]) |> ignore
                elif p.Length = 6 then
                    shape.bezierCurveTo(p.[0], p.[1], p.[2], p.[3], p.[4], p.[5]) |> ignore
                else
                    failwith "PolyBool: Invalid point in region"

            shape.closePath() |> ignore

        {
            shape = shape
            inverted = poly.inverted
        }

    member this.combine(segments1: Segments, segments2: Segments) : CombinedSegments =
        {
            shape = segments1.shape.combine(segments2.shape)
            inverted1 = segments1.inverted
            inverted2 = segments2.inverted
        }

    member this.selectUnion(combined: CombinedSegments) : Segments =
        {
            shape =
                if combined.inverted1 then
                    if combined.inverted2 then
                        combined.shape.intersect()
                    else
                        combined.shape.difference()
                elif combined.inverted2 then
                    combined.shape.differenceRev()
                else
                    combined.shape.union()
            inverted = combined.inverted1 || combined.inverted2
        }

    member this.selectIntersect(combined: CombinedSegments) : Segments =
        {
            shape =
                if combined.inverted1 then
                    if combined.inverted2 then
                        combined.shape.union()
                    else
                        combined.shape.differenceRev()
                elif combined.inverted2 then
                    combined.shape.difference()
                else
                    combined.shape.intersect()
            inverted = combined.inverted1 && combined.inverted2
        }

    member this.selectDifference(combined: CombinedSegments) : Segments =
        {
            shape =
                if combined.inverted1 then
                    if combined.inverted2 then
                        combined.shape.differenceRev()
                    else
                        combined.shape.union()
                elif combined.inverted2 then
                    combined.shape.intersect()
                else
                    combined.shape.difference()
            inverted = combined.inverted1 && not combined.inverted2
        }

    member this.selectDifferenceRev(combined: CombinedSegments) : Segments =
        {
            shape =
                if combined.inverted1 then
                    if combined.inverted2 then
                        combined.shape.difference()
                    else
                        combined.shape.intersect()
                elif combined.inverted2 then
                    combined.shape.union()
                else
                    combined.shape.differenceRev()
            inverted = not combined.inverted1 && combined.inverted2
        }

    member this.selectXor(combined: CombinedSegments) : Segments =
        {
            shape = combined.shape.xor()
            inverted = combined.inverted1 <> combined.inverted2
        }

    member this.polygon(segments: Segments) : Polygon =
        let regions: ResizeArray<ResizeArray<float[]>> = ResizeArray<ResizeArray<float[]>>()

        let receiver =
            { new IPolyBoolReceiver with
                member _.beginPath() : unit = ()

                member _.moveTo(_: float, _: float) : unit =
                    regions.Add(ResizeArray<float[]>())

                member _.lineTo(x: float, y: float) : unit =
                    regions.[regions.Count - 1].Add([| x; y |])

                member _.bezierCurveTo(
                    c1x: float,
                    c1y: float,
                    c2x: float,
                    c2y: float,
                    x: float,
                    y: float
                ) : unit =
                    regions.[regions.Count - 1].Add([| c1x; c1y; c2x; c2y; x; y |])

                member _.closePath() : unit = ()
            }

        segments.shape.output(receiver) |> ignore

        {
            regions = regions |> Seq.map (fun region -> region.ToArray()) |> Array.ofSeq
            inverted = segments.inverted
        }

    // helper functions for common operations
    member this.union(poly1: Polygon, poly2: Polygon) : Polygon =
        let seg1: Segments = this.segments(poly1)
        let seg2: Segments = this.segments(poly2)
        let comb: CombinedSegments = this.combine(seg1, seg2)
        let seg3: Segments = this.selectUnion(comb)
        this.polygon(seg3)

    member this.intersect(poly1: Polygon, poly2: Polygon) : Polygon =
        let seg1: Segments = this.segments(poly1)
        let seg2: Segments = this.segments(poly2)
        let comb: CombinedSegments = this.combine(seg1, seg2)
        let seg3: Segments = this.selectIntersect(comb)
        this.polygon(seg3)

    member this.difference(poly1: Polygon, poly2: Polygon) : Polygon =
        let seg1: Segments = this.segments(poly1)
        let seg2: Segments = this.segments(poly2)
        let comb: CombinedSegments = this.combine(seg1, seg2)
        let seg3: Segments = this.selectDifference(comb)
        this.polygon(seg3)

    member this.differenceRev(poly1: Polygon, poly2: Polygon) : Polygon =
        let seg1: Segments = this.segments(poly1)
        let seg2: Segments = this.segments(poly2)
        let comb: CombinedSegments = this.combine(seg1, seg2)
        let seg3: Segments = this.selectDifferenceRev(comb)
        this.polygon(seg3)

    member this.xor(poly1: Polygon, poly2: Polygon) : Polygon =
        let seg1: Segments = this.segments(poly1)
        let seg2: Segments = this.segments(poly2)
        let comb: CombinedSegments = this.combine(seg1, seg2)
        let seg3: Segments = this.selectXor(comb)
        this.polygon(seg3)

module PolyBoolExports =
    let polybool: PolyBool = PolyBool()
