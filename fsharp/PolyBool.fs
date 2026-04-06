namespace PolyBool

//
// polybool - Boolean operations on polygons (union, intersection, etc)
// by Sean Connelly (@velipso), https://sean.fun
// Project Home: https://github.com/velipso/polybool
// SPDX-License-Identifier: 0BSD
//

type internal Polygon =
    {
        regions: Vec2[][]
        inverted: bool
    }

type internal Segments =
    {
        shape: Shape
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

    let polygonFromRegions(regions: Vec2[][]) : Polygon =
        {
            regions = regions
            inverted = false
        }

    let polygonFromRegionLists(regions: Vec2 list list) : Polygon =
        {
            regions = regions |> List.map List.toArray |> List.toArray
            inverted = false
        }

    let regionListsFromPolygon(poly: Polygon) : Vec2 list list =
        poly.regions |> Array.map Array.toList |> Array.toList



    member this.buildLog(enable: bool) : ResizeArray<BuildLogEntry> option =
        log <- if enable then Some(BuildLog()) else None
        log |> Option.map (fun log -> log.list)

    member private this.segmentsOfPolygon(poly: Polygon) : Segments =
        let shape: Shape = Shape(None, log)
        shape.beginPath() |> ignore

        let asVertex(point: Vec2) : Vec2 =
            if point.Length <> 2 then
                failwith "PolyBool: Invalid point in region; only polygon vertices are supported"
            point

        for region: Vec2[] in poly.regions do
            if region.Length <= 0 then
                failwith "PolyBool: Regions must contain at least one vertex"

            let lastPoint: Vec2 = asVertex region.[region.Length - 1]
            shape.moveTo(lastPoint.[0], lastPoint.[1]) |> ignore

            for p: Vec2 in region do
                let point: Vec2 = asVertex p
                shape.lineTo(point.[0], point.[1]) |> ignore

            shape.closePath() |> ignore

        {
            shape = shape
            inverted = poly.inverted
        }

    member private this.combineSegments(segments1: Segments, segments2: Segments) : CombinedSegments =
        {
            shape = segments1.shape.combine(segments2.shape)
            inverted1 = segments1.inverted
            inverted2 = segments2.inverted
        }

    member private this.selectUnionSegments(combined: CombinedSegments) : Segments =
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

    member private this.selectIntersectSegments(combined: CombinedSegments) : Segments =
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

    member private this.selectDifferenceSegments(combined: CombinedSegments) : Segments =
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

    member private this.selectDifferenceRevSegments(combined: CombinedSegments) : Segments =
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

    member private this.selectXorSegments(combined: CombinedSegments) : Segments =
        {
            shape = combined.shape.xor()
            inverted = combined.inverted1 <> combined.inverted2
        }

    member private this.polygonOfSegments(segments: Segments) : Polygon =
        let regions: ResizeArray<ResizeArray<Vec2>> = ResizeArray<ResizeArray<Vec2>>()

        let receiver =
            PolyBoolReceiver(
                moveTo = (fun (_: float, _: float) -> regions.Add(ResizeArray<Vec2>())),
                lineTo = (fun (x: float, y: float) -> regions.[regions.Count - 1].Add([| x; y |]))
            )

        segments.shape.output(receiver) |> ignore

        {
            regions = regions |> Seq.map (fun region -> region.ToArray()) |> Array.ofSeq
            inverted = segments.inverted
        }

    member private this.unionPolygon(poly1: Polygon, poly2: Polygon) : Polygon =
        let seg1: Segments = this.segmentsOfPolygon(poly1)
        let seg2: Segments = this.segmentsOfPolygon(poly2)
        let comb: CombinedSegments = this.combineSegments(seg1, seg2)
        let seg3: Segments = this.selectUnionSegments(comb)
        this.polygonOfSegments(seg3)

    member private this.intersectPolygon(poly1: Polygon, poly2: Polygon) : Polygon =
        let seg1: Segments = this.segmentsOfPolygon(poly1)
        let seg2: Segments = this.segmentsOfPolygon(poly2)
        let comb: CombinedSegments = this.combineSegments(seg1, seg2)
        let seg3: Segments = this.selectIntersectSegments(comb)
        this.polygonOfSegments(seg3)

    member private this.differencePolygon(poly1: Polygon, poly2: Polygon) : Polygon =
        let seg1: Segments = this.segmentsOfPolygon(poly1)
        let seg2: Segments = this.segmentsOfPolygon(poly2)
        let comb: CombinedSegments = this.combineSegments(seg1, seg2)
        let seg3: Segments = this.selectDifferenceSegments(comb)
        this.polygonOfSegments(seg3)

    member private this.differenceRevPolygon(poly1: Polygon, poly2: Polygon) : Polygon =
        let seg1: Segments = this.segmentsOfPolygon(poly1)
        let seg2: Segments = this.segmentsOfPolygon(poly2)
        let comb: CombinedSegments = this.combineSegments(seg1, seg2)
        let seg3: Segments = this.selectDifferenceRevSegments(comb)
        this.polygonOfSegments(seg3)

    member private this.xorPolygon(poly1: Polygon, poly2: Polygon) : Polygon =
        let seg1: Segments = this.segmentsOfPolygon(poly1)
        let seg2: Segments = this.segmentsOfPolygon(poly2)
        let comb: CombinedSegments = this.combineSegments(seg1, seg2)
        let seg3: Segments = this.selectXorSegments(comb)
        this.polygonOfSegments(seg3)

    member this.union(regions1: Vec2[][], regions2: Vec2[][]) : Vec2[][] =
        let poly1: Polygon = polygonFromRegions regions1
        let poly2: Polygon = polygonFromRegions regions2
        let result: Polygon = this.unionPolygon(poly1, poly2)
        result.regions

    member this.intersect(regions1: Vec2[][], regions2: Vec2[][]) : Vec2[][] =
        let poly1: Polygon = polygonFromRegions regions1
        let poly2: Polygon = polygonFromRegions regions2
        let result: Polygon = this.intersectPolygon(poly1, poly2)
        result.regions

    member this.difference(regions1: Vec2[][], regions2: Vec2[][]) : Vec2[][] =
        let poly1: Polygon = polygonFromRegions regions1
        let poly2: Polygon = polygonFromRegions regions2
        let result: Polygon = this.differencePolygon(poly1, poly2)
        result.regions

    member this.differenceRev(regions1: Vec2[][], regions2: Vec2[][]) : Vec2[][] =
        let poly1: Polygon = polygonFromRegions regions1
        let poly2: Polygon = polygonFromRegions regions2
        let result: Polygon = this.differenceRevPolygon(poly1, poly2)
        result.regions

    member this.xor(regions1: Vec2[][], regions2: Vec2[][]) : Vec2[][] =
        let poly1: Polygon = polygonFromRegions regions1
        let poly2: Polygon = polygonFromRegions regions2
        let result: Polygon = this.xorPolygon(poly1, poly2)
        result.regions

    member this.union(regions1: Vec2 list list, regions2: Vec2 list list) : Vec2 list list =
        let poly1: Polygon = polygonFromRegionLists regions1
        let poly2: Polygon = polygonFromRegionLists regions2
        let result: Polygon = this.unionPolygon(poly1, poly2)
        regionListsFromPolygon result

    member this.intersect(regions1: Vec2 list list, regions2: Vec2 list list) : Vec2 list list =
        let poly1: Polygon = polygonFromRegionLists regions1
        let poly2: Polygon = polygonFromRegionLists regions2
        let result: Polygon = this.intersectPolygon(poly1, poly2)
        regionListsFromPolygon result

    member this.difference(regions1: Vec2 list list, regions2: Vec2 list list) : Vec2 list list =
        let poly1: Polygon = polygonFromRegionLists regions1
        let poly2: Polygon = polygonFromRegionLists regions2
        let result: Polygon = this.differencePolygon(poly1, poly2)
        regionListsFromPolygon result

    member this.differenceRev(regions1: Vec2 list list, regions2: Vec2 list list) : Vec2 list list =
        let poly1: Polygon = polygonFromRegionLists regions1
        let poly2: Polygon = polygonFromRegionLists regions2
        let result: Polygon = this.differenceRevPolygon(poly1, poly2)
        regionListsFromPolygon result

    member this.xor(regions1: Vec2 list list, regions2: Vec2 list list) : Vec2 list list =
        let poly1: Polygon = polygonFromRegionLists regions1
        let poly2: Polygon = polygonFromRegionLists regions2
        let result: Polygon = this.xorPolygon(poly1, poly2)
        regionListsFromPolygon result

module PolyBoolExports =
    let polybool: PolyBool = PolyBool()