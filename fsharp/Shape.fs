namespace PolyBool

//
// polybool - Boolean operations on polygons (union, intersection, etc)
// by Sean Connelly (@velipso), https://sean.fun
// Project Home: https://github.com/velipso/polybool
// SPDX-License-Identifier: 0BSD
//

type private PathState =
    | BeginPath
    | MoveTo of start: Vec2 * current: Vec2

type private ShapeResultState =
    | New of selfIntersect: Intersecter
    | Seg of segments: SegmentBool[]
    | Reg of segments: SegmentBool[] * regions: Segment[][]

type internal Shape( segments: SegmentBool[] option, log: BuildLog option) =
    let mutable pathState: PathState = BeginPath
    let mutable resultState: ShapeResultState =
        match segments with
        | Some initialSegments ->
            Seg initialSegments
        | None ->
            New(Intersecter(true,  ?log = log))


    member this.log: BuildLog option = log

    member this.beginPath() : Shape =
        match resultState with
        | New selfIntersect ->
            selfIntersect.beginPath()
            this.endPath()
        | _ ->
            failwith "PolyBool: Cannot change shape after using it in an operation"

    member this.moveTo(x: float, y: float) : Shape =
        match resultState with
        | New _ ->
            match pathState with
            | BeginPath ->
                ()
            | _ ->
                this.beginPath() |> ignore

            let current: Vec2 = [| x; y |]
            pathState <- MoveTo(current, current)
            this
        | _ ->
            failwith "PolyBool: Cannot change shape after using it in an operation"

    member this.lineTo(x: float, y: float) : Shape =
        match resultState, pathState with
        | New selfIntersect, MoveTo(start, currentPoint) ->
            let current: Vec2 = [| x; y |]
            selfIntersect.addLine(currentPoint, current)
            pathState <- MoveTo(start, current)
            this
        | New _, _ ->
            failwith "PolyBool: Must call moveTo prior to calling lineTo"
        | _ ->
            failwith "PolyBool: Cannot change shape after using it in an operation"

    member this.rect(x: float, y: float, width: float, height: float) : Shape =
        this.moveTo(x, y)
            .lineTo(x + width, y)
            .lineTo(x + width, y + height)
            .lineTo(x, y + height)
            .closePath()
            .moveTo(x, y)

    member this.closePath() : Shape =
        match resultState with
        | New selfIntersect ->
            match pathState with
            | MoveTo(start, current) when not (Geometry.isEqualVec2(start, current)) ->
                selfIntersect.addLine(current, start)
                pathState <- MoveTo(start, start)
            | _ ->
                ()

            selfIntersect.closePath()
            this.endPath()
        | _ ->
            failwith "PolyBool: Cannot change shape after using it in an operation"

    member this.endPath() : Shape =
        match resultState with
        | New _ ->
            pathState <- BeginPath
            this
        | _ ->
            failwith "PolyBool: Cannot change shape after using it in an operation"

    member private this.selfIntersect() : SegmentBool[] =
        match resultState with
        | New selfIntersect ->
            let calculatedSegments: SegmentBool[] = selfIntersect.calculate()
            resultState <- Seg calculatedSegments
            calculatedSegments
        | Seg calculatedSegments ->
            calculatedSegments
        | Reg(calculatedSegments, _) ->
            calculatedSegments

    member this.segments() : Segment[][] =
        match resultState with
        | Reg(_, regions) ->
            regions
        | _ ->
            let calculatedSegments: SegmentBool[] = this.selfIntersect()
            let regions: Segment[][] = SegmentChainer.segmentChainer(calculatedSegments, this.log)
            resultState <- Reg(calculatedSegments, regions)
            regions

    member this.output(receiver: PolyBoolReceiver) : PolyBoolReceiver =
        SegmentChainer.segmentsToReceiver(this.segments(), receiver)

    member this.combine(shape: Shape) : ShapeCombined =
        let intersection: Intersecter = Intersecter(false, ?log = this.log)

        for seg: SegmentBool in this.selfIntersect() do
            intersection.addSegment(IntersecterFunctions.copySegmentBool(seg, this.log), true) |> ignore

        for seg: SegmentBool in shape.selfIntersect() do
            intersection.addSegment(IntersecterFunctions.copySegmentBool(seg, this.log), false) |> ignore

        ShapeCombined(intersection.calculate(), this.log)

and internal ShapeCombined(segments: SegmentBool[],  log: BuildLog option) =

    member this.log: BuildLog option = log
    member this.segmentsData: SegmentBool[] = segments

    member this.union() : Shape =
        Shape(Some(SegmentSelector.union(this.segmentsData, this.log)), this.log)

    member this.intersect() : Shape =
        Shape(Some(SegmentSelector.intersect(this.segmentsData, this.log)), this.log)

    member this.difference() : Shape =
        Shape(Some(SegmentSelector.difference(this.segmentsData, this.log)), this.log)

    member this.differenceRev() : Shape =
        Shape(Some(SegmentSelector.differenceRev(this.segmentsData, this.log)), this.log)

    member this.xor() : Shape =
        Shape(Some(SegmentSelector.xor(this.segmentsData, this.log)), this.log)