namespace PolyBool

//
// polybool - Boolean operations on polygons (union, intersection, etc)
// by Sean Connelly (@velipso), https://sean.fun
// Project Home: https://github.com/velipso/polybool
// SPDX-License-Identifier: 0BSD
//

type private PathState =
    | BeginPath
    | MoveTo of startX: float * startY: float * currentX: float * currentY: float

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


    member this.Log: BuildLog option = log

    member this.BeginPath() : Shape =
        match resultState with
        | New selfIntersect ->
            selfIntersect.BeginPath()
            this.EndPath()
        | _ ->
            failwith "PolyBool: Cannot change shape after using it in an operation"

    member this.MoveTo(x: float, y: float) : Shape =
        match resultState with
        | New _ ->
            match pathState with
            | BeginPath ->
                ()
            | _ ->
                this.BeginPath() |> ignore

            pathState <- MoveTo(x, y, x, y)
            this
        | _ ->
            failwith "PolyBool: Cannot change shape after using it in an operation"

    member this.LineTo(x: float, y: float) : Shape =
        match resultState, pathState with
        | New selfIntersect, MoveTo(startX, startY, currentX, currentY) ->
            selfIntersect.AddLine(currentX, currentY, x, y)
            pathState <- MoveTo(startX, startY, x, y)
            this
        | New _, _ ->
            failwith "PolyBool: Must call moveTo prior to calling lineTo"
        | _ ->
            failwith "PolyBool: Cannot change shape after using it in an operation"

    member this.Rect(x: float, y: float, width: float, height: float) : Shape =
        this.MoveTo(x, y)
            .LineTo(x + width, y)
            .LineTo(x + width, y + height)
            .LineTo(x, y + height)
            .ClosePath()
            .MoveTo(x, y)

    member this.ClosePath() : Shape =
        match resultState with
        | New selfIntersect ->
            match pathState with
            | MoveTo(startX, startY, currentX, currentY) when not (Geometry.isEqualVec2(startX, startY, currentX, currentY)) ->
                selfIntersect.AddLine(currentX, currentY, startX, startY)
                pathState <- MoveTo(startX, startY, startX, startY)
            | _ ->
                ()

            selfIntersect.ClosePath()
            this.EndPath()
        | _ ->
            failwith "PolyBool: Cannot change shape after using it in an operation"

    member this.EndPath() : Shape =
        match resultState with
        | New _ ->
            pathState <- BeginPath
            this
        | _ ->
            failwith "PolyBool: Cannot change shape after using it in an operation"

    member private this.SelfIntersect() : SegmentBool[] =
        match resultState with
        | New selfIntersect ->
            let calculatedSegments: SegmentBool[] = selfIntersect.Calculate()
            resultState <- Seg calculatedSegments
            calculatedSegments
        | Seg calculatedSegments ->
            calculatedSegments
        | Reg(calculatedSegments, _) ->
            calculatedSegments

    member this.Segments() : Segment[][] =
        match resultState with
        | Reg(_, regions) ->
            regions
        | _ ->
            let calculatedSegments: SegmentBool[] = this.SelfIntersect()
            let regions: Segment[][] = SegmentChainer.segmentChainer(calculatedSegments, this.Log)
            resultState <- Reg(calculatedSegments, regions)
            regions

    member this.Output(receiver: PolyBoolReceiver) : PolyBoolReceiver =
        SegmentChainer.segmentsToReceiver(this.Segments(), receiver)

    member this.Combine(shape: Shape) : ShapeCombined =
        let intersection: Intersecter = Intersecter(false, ?log = this.Log)

        for seg: SegmentBool in this.SelfIntersect() do
            intersection.AddSegment(IntersecterFunctions.copySegmentBool(seg, this.Log), true) |> ignore

        for seg: SegmentBool in shape.SelfIntersect() do
            intersection.AddSegment(IntersecterFunctions.copySegmentBool(seg, this.Log), false) |> ignore

        ShapeCombined(intersection.Calculate(), this.Log)

and internal ShapeCombined(segments: SegmentBool[],  log: BuildLog option) =

    member this.Log: BuildLog option = log
    member this.SegmentsData: SegmentBool[] = segments

    member this.Union() : Shape =
           Shape(Some(SegmentSelector.union(this.SegmentsData, this.Log)), this.Log)

    member this.Intersect() : Shape =
           Shape(Some(SegmentSelector.intersect(this.SegmentsData, this.Log)), this.Log)

    member this.Difference() : Shape =
           Shape(Some(SegmentSelector.difference(this.SegmentsData, this.Log)), this.Log)

    member this.DifferenceRev() : Shape =
           Shape(Some(SegmentSelector.differenceRev(this.SegmentsData, this.Log)), this.Log)

    member this.Xor() : Shape =
           Shape(Some(SegmentSelector.xor(this.SegmentsData, this.Log)), this.Log)
