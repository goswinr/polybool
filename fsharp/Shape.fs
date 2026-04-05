namespace PolyBool

//
// polybool - Boolean operations on polygons (union, intersection, etc)
// by Sean Connelly (@velipso), https://sean.fun
// Project Home: https://github.com/velipso/polybool
// SPDX-License-Identifier: 0BSD
//

type Transform =
    {
        a: float
        b: float
        c: float
        d: float
        e: float
        f: float
    }

type private PathState =
    | BeginPath
    | MoveTo of start: Vec2 * current: Vec2

type private ShapeResultState =
    | New of selfIntersect: Intersecter
    | Seg of segments: SegmentBool[]
    | Reg of segments: SegmentBool[] * regions: Segment[][]

type Shape(geo: Geometry, segments: SegmentBool[] option, log: BuildLog option) =
    let mutable pathState: PathState = BeginPath
    let mutable resultState: ShapeResultState =
        match segments with
        | Some segments ->
            Seg segments
        | None ->
            New(Intersecter(true, geo, ?log = log))

    let saveStack: ResizeArray<Vec6> = ResizeArray<Vec6>()
    let mutable matrix: Vec6 = [| 1.0; 0.0; 0.0; 1.0; 0.0; 0.0 |]

    member this.geo: Geometry = geo
    member this.log: BuildLog option = log

    member this.setTransform(a: float, b: float, c: float, d: float, e: float, f: float) : Shape =
        match resultState with
        | New _ ->
            matrix <- [| a; b; c; d; e; f |]
            this
        | _ ->
            failwith "PolyBool: Cannot change shape after using it in an operation"

    member this.resetTransform() : Shape =
        matrix <- [| 1.0; 0.0; 0.0; 1.0; 0.0; 0.0 |]
        this

    member this.getTransform() : Transform =
        match resultState with
        | New _ ->
            {
                a = matrix.[0]
                b = matrix.[1]
                c = matrix.[2]
                d = matrix.[3]
                e = matrix.[4]
                f = matrix.[5]
            }
        | _ ->
            failwith "PolyBool: Cannot change shape after using it in an operation"

    member this.transform(a: float, b: float, c: float, d: float, e: float, f: float) : Shape =
        let a0: float = matrix.[0]
        let b0: float = matrix.[1]
        let c0: float = matrix.[2]
        let d0: float = matrix.[3]
        let e0: float = matrix.[4]
        let f0: float = matrix.[5]

        matrix <-
            [|
                a0 * a + c0 * b
                b0 * a + d0 * b
                a0 * c + c0 * d
                b0 * c + d0 * d
                a0 * e + c0 * f + e0
                b0 * e + d0 * f + f0
            |]

        this

    member this.rotate(angle: float) : Shape =
        let cos: float = System.Math.Cos(angle)
        let sin: float = System.Math.Sin(angle)
        let a0: float = matrix.[0]
        let b0: float = matrix.[1]
        let c0: float = matrix.[2]
        let d0: float = matrix.[3]
        let e0: float = matrix.[4]
        let f0: float = matrix.[5]

        matrix <-
            [|
                a0 * cos + c0 * sin
                b0 * cos + d0 * sin
                c0 * cos - a0 * sin
                d0 * cos - b0 * sin
                e0
                f0
            |]

        this

    member this.rotateDeg(angle: float) : Shape =
        let ang: float = ((angle % 360.0) + 360.0) % 360.0

        if ang = 0.0 then
            this
        else
            let mutable cos: float = 0.0
            let mutable sin: float = 0.0

            if ang = 90.0 then
                sin <- 1.0
            elif ang = 180.0 then
                cos <- -1.0
            elif ang = 270.0 then
                sin <- -1.0
            elif ang = 45.0 then
                cos <- System.Math.Sqrt(0.5)
                sin <- System.Math.Sqrt(0.5)
            elif ang = 135.0 then
                sin <- System.Math.Sqrt(0.5)
                cos <- -System.Math.Sqrt(0.5)
            elif ang = 225.0 then
                cos <- -System.Math.Sqrt(0.5)
                sin <- -System.Math.Sqrt(0.5)
            elif ang = 315.0 then
                cos <- System.Math.Sqrt(0.5)
                sin <- -System.Math.Sqrt(0.5)
            elif ang = 30.0 then
                cos <- System.Math.Sqrt(3.0) / 2.0
                sin <- 0.5
            elif ang = 60.0 then
                cos <- 0.5
                sin <- System.Math.Sqrt(3.0) / 2.0
            elif ang = 120.0 then
                cos <- -0.5
                sin <- System.Math.Sqrt(3.0) / 2.0
            elif ang = 150.0 then
                cos <- -System.Math.Sqrt(3.0) / 2.0
                sin <- 0.5
            elif ang = 210.0 then
                cos <- -System.Math.Sqrt(3.0) / 2.0
                sin <- -0.5
            elif ang = 240.0 then
                cos <- -0.5
                sin <- -System.Math.Sqrt(3.0) / 2.0
            elif ang = 300.0 then
                cos <- 0.5
                sin <- -System.Math.Sqrt(3.0) / 2.0
            elif ang = 330.0 then
                cos <- System.Math.Sqrt(3.0) / 2.0
                sin <- -0.5
            else
                let rad: float = (System.Math.PI * ang) / 180.0
                cos <- System.Math.Cos(rad)
                sin <- System.Math.Sin(rad)

            let a0: float = matrix.[0]
            let b0: float = matrix.[1]
            let c0: float = matrix.[2]
            let d0: float = matrix.[3]
            let e0: float = matrix.[4]
            let f0: float = matrix.[5]

            matrix <-
                [|
                    a0 * cos + c0 * sin
                    b0 * cos + d0 * sin
                    c0 * cos - a0 * sin
                    d0 * cos - b0 * sin
                    e0
                    f0
                |]

            this

    member this.scale(sx: float, sy: float) : Shape =
        let a0: float = matrix.[0]
        let b0: float = matrix.[1]
        let c0: float = matrix.[2]
        let d0: float = matrix.[3]
        let e0: float = matrix.[4]
        let f0: float = matrix.[5]

        matrix <- [| a0 * sx; b0 * sx; c0 * sy; d0 * sy; e0; f0 |]
        this

    member this.translate(tx: float, ty: float) : Shape =
        let a0: float = matrix.[0]
        let b0: float = matrix.[1]
        let c0: float = matrix.[2]
        let d0: float = matrix.[3]
        let e0: float = matrix.[4]
        let f0: float = matrix.[5]

        matrix <-
            [|
                a0
                b0
                c0
                d0
                a0 * tx + c0 * ty + e0
                b0 * tx + d0 * ty + f0
            |]

        this

    member this.save() : Shape =
        match resultState with
        | New _ ->
            saveStack.Add(Array.copy matrix)
            this
        | _ ->
            failwith "PolyBool: Cannot change shape after using it in an operation"

    member this.restore() : Shape =
        match resultState with
        | New _ ->
            if saveStack.Count > 0 then
                matrix <- saveStack.[saveStack.Count - 1]
                saveStack.RemoveAt(saveStack.Count - 1)

            this
        | _ ->
            failwith "PolyBool: Cannot change shape after using it in an operation"

    member this.transformPoint(x: float, y: float) : Vec2 =
        let a: float = matrix.[0]
        let b: float = matrix.[1]
        let c: float = matrix.[2]
        let d: float = matrix.[3]
        let e: float = matrix.[4]
        let f: float = matrix.[5]
        [| a * x + c * y + e; b * x + d * y + f |]

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

            let current: Vec2 = this.transformPoint(x, y)
            pathState <- MoveTo(current, current)
            this
        | _ ->
            failwith "PolyBool: Cannot change shape after using it in an operation"

    member this.lineTo(x: float, y: float) : Shape =
        match resultState, pathState with
        | New selfIntersect, MoveTo(_, currentPoint) ->
            let current: Vec2 = this.transformPoint(x, y)
            selfIntersect.addLine(currentPoint, current)
            pathState <- MoveTo(match pathState with | MoveTo(start, _) -> start, current | _ -> current, current)
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

    member this.bezierCurveTo(
        cp1x: float,
        cp1y: float,
        cp2x: float,
        cp2y: float,
        x: float,
        y: float
    ) : Shape =
        match resultState, pathState with
        | New selfIntersect, MoveTo(_, currentPoint) ->
            let current: Vec2 = this.transformPoint(x, y)

            selfIntersect.addCurve(
                currentPoint,
                this.transformPoint(cp1x, cp1y),
                this.transformPoint(cp2x, cp2y),
                current
            )

            pathState <- MoveTo(match pathState with | MoveTo(start, _) -> start, current | _ -> current, current)
            this
        | New _, _ ->
            failwith "PolyBool: Must call moveTo prior to calling bezierCurveTo"
        | _ ->
            failwith "PolyBool: Cannot change shape after using it in an operation"

    member this.closePath() : Shape =
        match resultState with
        | New selfIntersect ->
            // close with a line if needed
            match pathState with
            | MoveTo(start, current) when not (this.geo.isEqualVec2(start, current)) ->
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
            let segments: SegmentBool[] = selfIntersect.calculate()
            resultState <- Seg segments
            segments
        | Seg segments ->
            segments
        | Reg(segments, _) ->
            segments

    member this.segments() : Segment[][] =
        match resultState with
        | Reg(_, regions) ->
            regions
        | _ ->
            let seg: SegmentBool[] = this.selfIntersect()
            let regions: Segment[][] = SegmentChainer.segmentChainer(seg, this.geo, this.log)
            resultState <- Reg(seg, regions)
            regions

    member this.output<'T when 'T :> IPolyBoolReceiver>(receiver: 'T, ?matrix: Vec6) : 'T =
        SegmentChainer.segmentsToReceiver(
            this.segments(),
            this.geo,
            receiver,
            defaultArg matrix [| 1.0; 0.0; 0.0; 1.0; 0.0; 0.0 |]
        )

    member this.combine(shape: Shape) : ShapeCombined =
        let intersection: Intersecter = Intersecter(false, this.geo, ?log = this.log)

        for seg: SegmentBool in this.selfIntersect() do
            intersection.addSegment(IntersecterFunctions.copySegmentBool(seg, this.log), true) |> ignore

        for seg: SegmentBool in shape.selfIntersect() do
            intersection.addSegment(IntersecterFunctions.copySegmentBool(seg, this.log), false) |> ignore

        ShapeCombined(intersection.calculate(), this.geo, this.log)

and ShapeCombined(segments: SegmentBool[], geo: Geometry, log: BuildLog option) =
    member this.geo: Geometry = geo
    member this.log: BuildLog option = log
    member this.segmentsData: SegmentBool[] = segments

    member this.union() : Shape =
        Shape(this.geo, Some(SegmentSelector.union(this.segmentsData, this.log)), this.log)

    member this.intersect() : Shape =
        Shape(this.geo, Some(SegmentSelector.intersect(this.segmentsData, this.log)), this.log)

    member this.difference() : Shape =
        Shape(this.geo, Some(SegmentSelector.difference(this.segmentsData, this.log)), this.log)

    member this.differenceRev() : Shape =
        Shape(this.geo, Some(SegmentSelector.differenceRev(this.segmentsData, this.log)), this.log)

    member this.xor() : Shape =
        Shape(this.geo, Some(SegmentSelector.xor(this.segmentsData, this.log)), this.log)
