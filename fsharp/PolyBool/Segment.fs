namespace PolyBool

open System

//
// polybool - Boolean operations on polygons (union, intersection, etc)
// by Sean Connelly (@velipso), https://sean.fun
// Project Home: https://github.com/velipso/polybool
// SPDX-License-Identifier: 0BSD
//

type internal SegmentIntersectionKind =
    | TValuePairsKind
    | TRangePairsKind

type internal SegmentIntersection private (kind: SegmentIntersectionKind, ?tValuePairs: float[], ?tStartA: float, ?tStartB: float, ?tEndA: float, ?tEndB: float) =
    member this.Kind: SegmentIntersectionKind = kind

    member this.TValuePairs: float[] =
        match kind, tValuePairs with
        | TValuePairsKind, Some value ->
            value
        | _ ->
            failwith "PolyBool: Intersection does not contain tValuePairs"

    member this.TValuePairsCount: int =
        match kind, tValuePairs with
        | TValuePairsKind, Some value ->
            value.Length / 2
        | _ ->
            0

    member this.TStartA: float =
        match kind, tStartA with
        | TRangePairsKind, Some value ->
            value
        | _ ->
            failwith "PolyBool: Intersection does not contain tStartA"

    member this.TStartB: float =
        match kind, tStartB with
        | TRangePairsKind, Some value ->
            value
        | _ ->
            failwith "PolyBool: Intersection does not contain tStartB"

    member this.TEndA: float =
        match kind, tEndA with
        | TRangePairsKind, Some value ->
            value
        | _ ->
            failwith "PolyBool: Intersection does not contain tEndA"

    member this.TEndB: float =
        match kind, tEndB with
        | TRangePairsKind, Some value ->
            value
        | _ ->
            failwith "PolyBool: Intersection does not contain tEndB"

    static member FromTValuePairs(tValuePairs: float[]) : SegmentIntersection =
        SegmentIntersection(TValuePairsKind, tValuePairs = tValuePairs)

    static member FromTRangePairs(tStartA: float, tStartB: float, tEndA: float, tEndB: float) : SegmentIntersection =
        SegmentIntersection(TRangePairsKind, tStartA = tStartA, tStartB = tStartB, tEndA = tEndA, tEndB = tEndB)

type internal SegmentTValuePairsBuilder(allowOutOfRange: bool) =
    let tValuePairsStore: ResizeArray<float> = ResizeArray<float>()

    member this.AllowOutOfRange: bool = allowOutOfRange

    member this.Add(t1: float, t2: float) : SegmentTValuePairsBuilder =
        let t1: float = Geometry.snap01 t1
        let t2: float = Geometry.snap01 t2

        if not this.AllowOutOfRange && (t1 < 0.0 || t1 > 1.0 || t2 < 0.0 || t2 > 1.0) then
            this
        else
            let mutable alreadyHave: bool = false
            let mutable i: int = 0

            while i < tValuePairsStore.Count && not alreadyHave do
                if Geometry.snap0(t1 - tValuePairsStore.[i]) = 0.0 || Geometry.snap0(t2 - tValuePairsStore.[i + 1]) = 0.0 then
                    alreadyHave <- true

                i <- i + 2

            if not alreadyHave then
                tValuePairsStore.Add(t1)
                tValuePairsStore.Add(t2)

            this

    member this.List() : float[] =
        let result: float[] = tValuePairsStore.ToArray()
        let count: int = result.Length / 2

        for i = 1 to count - 1 do
            let keyA: float = result.[i * 2]
            let keyB: float = result.[i * 2 + 1]
            let mutable j: int = i - 1

            while j >= 0 && result.[j * 2] > keyA do
                result.[(j + 1) * 2] <- result.[j * 2]
                result.[(j + 1) * 2 + 1] <- result.[j * 2 + 1]
                j <- j - 1

            result.[(j + 1) * 2] <- keyA
            result.[(j + 1) * 2 + 1] <- keyB

        result

    member this.``done``() : SegmentIntersection option =
        if tValuePairsStore.Count <= 0 then
            None
        else
            Some(SegmentIntersection.FromTValuePairs(this.List()))

type internal PolyBoolReceiver(
    ?beginPath: unit -> unit,
    ?moveTo: float * float -> unit,
    ?lineTo: float * float -> unit,
    ?closePath: unit -> unit
) =
    let beginPathValue: unit -> unit = defaultArg beginPath (fun () -> ())
    let moveToValue: float * float -> unit = defaultArg moveTo (fun _ -> ())
    let lineToValue: float * float -> unit = defaultArg lineTo (fun _ -> ())
    let closePathValue: unit -> unit = defaultArg closePath (fun () -> ())

    member _.BeginPath() : unit = beginPathValue ()
    member _.MoveTo(x: float, y: float) : unit = moveToValue(x, y)
    member _.LineTo(x: float, y: float) : unit = lineToValue(x, y)
    member _.ClosePath() : unit = closePathValue ()

type Segment(p0x: float, p0y: float, p1x: float, p1y: float) =
    let mutable p0xValue: float = p0x
    let mutable p0yValue: float = p0y
    let mutable p1xValue: float = p1x
    let mutable p1yValue: float = p1y

    member this.P0X
        with get () : float = p0xValue
        and set (value: float) : unit = p0xValue <- value

    member this.P0Y
        with get () : float = p0yValue
        and set (value: float) : unit = p0yValue <- value

    member this.P1X
        with get () : float = p1xValue
        and set (value: float) : unit = p1xValue <- value

    member this.P1Y
        with get () : float = p1yValue
        and set (value: float) : unit = p1yValue <- value

    member this.SetStart(x: float, y: float) : unit =
        this.P0X <- x
        this.P0Y <- y

    member this.SetEnd(x: float, y: float) : unit =
        this.P1X <- x
        this.P1Y <- y

    member this.PointTo(t: float) : unit =
        if t = 0.0 then
            Geometry.resultX <- this.P0X
            Geometry.resultY <- this.P0Y
        elif t = 1.0 then
            Geometry.resultX <- this.P1X
            Geometry.resultY <- this.P1Y
        else
            Geometry.resultX <- this.P0X + (this.P1X - this.P0X) * t
            Geometry.resultY <- this.P0Y + (this.P1Y - this.P0Y) * t

    member this.Split(ts: float[]) : Segment[] =
        if ts.Length <= 0 then
            [| this |]
        else
            let result: ResizeArray<Segment> = ResizeArray<Segment>()
            let mutable lastX: float = this.P0X
            let mutable lastY: float = this.P0Y

            for t: float in ts do
                this.PointTo(t)
                let px: float = Geometry.resultX
                let py: float = Geometry.resultY
                result.Add(Segment(lastX, lastY, px, py))
                lastX <- px
                lastY <- py

            result.Add(Segment(lastX, lastY, this.P1X, this.P1Y))
            result.ToArray()

    member this.Reverse() : Segment =
        Segment(this.P1X, this.P1Y, this.P0X, this.P0Y)

    member this.PointOn(px: float, py: float) : bool =
        Geometry.isCollinear(px, py, this.P0X, this.P0Y, this.P1X, this.P1Y)

module SegmentFunctions =
    let private projectPointOntoSegment(px: float, py: float, seg: Segment) : float =
        let dx: float = seg.P1X - seg.P0X
        let dy: float = seg.P1Y - seg.P0Y
        let ppx: float = px - seg.P0X
        let ppy: float = py - seg.P0Y
        let dist: float = dx * dx + dy * dy
        let dot: float = ppx * dx + ppy * dy
        dot / dist

    let internal segmentsIntersect(segA: Segment, segB: Segment, allowOutOfRange: bool) : SegmentIntersection option =
        let a0x: float = segA.P0X
        let a0y: float = segA.P0Y
        let a1x: float = segA.P1X
        let a1y: float = segA.P1Y
        let b0x: float = segB.P0X
        let b0y: float = segB.P0Y
        let b1x: float = segB.P1X
        let b1y: float = segB.P1Y
        let adx: float = a1x - a0x
        let ady: float = a1y - a0y
        let bdx: float = b1x - b0x
        let bdy: float = b1y - b0y
        let axb: float = adx * bdy - ady * bdx

        if Geometry.snap0 axb = 0.0 then
            if not (Geometry.isCollinear(a0x, a0y, a1x, a1y, b0x, b0y)) then
                None
            else
                let tB0onA: float = projectPointOntoSegment(b0x, b0y, segA)
                let tB1onA: float = projectPointOntoSegment(b1x, b1y, segA)
                let tAMin: float = Geometry.snap01(Math.Min(tB0onA, tB1onA))
                let tAMax: float = Geometry.snap01(Math.Max(tB0onA, tB1onA))

                if tAMax < 0.0 || tAMin > 1.0 then
                    None
                else
                    let tA0onB: float = projectPointOntoSegment(a0x, a0y, segB)
                    let tA1onB: float = projectPointOntoSegment(a1x, a1y, segB)
                    let tBMin: float = Geometry.snap01(Math.Min(tA0onB, tA1onB))
                    let tBMax: float = Geometry.snap01(Math.Max(tA0onB, tA1onB))

                    if tBMax < 0.0 || tBMin > 1.0 then
                        None
                    else
                        Some(
                            SegmentIntersection.FromTRangePairs(
                                Math.Max(0.0, tAMin), Math.Max(0.0, tBMin),
                                Math.Min(1.0, tAMax), Math.Min(1.0, tBMax)
                            )
                        )
        else
            let dx: float = a0x - b0x
            let dy: float = a0y - b0y

            SegmentTValuePairsBuilder(allowOutOfRange)
                .Add((bdx * dy - bdy * dx) / axb, (adx * dy - ady * dx) / axb)
                .``done``()
