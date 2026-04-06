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

type internal SegmentIntersection private (kind: SegmentIntersectionKind, ?tValuePairs: Vec2[], ?tStart: Vec2, ?tEnd: Vec2) =
    member this.Kind: SegmentIntersectionKind = kind

    member this.TValuePairs: Vec2[] =
        match kind, tValuePairs with
        | TValuePairsKind, Some value ->
            value
        | _ ->
            failwith "PolyBool: Intersection does not contain tValuePairs"

    member this.TStart: Vec2 =
        match kind, tStart with
        | TRangePairsKind, Some value ->
            value
        | _ ->
            failwith "PolyBool: Intersection does not contain tStart"

    member this.TEnd: Vec2 =
        match kind, tEnd with
        | TRangePairsKind, Some value ->
            value
        | _ ->
            failwith "PolyBool: Intersection does not contain tEnd"

    static member FromTValuePairs(tValuePairs: Vec2[]) : SegmentIntersection =
        SegmentIntersection(TValuePairsKind, tValuePairs = tValuePairs)

    static member FromTRangePairs(tStart: Vec2, tEnd: Vec2) : SegmentIntersection =
        SegmentIntersection(TRangePairsKind, tStart = tStart, tEnd = tEnd)

type internal SegmentTValuePairsBuilder(allowOutOfRange: bool) =
    let tValuePairsStore: ResizeArray<Vec2> = ResizeArray<Vec2>()

    member this.TValuePairs: ResizeArray<Vec2> = tValuePairsStore
    member this.AllowOutOfRange: bool = allowOutOfRange


    member this.Add(t1: float, t2: float) : SegmentTValuePairsBuilder =
        let t1: float = Geometry.snap01(t1)
        let t2: float = Geometry.snap01(t2)

        if not this.AllowOutOfRange && (t1 < 0.0 || t1 > 1.0 || t2 < 0.0 || t2 > 1.0) then
            this
        else
            let mutable alreadyHave: bool = false
            let mutable i: int = 0

            while i < tValuePairsStore.Count && not alreadyHave do
                let tv: Vec2 = tValuePairsStore.[i]

                if Geometry.snap0(t1 - tv.[0]) = 0.0 || Geometry.snap0(t2 - tv.[1]) = 0.0 then
                    alreadyHave <- true

                i <- i + 1

            if not alreadyHave then
                tValuePairsStore.Add([| t1; t2 |])

            this

    member this.List() : Vec2[] =
        let result: Vec2[] = tValuePairsStore.ToArray()
        Array.sortInPlaceBy (fun (pair: Vec2) -> pair.[0]) result
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

type Segment(p0: Vec2, p1: Vec2) =
    let mutable p0Value: Vec2 = p0
    let mutable p1Value: Vec2 = p1



    member this.P0
        with get () : Vec2 = p0Value
        and set (value: Vec2) : unit = p0Value <- value

    member this.P1
        with get () : Vec2 = p1Value
        and set (value: Vec2) : unit = p1Value <- value

    member this.Start() : Vec2 =
        this.P0

    member this.``end``() : Vec2 =
        this.P1

    member this.SetStart(p0: Vec2) : unit =
        this.P0 <- p0

    member this.SetEnd(p1: Vec2) : unit =
        this.P1 <- p1

    member this.Point(t: float) : Vec2 =
        let p0: Vec2 = this.P0
        let p1: Vec2 = this.P1

        if t = 0.0 then
            p0
        elif t = 1.0 then
            p1
        else
            [|
                p0.[0] + (p1.[0] - p0.[0]) * t
                p0.[1] + (p1.[1] - p0.[1]) * t
            |]

    member this.Split(ts: float[]) : Segment[] =
        if ts.Length <= 0 then
            [| this |]
        else
            let pts: Vec2[] = ts |> Array.map this.Point
            let result: ResizeArray<Segment> = ResizeArray<Segment>()
            let mutable last: Vec2 = this.P0

            for p: Vec2 in pts do
                result.Add(Segment(last, p))
                last <- p

            result.Add(Segment(last, this.P1))
            result.ToArray()

    member this.Reverse() : Segment =
        Segment(this.P1, this.P0)

    member this.PointOn(p: Vec2) : bool =
        Geometry.isCollinear(p, this.P0, this.P1)

module SegmentFunctions =
    let private projectPointOntoSegment(p: Vec2, seg: Segment) : float =
        let dx: float = seg.P1.[0] - seg.P0.[0]
        let dy: float = seg.P1.[1] - seg.P0.[1]
        let px: float = p.[0] - seg.P0.[0]
        let py: float = p.[1] - seg.P0.[1]
        let dist: float = dx * dx + dy * dy
        let dot: float = px * dx + py * dy
        dot / dist

    let internal segmentsIntersect(segA: Segment, segB: Segment, allowOutOfRange: bool) : SegmentIntersection option =
        let a0: Vec2 = segA.P0
        let a1: Vec2 = segA.P1
        let b0: Vec2 = segB.P0
        let b1: Vec2 = segB.P1
        let adx: float = a1.[0] - a0.[0]
        let ady: float = a1.[1] - a0.[1]
        let bdx: float = b1.[0] - b0.[0]
        let bdy: float = b1.[1] - b0.[1]
        let axb: float = adx * bdy - ady * bdx

        if Geometry.snap0(axb) = 0.0 then
            if not (Geometry.isCollinear(a0, a1, b0)) then
                None
            else
                let tB0onA: float = projectPointOntoSegment(segB.P0, segA)
                let tB1onA: float = projectPointOntoSegment(segB.P1, segA)
                let tAMin: float = Geometry.snap01(Math.Min(tB0onA, tB1onA))
                let tAMax: float = Geometry.snap01(Math.Max(tB0onA, tB1onA))

                if tAMax < 0.0 || tAMin > 1.0 then
                    None
                else
                    let tA0onB: float = projectPointOntoSegment(segA.P0, segB)
                    let tA1onB: float = projectPointOntoSegment(segA.P1, segB)
                    let tBMin: float = Geometry.snap01(Math.Min(tA0onB, tA1onB))
                    let tBMax: float = Geometry.snap01(Math.Max(tA0onB, tA1onB))

                    if tBMax < 0.0 || tBMin > 1.0 then
                        None
                    else
                        Some(
                            SegmentIntersection.FromTRangePairs(
                                [| Math.Max(0.0, tAMin); Math.Max(0.0, tBMin) |],
                                [| Math.Min(1.0, tAMax); Math.Min(1.0, tBMax) |]
                            )
                        )
        else
            let dx: float = a0.[0] - b0.[0]
            let dy: float = a0.[1] - b0.[1]

            SegmentTValuePairsBuilder(allowOutOfRange)
                .Add((bdx * dy - bdy * dx) / axb, (adx * dy - ady * dx) / axb)
                .``done``()