namespace PolyBool

open System

//
// polybool - Boolean operations on polygons (union, intersection, etc)
// by Sean Connelly (@velipso), https://sean.fun
// Project Home: https://github.com/velipso/polybool
// SPDX-License-Identifier: 0BSD
//

[<AbstractClass; AllowNullLiteral>]
type SegmentIntersection(kind: string) =
    member this.kind: string = kind

type SegmentTValuePairs(tValuePairs: Vec2[]) =
    inherit SegmentIntersection("tValuePairs")

    member val tValuePairs: Vec2[] = tValuePairs with get, set

type SegmentTRangePairs(tStart: Vec2, tEnd: Vec2) =
    inherit SegmentIntersection("tRangePairs")

    member val tStart: Vec2 = tStart with get, set
    member val tEnd: Vec2 = tEnd with get, set

type SegmentTValuePairsBuilder(allowOutOfRange: bool, geo: Geometry) =
    let tValuePairsStore: ResizeArray<Vec2> = ResizeArray<Vec2>()

    member this.tValuePairs: ResizeArray<Vec2> = tValuePairsStore
    member this.allowOutOfRange: bool = allowOutOfRange
    member this.geo: Geometry = geo

    member this.add(t1: float, t2: float) : SegmentTValuePairsBuilder =
        let t1: float = this.geo.snap01(t1)
        let t2: float = this.geo.snap01(t2)

        if not this.allowOutOfRange && (t1 < 0.0 || t1 > 1.0 || t2 < 0.0 || t2 > 1.0) then
            this
        else
            let mutable alreadyHave: bool = false
            let mutable i: int = 0

            while i < tValuePairsStore.Count && not alreadyHave do
                let tv: Vec2 = tValuePairsStore.[i]

                if this.geo.snap0(t1 - tv.[0]) = 0.0 || this.geo.snap0(t2 - tv.[1]) = 0.0 then
                    alreadyHave <- true

                i <- i + 1

            if not alreadyHave then
                tValuePairsStore.Add([| t1; t2 |])

            this

    member this.list() : Vec2[] =
        let result: Vec2[] = tValuePairsStore.ToArray()
        Array.sortInPlaceBy (fun (pair: Vec2) -> pair.[0]) result
        result

    member this.``done``() : SegmentIntersection option =
        if tValuePairsStore.Count <= 0 then
            None
        else
            Some(SegmentTValuePairs(this.list()) :> SegmentIntersection)

type IPolyBoolReceiver =
    abstract beginPath: unit -> unit
    abstract moveTo: float * float -> unit
    abstract lineTo: float * float -> unit
    abstract closePath: unit -> unit

[<AbstractClass; AllowNullLiteral>]
type Segment(geo: Geometry) =
    member this.geo: Geometry = geo

    abstract start: unit -> Vec2
    abstract ``end``: unit -> Vec2
    abstract setStart: Vec2 -> unit
    abstract setEnd: Vec2 -> unit
    abstract point: float -> Vec2
    abstract split: float[] -> Segment[]
    abstract reverse: unit -> Segment
    abstract pointOn: Vec2 -> bool

[<AllowNullLiteral>]
type SegmentLine(p0: Vec2, p1: Vec2, geo: Geometry) =
    inherit Segment(geo)

    let mutable p0Value: Vec2 = p0
    let mutable p1Value: Vec2 = p1

    member this.p0
        with get () : Vec2 = p0Value
        and set (value: Vec2) : unit = p0Value <- value

    member this.p1
        with get () : Vec2 = p1Value
        and set (value: Vec2) : unit = p1Value <- value

    override this.start() : Vec2 =
        this.p0

    override this.``end``() : Vec2 =
        this.p1

    override this.setStart(p0: Vec2) : unit =
        this.p0 <- p0

    override this.setEnd(p1: Vec2) : unit =
        this.p1 <- p1

    override this.point(t: float) : Vec2 =
        let p0: Vec2 = this.p0
        let p1: Vec2 = this.p1

        if t = 0.0 then
            p0
        elif t = 1.0 then
            p1
        else
            [|
                p0.[0] + (p1.[0] - p0.[0]) * t
                p0.[1] + (p1.[1] - p0.[1]) * t
            |]

    override this.split(ts: float[]) : Segment[] =
        if ts.Length <= 0 then
            [| this :> Segment |]
        else
            let pts: Vec2[] = ts |> Array.map this.point
            let result: ResizeArray<Segment> = ResizeArray<Segment>()
            let mutable last: Vec2 = this.p0

            for p: Vec2 in pts do
                result.Add(SegmentLine(last, p, this.geo) :> Segment)
                last <- p

            result.Add(SegmentLine(last, this.p1, this.geo) :> Segment)
            result.ToArray()

    override this.reverse() : Segment =
        SegmentLine(this.p1, this.p0, this.geo) :> Segment

    override this.pointOn(p: Vec2) : bool =
        this.geo.isCollinear(p, this.p0, this.p1)

module SegmentFunctions =
    let projectPointOntoSegmentLine(p: Vec2, seg: SegmentLine) : float =
        let dx: float = seg.p1.[0] - seg.p0.[0]
        let dy: float = seg.p1.[1] - seg.p0.[1]
        let px: float = p.[0] - seg.p0.[0]
        let py: float = p.[1] - seg.p0.[1]
        let dist: float = dx * dx + dy * dy
        let dot: float = px * dx + py * dy
        dot / dist

    let segmentLineIntersectSegmentLine(
        segA: SegmentLine,
        segB: SegmentLine,
        allowOutOfRange: bool
    ) : SegmentIntersection option =
        let geo: Geometry = segA.geo
        let a0: Vec2 = segA.p0
        let a1: Vec2 = segA.p1
        let b0: Vec2 = segB.p0
        let b1: Vec2 = segB.p1
        let adx: float = a1.[0] - a0.[0]
        let ady: float = a1.[1] - a0.[1]
        let bdx: float = b1.[0] - b0.[0]
        let bdy: float = b1.[1] - b0.[1]
        let axb: float = adx * bdy - ady * bdx

        if geo.snap0(axb) = 0.0 then
            if not (geo.isCollinear(a0, a1, b0)) then
                None
            else
                let tB0onA: float = projectPointOntoSegmentLine(segB.p0, segA)
                let tB1onA: float = projectPointOntoSegmentLine(segB.p1, segA)
                let tAMin: float = geo.snap01(Math.Min(tB0onA, tB1onA))
                let tAMax: float = geo.snap01(Math.Max(tB0onA, tB1onA))

                if tAMax < 0.0 || tAMin > 1.0 then
                    None
                else
                    let tA0onB: float = projectPointOntoSegmentLine(segA.p0, segB)
                    let tA1onB: float = projectPointOntoSegmentLine(segA.p1, segB)
                    let tBMin: float = geo.snap01(Math.Min(tA0onB, tA1onB))
                    let tBMax: float = geo.snap01(Math.Max(tA0onB, tA1onB))

                    if tBMax < 0.0 || tBMin > 1.0 then
                        None
                    else
                        Some(
                            SegmentTRangePairs(
                                [| Math.Max(0.0, tAMin); Math.Max(0.0, tBMin) |],
                                [| Math.Min(1.0, tAMax); Math.Min(1.0, tBMax) |]
                            )
                            :> SegmentIntersection
                        )
        else
            let dx: float = a0.[0] - b0.[0]
            let dy: float = a0.[1] - b0.[1]

            SegmentTValuePairsBuilder(allowOutOfRange, geo)
                .add((bdx * dy - bdy * dx) / axb, (adx * dy - ady * dx) / axb)
                .``done``()

    let segmentsIntersect(segA: Segment, segB: Segment, allowOutOfRange: bool) : SegmentIntersection option =
        match segA, segB with
        | (:? SegmentLine as segA), (:? SegmentLine as segB) ->
            segmentLineIntersectSegmentLine(segA, segB, allowOutOfRange)
        | _ ->
            failwith "PolyBool: Unknown segment instance in segmentsIntersect"