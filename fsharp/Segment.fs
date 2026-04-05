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

type SegmentTValuesBuilder(geo: Geometry) =
    let tValuesStore: ResizeArray<float> = ResizeArray<float>()

    member this.tValues: ResizeArray<float> = tValuesStore
    member this.geo: Geometry = geo

    member this.addArray(ts: float[]) : SegmentTValuesBuilder =
        for t: float in ts do
            tValuesStore.Add(t)

        this

    member this.add(t: float) : SegmentTValuesBuilder =
        let t: float = this.geo.snap01(t)

        // ignore values outside 0-1 range
        if t < 0.0 || t > 1.0 then
            this
        else
            let mutable alreadyHave: bool = false
            let mutable i: int = 0

            while i < tValuesStore.Count && not alreadyHave do
                if this.geo.snap0(t - tValuesStore.[i]) = 0.0 then
                    // already have this location
                    alreadyHave <- true

                i <- i + 1

            if not alreadyHave then
                tValuesStore.Add(t)

            this

    member this.list() : float[] =
        let result: float[] = tValuesStore.ToArray()
        Array.sortInPlace result
        result

type SegmentTValuePairsBuilder(allowOutOfRange: bool, geo: Geometry) =
    let tValuePairsStore: ResizeArray<Vec2> = ResizeArray<Vec2>()

    member this.tValuePairs: ResizeArray<Vec2> = tValuePairsStore
    member this.allowOutOfRange: bool = allowOutOfRange
    member this.geo: Geometry = geo

    member this.add(t1: float, t2: float) : SegmentTValuePairsBuilder =
        let t1: float = this.geo.snap01(t1)
        let t2: float = this.geo.snap01(t2)

        // ignore values outside 0-1 range
        if not this.allowOutOfRange && (t1 < 0.0 || t1 > 1.0 || t2 < 0.0 || t2 > 1.0) then
            this
        else
            let mutable alreadyHave: bool = false
            let mutable i: int = 0

            while i < tValuePairsStore.Count && not alreadyHave do
                let tv: Vec2 = tValuePairsStore.[i]

                if this.geo.snap0(t1 - tv.[0]) = 0.0 || this.geo.snap0(t2 - tv.[1]) = 0.0 then
                    // already have this location
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
    abstract bezierCurveTo: float * float * float * float * float * float -> unit
    abstract closePath: unit -> unit


[<AbstractClass; AllowNullLiteral>]
type Segment(geo: Geometry) =
    member this.geo: Geometry = geo

    abstract copy: unit -> Segment
    abstract isEqual: Segment -> bool
    abstract start: unit -> Vec2
    abstract start2: unit -> Vec2
    abstract end2: unit -> Vec2
    abstract ``end``: unit -> Vec2
    abstract setStart: Vec2 -> unit
    abstract setEnd: Vec2 -> unit
    abstract point: float -> Vec2
    abstract split: float[] -> Segment[]
    abstract reverse: unit -> Segment
    abstract boundingBox: unit -> BBox
    abstract pointOn: Vec2 -> bool
    abstract draw: IPolyBoolReceiver -> IPolyBoolReceiver

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

    override this.copy() : Segment =
        SegmentLine(this.p0, this.p1, this.geo) :> Segment

    override this.isEqual(other: Segment) : bool =
        match other with
        | :? SegmentLine as other ->
            this.geo.isEqualVec2(this.p0, other.p0)
            && this.geo.isEqualVec2(this.p1, other.p1)
        | _ ->
            false

    override this.start() : Vec2 =
        this.p0

    override this.start2() : Vec2 =
        this.p1

    override this.end2() : Vec2 =
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

    override this.boundingBox() : BBox =
        let p0: Vec2 = this.p0
        let p1: Vec2 = this.p1

        (
            [| Math.Min(p0.[0], p1.[0]); Math.Min(p0.[1], p1.[1]) |],
            [| Math.Max(p0.[0], p1.[0]); Math.Max(p0.[1], p1.[1]) |]
        )

    override this.pointOn(p: Vec2) : bool =
        this.geo.isCollinear(p, this.p0, this.p1)

    override this.draw(ctx: IPolyBoolReceiver) : IPolyBoolReceiver =
        let p0: Vec2 = this.p0
        let p1: Vec2 = this.p1
        ctx.moveTo(p0.[0], p0.[1])
        ctx.lineTo(p1.[0], p1.[1])
        ctx

[<AllowNullLiteral>]
type SegmentCurve(p0: Vec2, p1: Vec2, p2: Vec2, p3: Vec2, geo: Geometry) =
    inherit Segment(geo)

    let mutable p0Value: Vec2 = p0
    let mutable p1Value: Vec2 = p1
    let mutable p2Value: Vec2 = p2
    let mutable p3Value: Vec2 = p3

    member this.p0
        with get () : Vec2 = p0Value
        and set (value: Vec2) : unit = p0Value <- value

    member this.p1
        with get () : Vec2 = p1Value
        and set (value: Vec2) : unit = p1Value <- value

    member this.p2
        with get () : Vec2 = p2Value
        and set (value: Vec2) : unit = p2Value <- value

    member this.p3
        with get () : Vec2 = p3Value
        and set (value: Vec2) : unit = p3Value <- value

    override this.copy() : Segment =
        SegmentCurve(this.p0, this.p1, this.p2, this.p3, this.geo) :> Segment

    override this.isEqual(other: Segment) : bool =
        match other with
        | :? SegmentCurve as other ->
            this.geo.isEqualVec2(this.p0, other.p0)
            && this.geo.isEqualVec2(this.p1, other.p1)
            && this.geo.isEqualVec2(this.p2, other.p2)
            && this.geo.isEqualVec2(this.p3, other.p3)
        | _ ->
            false

    override this.start() : Vec2 =
        this.p0

    override this.start2() : Vec2 =
        this.p1

    override this.end2() : Vec2 =
        this.p2

    override this.``end``() : Vec2 =
        this.p3

    override this.setStart(p0: Vec2) : unit =
        this.p0 <- p0

    override this.setEnd(p3: Vec2) : unit =
        this.p3 <- p3

    override this.point(t: float) : Vec2 =
        let p0: Vec2 = this.p0
        let p1: Vec2 = this.p1
        let p2: Vec2 = this.p2
        let p3: Vec2 = this.p3

        if t = 0.0 then
            p0
        elif t = 1.0 then
            p3
        else
            let t1t: float = (1.0 - t) * (1.0 - t)
            let tt: float = t * t
            let t0: float = t1t * (1.0 - t)
            let t1: float = 3.0 * t1t * t
            let t2: float = 3.0 * tt * (1.0 - t)
            let t3: float = tt * t

            [|
                p0.[0] * t0 + p1.[0] * t1 + p2.[0] * t2 + p3.[0] * t3
                p0.[1] * t0 + p1.[1] * t1 + p2.[1] * t2 + p3.[1] * t3
            |]

    override this.split(ts: float[]) : Segment[] =
        if ts.Length <= 0 then
            [| this :> Segment |]
        else
            let result: ResizeArray<Segment> = ResizeArray<Segment>()

            let splitSingle (pts: Vec2 * Vec2 * Vec2 * Vec2) (t: float) : Vec2 * Vec2 * Vec2 * Vec2 =
                let p0, p1, p2, p3 = pts
                let p4: Vec2 = GeometryFunctions.lerpVec2 p0 p1 t
                let p5: Vec2 = GeometryFunctions.lerpVec2 p1 p2 t
                let p6: Vec2 = GeometryFunctions.lerpVec2 p2 p3 t
                let p7: Vec2 = GeometryFunctions.lerpVec2 p4 p5 t
                let p8: Vec2 = GeometryFunctions.lerpVec2 p5 p6 t
                let p9: Vec2 = GeometryFunctions.lerpVec2 p7 p8 t
                result.Add(SegmentCurve(p0, p4, p7, p9, this.geo) :> Segment)
                (p9, p8, p6, p3)

            let mutable last: Vec2 * Vec2 * Vec2 * Vec2 = (this.p0, this.p1, this.p2, this.p3)
            let mutable lastT: float = 0.0

            for t: float in ts do
                last <- splitSingle last ((t - lastT) / (1.0 - lastT))
                lastT <- t

            let l0, l1, l2, l3 = last
            result.Add(SegmentCurve(l0, l1, l2, l3, this.geo) :> Segment)
            result.ToArray()

    override this.reverse() : Segment =
        SegmentCurve(this.p3, this.p2, this.p1, this.p0, this.geo) :> Segment

    member this.getCubicCoefficients(axis: int) : float[] =
        let p0: float = this.p0.[axis]
        let p1: float = this.p1.[axis]
        let p2: float = this.p2.[axis]
        let p3: float = this.p3.[axis]

        [|
            p3 - 3.0 * p2 + 3.0 * p1 - p0
            3.0 * p2 - 6.0 * p1 + 3.0 * p0
            3.0 * p1 - 3.0 * p0
            p0
        |]

    member this.boundingTValues() : float[] =
        let result: SegmentTValuesBuilder = SegmentTValuesBuilder(this.geo)

        let bounds (x0: float) (x1: float) (x2: float) (x3: float) : SegmentTValuesBuilder =
            let a: float = 3.0 * x3 - 9.0 * x2 + 9.0 * x1 - 3.0 * x0
            let b: float = 6.0 * x0 - 12.0 * x1 + 6.0 * x2
            let c: float = 3.0 * x1 - 3.0 * x0

            if this.geo.snap0(a) = 0.0 then
                result.add(-c / b)
            else
                let disc: float = b * b - 4.0 * a * c

                if disc >= 0.0 then
                    let sq: float = Math.Sqrt(disc)
                    result.add((-b + sq) / (2.0 * a)) |> ignore
                    result.add((-b - sq) / (2.0 * a))
                else
                    result

        let p0: Vec2 = this.p0
        let p1: Vec2 = this.p1
        let p2: Vec2 = this.p2
        let p3: Vec2 = this.p3
        bounds p0.[0] p1.[0] p2.[0] p3.[0] |> ignore
        bounds p0.[1] p1.[1] p2.[1] p3.[1] |> ignore
        result.list()

    member this.inflectionTValues() : float[] =
        let result: SegmentTValuesBuilder = SegmentTValuesBuilder(this.geo)
        result.addArray(this.boundingTValues()) |> ignore

        let p0: Vec2 = this.p0
        let p1: Vec2 = this.p1
        let p2: Vec2 = this.p2
        let p3: Vec2 = this.p3
        let p10x: float = 3.0 * (p1.[0] - p0.[0])
        let p10y: float = 3.0 * (p1.[1] - p0.[1])
        let p21x: float = 6.0 * (p2.[0] - p1.[0])
        let p21y: float = 6.0 * (p2.[1] - p1.[1])
        let p32x: float = 3.0 * (p3.[0] - p2.[0])
        let p32y: float = 3.0 * (p3.[1] - p2.[1])
        let p210x: float = 6.0 * (p2.[0] - 2.0 * p1.[0] + p0.[0])
        let p210y: float = 6.0 * (p2.[1] - 2.0 * p1.[1] + p0.[1])
        let p321x: float = 6.0 * (p3.[0] - 2.0 * p2.[0] + p1.[0])
        let p321y: float = 6.0 * (p3.[1] - 2.0 * p2.[1] + p1.[1])
        let qx: float = p10x - p21x + p32x
        let qy: float = p10y - p21y + p32y
        let rx: float = p21x - 2.0 * p10x
        let ry: float = p21y - 2.0 * p10y
        let sx: float = p10x
        let sy: float = p10y
        let ux: float = p321x - p210x
        let uy: float = p321y - p210y
        let vx: float = p210x
        let vy: float = p210y
        let a: float = qx * uy - qy * ux
        let b: float = qx * vy + rx * uy - qy * vx - ry * ux
        let c: float = rx * vy + sx * uy - ry * vx - sy * ux
        let d: float = sx * vy - sy * vx

        for s: float in this.geo.solveCubic(a, b, c, d) do
            result.add(s) |> ignore

        result.list()

    override this.boundingBox() : BBox =
        let p0: Vec2 = this.p0
        let p3: Vec2 = this.p3
        let min: Vec2 = [| Math.Min(p0.[0], p3.[0]); Math.Min(p0.[1], p3.[1]) |]
        let max: Vec2 = [| Math.Max(p0.[0], p3.[0]); Math.Max(p0.[1], p3.[1]) |]

        for t: float in this.boundingTValues() do
            let p: Vec2 = this.point(t)
            min.[0] <- Math.Min(min.[0], p.[0])
            min.[1] <- Math.Min(min.[1], p.[1])
            max.[0] <- Math.Max(max.[0], p.[0])
            max.[1] <- Math.Max(max.[1], p.[1])

        (min, max)

    member this.mapXtoT(x: float, ?force: bool) : float option =
        let force: bool = defaultArg force false

        if this.geo.snap0(this.p0.[0] - x) = 0.0 then
            Some 0.0
        elif this.geo.snap0(this.p3.[0] - x) = 0.0 then
            Some 1.0
        else
            let p0: float = this.p0.[0] - x
            let p1: float = this.p1.[0] - x
            let p2: float = this.p2.[0] - x
            let p3: float = this.p3.[0] - x
            let r: float[] =
                [|
                    p3 - 3.0 * p2 + 3.0 * p1 - p0
                    3.0 * p2 - 6.0 * p1 + 3.0 * p0
                    3.0 * p1 - 3.0 * p0
                    p0
                |]

            let mutable result: float option = None
            let roots: float[] = this.geo.solveCubic(r.[0], r.[1], r.[2], r.[3])
            let mutable i: int = 0

            while i < roots.Length && Option.isNone result do
                let t: float = roots.[i]
                let ts: float = this.geo.snap01(t)

                if ts >= 0.0 && ts <= 1.0 then
                    result <- Some t

                i <- i + 1

            if Option.isSome result then
                result
            elif force || (x >= Math.Min(this.p0.[0], this.p3.[0]) && x <= Math.Max(this.p0.[0], this.p3.[0])) then
                let mutable attempt: int = 0

                while attempt < 4 && Option.isNone result do
                    // collapse an R value to 0, this is so wrong!!!
                    let mutable ii: int = -1

                    for i = 0 to 3 do
                        if r.[i] <> 0.0 && (ii < 0 || Math.Abs(r.[i]) < Math.Abs(r.[ii])) then
                            ii <- i

                    if ii < 0 then
                        result <- Some 0.0
                    else
                        r.[ii] <- 0.0
                        let roots: float[] = this.geo.solveCubic(r.[0], r.[1], r.[2], r.[3])
                        let mutable j: int = 0

                        while j < roots.Length && Option.isNone result do
                            let t: float = roots.[j]
                            let ts: float = this.geo.snap01(t)

                            if ts >= 0.0 && ts <= 1.0 then
                                result <- Some t

                            j <- j + 1

                    attempt <- attempt + 1

                result
            else
                None

    member this.mapXtoY(x: float, ?force: bool) : float option =
        match this.mapXtoT(x, defaultArg force false) with
        | Some t ->
            Some(this.point(t).[1])
        | None ->
            None

    override this.pointOn(p: Vec2) : bool =
        if this.geo.isEqualVec2(this.p0, p) || this.geo.isEqualVec2(this.p3, p) then
            true
        else
            match this.mapXtoY(p.[0]) with
            | Some y ->
                this.geo.snap0(y - p.[1]) = 0.0
            | None ->
                false

    member this.toLine() : SegmentLine option =
        // note: this won't work for arbitrary curves, because they could loop back on themselves,
        // but will work fine for curves that have already been split at all inflection points
        let p0: Vec2 = this.p0
        let p1: Vec2 = this.p1
        let p2: Vec2 = this.p2
        let p3: Vec2 = this.p3

        if
            // vertical line
            (this.geo.snap0(p0.[0] - p1.[0]) = 0.0
             && this.geo.snap0(p0.[0] - p2.[0]) = 0.0
             && this.geo.snap0(p0.[0] - p3.[0]) = 0.0)
            ||
            // horizontal line
            (this.geo.snap0(p0.[1] - p1.[1]) = 0.0
             && this.geo.snap0(p0.[1] - p2.[1]) = 0.0
             && this.geo.snap0(p0.[1] - p3.[1]) = 0.0)
        then
            Some(SegmentLine(p0, p3, this.geo))
        else
            None

    override this.draw(ctx: IPolyBoolReceiver) : IPolyBoolReceiver =
        let p0: Vec2 = this.p0
        let p1: Vec2 = this.p1
        let p2: Vec2 = this.p2
        let p3: Vec2 = this.p3
        ctx.moveTo(p0.[0], p0.[1])
        ctx.bezierCurveTo(p1.[0], p1.[1], p2.[0], p2.[1], p3.[0], p3.[1])
        ctx

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
            // lines are coincident or parallel
            if not (geo.isCollinear(a0, a1, b0)) then
                // they're not coincident, so they're parallel, with no intersections
                None
            else
                // otherwise, segments are on top of each other somehow (aka coincident)
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
            // otherwise, not coincident, so they intersect somewhere
            let dx: float = a0.[0] - b0.[0]
            let dy: float = a0.[1] - b0.[1]

            SegmentTValuePairsBuilder(allowOutOfRange, geo)
                .add((bdx * dy - bdy * dx) / axb, (adx * dy - ady * dx) / axb)
                .``done``()

    let segmentLineIntersectSegmentCurve(
        segA: SegmentLine,
        segB: SegmentCurve,
        allowOutOfRange: bool,
        invert: bool
    ) : SegmentIntersection option =
        let geo: Geometry = segA.geo
        let a0: Vec2 = segA.p0
        let a1: Vec2 = segA.p1
        let a: float = a1.[1] - a0.[1]
        let b: float = a0.[0] - a1.[0]

        if geo.snap0(b) = 0.0 then
            // vertical line
            match segB.mapXtoT(a0.[0], false) with
            | Some t ->
                let y: float = segB.point(t).[1]
                let s: float = (y - a0.[1]) / a
                let result: SegmentTValuePairsBuilder = SegmentTValuePairsBuilder(allowOutOfRange, geo)

                if invert then
                    result.add(t, s) |> ignore
                else
                    result.add(s, t) |> ignore

                result.``done``()
            | None ->
                None
        else
            let c: float = a * a0.[0] + b * a0.[1]
            let bx: float[] = segB.getCubicCoefficients(0)
            let by: float[] = segB.getCubicCoefficients(1)
            let rA: float = a * bx.[0] + b * by.[0]
            let rB: float = a * bx.[1] + b * by.[1]
            let rC: float = a * bx.[2] + b * by.[2]
            let rD: float = a * bx.[3] + b * by.[3] - c
            let roots: float[] = geo.solveCubic(rA, rB, rC, rD)
            let result: SegmentTValuePairsBuilder = SegmentTValuePairsBuilder(allowOutOfRange, geo)

            if geo.snap0(a) = 0.0 then
                // project curve's X component onto line
                for t: float in roots do
                    let xValue: float = bx.[0] * t * t * t + bx.[1] * t * t + bx.[2] * t + bx.[3]
                    let s: float = (a0.[0] - xValue) / b

                    if invert then
                        result.add(t, s) |> ignore
                    else
                        result.add(s, t) |> ignore
            else
                // project curve's Y component onto line
                for t: float in roots do
                    let yValue: float = by.[0] * t * t * t + by.[1] * t * t + by.[2] * t + by.[3]
                    let s: float = (yValue - a0.[1]) / a

                    if invert then
                        result.add(t, s) |> ignore
                    else
                        result.add(s, t) |> ignore

            result.``done``()

    let segmentCurveIntersectSegmentCurve(
        segA: SegmentCurve,
        segB: SegmentCurve,
        allowOutOfRange: bool
    ) : SegmentIntersection option =
        let geo: Geometry = segA.geo

        // dummy coincident calculation for now
        // TODO: implement actual range/equality testing
        if geo.isEqualVec2(segA.p0, segB.p0) then
            if geo.isEqualVec2(segA.p3, segB.p3) then
                if geo.isEqualVec2(segA.p1, segB.p1) && geo.isEqualVec2(segA.p2, segB.p2) then
                    Some(SegmentTRangePairs([| 0.0; 0.0 |], [| 1.0; 1.0 |]) :> SegmentIntersection)
                else
                    Some(SegmentTValuePairs([| [| 0.0; 0.0 |]; [| 1.0; 1.0 |] |]) :> SegmentIntersection)
            else
                Some(SegmentTValuePairs([| [| 0.0; 0.0 |] |]) :> SegmentIntersection)
        elif geo.isEqualVec2(segA.p0, segB.p3) then
            Some(SegmentTValuePairs([| [| 0.0; 1.0 |] |]) :> SegmentIntersection)
        elif geo.isEqualVec2(segA.p3, segB.p0) then
            Some(SegmentTValuePairs([| [| 1.0; 0.0 |] |]) :> SegmentIntersection)
        elif geo.isEqualVec2(segA.p3, segB.p3) then
            Some(SegmentTValuePairs([| [| 1.0; 1.0 |] |]) :> SegmentIntersection)
        else
            let result: SegmentTValuePairsBuilder = SegmentTValuePairsBuilder(allowOutOfRange, geo)

            let rec checkCurves(
                c1: SegmentCurve,
                t1L: float,
                t1R: float,
                c2: SegmentCurve,
                t2L: float,
                t2R: float
            ) : unit =
                let bbox1: BBox = c1.boundingBox()
                let bbox2: BBox = c2.boundingBox()

                if GeometryFunctions.boundingBoxesIntersect bbox1 bbox2 then
                    let t1M: float = (t1L + t1R) / 2.0
                    let t2M: float = (t2L + t2R) / 2.0

                    if geo.snap0(t1R - t1L) = 0.0 && geo.snap0(t2R - t2L) = 0.0 then
                        result.add(t1M, t2M) |> ignore
                    else
                        let c1Split: Segment[] = c1.split([| 0.5 |])
                        let c2Split: Segment[] = c2.split([| 0.5 |])
                        let c1L: SegmentCurve = c1Split.[0] :?> SegmentCurve
                        let c1R: SegmentCurve = c1Split.[1] :?> SegmentCurve
                        let c2L: SegmentCurve = c2Split.[0] :?> SegmentCurve
                        let c2R: SegmentCurve = c2Split.[1] :?> SegmentCurve
                        checkCurves(c1L, t1L, t1M, c2L, t2L, t2M)
                        checkCurves(c1R, t1M, t1R, c2L, t2L, t2M)
                        checkCurves(c1L, t1L, t1M, c2R, t2M, t2R)
                        checkCurves(c1R, t1M, t1R, c2R, t2M, t2R)

            checkCurves(segA, 0.0, 1.0, segB, 0.0, 1.0)
            result.``done``()

    // return value:
    //   null               => no intersection
    //   SegmentTValuePairs => the segments intersect along a series of points, whose position is
    //                         represented by T values pairs [segA_tValue, segB_tValue]
    //                         note: a T value pair is returned even if it's just a shared vertex!
    //   SegmentTRangePairs => the segments are coincident (on top of each other), and intersect along a
    //                         segment, ranged by T values
    let segmentsIntersect(segA: Segment, segB: Segment, allowOutOfRange: bool) : SegmentIntersection option =
        match segA with
        | :? SegmentLine as segA ->
            match segB with
            | :? SegmentLine as segB ->
                segmentLineIntersectSegmentLine(segA, segB, allowOutOfRange)
            | :? SegmentCurve as segB ->
                segmentLineIntersectSegmentCurve(segA, segB, allowOutOfRange, false)
            | _ ->
                failwith "PolyBool: Unknown segment instance in segmentsIntersect"
        | :? SegmentCurve as segA ->
            match segB with
            | :? SegmentLine as segB ->
                segmentLineIntersectSegmentCurve(segB, segA, allowOutOfRange, true)
            | :? SegmentCurve as segB ->
                segmentCurveIntersectSegmentCurve(segA, segB, allowOutOfRange)
            | _ ->
                failwith "PolyBool: Unknown segment instance in segmentsIntersect"
        | _ ->
            failwith "PolyBool: Unknown segment instance in segmentsIntersect"
