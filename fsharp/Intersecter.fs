namespace PolyBool

open System

//
// polybool - Boolean operations on polygons (union, intersection, etc)
// by Sean Connelly (@velipso), https://sean.fun
// Project Home: https://github.com/velipso/polybool
// SPDX-License-Identifier: 0BSD
//

type SegmentBoolFill =
    {
        mutable above: bool option
        mutable below: bool option
    }

type ListBoolTransition<'T> =
    {
        before: 'T option
        after: 'T option
        insert: 'T -> 'T
    }

[<AbstractClass>]
type SegmentBool(data: Segment, fill: SegmentBoolFill option, closed: bool, log: BuildLog option) =
    let idValue: int =
        match log with
        | Some log ->
            log.segmentId()
        | None ->
            -1

    let mutable dataValue: Segment = data

    let mutable myFillValue: SegmentBoolFill =
        {
            above = fill |> Option.bind (fun fill -> fill.above)
            below = fill |> Option.bind (fun fill -> fill.below)
        }

    let mutable otherFillValue: SegmentBoolFill option = None
    let mutable closedValue: bool = closed

    member this.id: int = idValue

    member this.data
        with get () : Segment = dataValue
        and set (value: Segment) : unit = dataValue <- value

    member this.myFill
        with get () : SegmentBoolFill = myFillValue
        and set (value: SegmentBoolFill) : unit = myFillValue <- value

    member this.otherFill
        with get () : SegmentBoolFill option = otherFillValue
        and set (value: SegmentBoolFill option) : unit = otherFillValue <- value

    member this.closed
        with get () : bool = closedValue
        and set (value: bool) : unit = closedValue <- value

type SegmentBoolLine(data: SegmentLine, fill: SegmentBoolFill option, closed: bool, log: BuildLog option) =
    inherit SegmentBool(data :> Segment, fill, closed, log)

type SegmentBoolCurve(data: SegmentCurve, fill: SegmentBoolFill option, closed: bool, log: BuildLog option) =
    inherit SegmentBool(data :> Segment, fill, closed, log)

module IntersecterFunctions =
    let copySegmentBool(seg: SegmentBool, log: BuildLog option) : SegmentBool =
        match seg with
        | :? SegmentBoolLine as seg ->
            SegmentBoolLine(seg.data :?> SegmentLine, Some seg.myFill, seg.closed, log) :> SegmentBool
        | :? SegmentBoolCurve as seg ->
            SegmentBoolCurve(seg.data :?> SegmentCurve, Some seg.myFill, seg.closed, log) :> SegmentBool
        | _ ->
            failwith "PolyBool: Unknown SegmentBool in copySegmentBool"

[<AllowNullLiteral>]
type EventBool(isStart: bool, p: Vec2, seg: SegmentBool, primary: bool) =
    let mutable pValue: Vec2 = p
    let mutable otherValue: EventBool = null
    let mutable statusValue: EventBool = null

    member this.isStart: bool = isStart

    member this.p
        with get () : Vec2 = pValue
        and set (value: Vec2) : unit = pValue <- value

    member this.seg: SegmentBool = seg
    member this.primary: bool = primary

    member this.other
        with get () : EventBool = otherValue
        and set (value: EventBool) : unit = otherValue <- value

    member this.status
        with get () : EventBool = statusValue
        and set (value: EventBool) : unit = statusValue <- value

type ListBool<'T when 'T: equality>() =
    let nodesValue: ResizeArray<'T> = ResizeArray<'T>()

    member this.nodes: ResizeArray<'T> = nodesValue

    member this.remove(node: 'T) : unit =
        let i: int = nodesValue.IndexOf(node)

        if i >= 0 then
            nodesValue.RemoveAt(i)

    member this.getIndex(node: 'T) : int =
        nodesValue.IndexOf(node)

    member this.isEmpty() : bool =
        nodesValue.Count <= 0

    member this.getHead() : 'T =
        nodesValue.[0]

    member this.removeHead() : unit =
        nodesValue.RemoveAt(0)

    member this.insertBefore(node: 'T, check: 'T -> int) : unit =
        this.findTransition(node, check).insert(node) |> ignore

    member this.findTransition(node: 'T, check: 'T -> int) : ListBoolTransition<'T> =
        // bisect to find the transition point
        let compare (a: 'T) (b: 'T) : int =
            check b - check a

        let mutable i: int = 0
        let mutable high: int = nodesValue.Count

        while i < high do
            let mid: int = (i + high) >>> 1

            if compare nodesValue.[mid] node > 0 then
                high <- mid
            else
                i <- mid + 1

        {
            before = if i <= 0 then None else Some nodesValue.[i - 1]
            after = if i >= nodesValue.Count then None else Some nodesValue.[i]
            insert =
                fun node ->
                    nodesValue.Insert(i, node)
                    node
        }

type Intersecter(selfIntersection: bool, geo: Geometry, ?log: BuildLog) =
    let eventsValue: ListBool<EventBool> = ListBool<EventBool>()
    let statusValue: ListBool<EventBool> = ListBool<EventBool>()
    let mutable currentPath: ResizeArray<SegmentBool> = ResizeArray<SegmentBool>()
    let logValue: BuildLog option = log

    member this.selfIntersection: bool = selfIntersection
    member this.geo: Geometry = geo
    member this.events: ListBool<EventBool> = eventsValue
    member this.status: ListBool<EventBool> = statusValue
    member this.log: BuildLog option = logValue

    member private this.segmentOrFalse(ev: EventBool option) : obj =
        match ev with
        | Some ev ->
            box ev.seg
        | None ->
            box false

    member this.compareEvents(
        aStart: bool,
        a1: Vec2,
        a2: Vec2,
        aSeg: Segment,
        bStart: bool,
        b1: Vec2,
        b2: Vec2,
        bSeg: Segment
    ) : int =
        // compare the selected points first
        let comp: int = this.geo.compareVec2(a1, b1)

        if comp <> 0 then
            comp
        else
            // the selected points are the same
            match aSeg, bSeg with
            | :? SegmentLine, :? SegmentLine when this.geo.isEqualVec2(a2, b2) ->
                // if the non-selected points are the same too...
                0 // then the segments are equal
            | _ ->
                if aStart <> bStart then
                    // if one is a start and the other isn't...
                    if aStart then 1 else -1 // favor the one that isn't the start
                else
                    this.compareSegments(bSeg, aSeg)

    member this.addEvent(ev: EventBool) : unit =
        this.events.insertBefore(
            ev,
            fun here ->
                if obj.ReferenceEquals(here, ev) then
                    0
                else
                    this.compareEvents(
                        ev.isStart,
                        ev.p,
                        ev.other.p,
                        ev.seg.data,
                        here.isStart,
                        here.p,
                        here.other.p,
                        here.seg.data
                    )
        )

    member this.divideEvent(ev: EventBool, t: float, p: Vec2) : EventBool =
        this.log |> Option.iter (fun log -> log.segmentDivide(ev.seg, p))

        let split: Segment[] = ev.seg.data.split([| t |])
        let left: Segment = split.[0]
        let right: Segment = split.[1]

        // set the *exact* intersection point
        left.setEnd(p)
        right.setStart(p)

        let ns: SegmentBool =
            match right with
            | :? SegmentLine as right ->
                SegmentBoolLine(right, Some ev.seg.myFill, ev.seg.closed, this.log) :> SegmentBool
            | :? SegmentCurve as right ->
                SegmentBoolCurve(right, Some ev.seg.myFill, ev.seg.closed, this.log) :> SegmentBool
            | _ ->
                failwith "PolyBool: Unknown segment data in divideEvent"

        // slides an end backwards
        //   (start)------------(end)    to:
        //   (start)---(end)
        this.events.remove(ev.other)
        ev.seg.data <- left
        this.log |> Option.iter (fun log -> log.segmentChop(ev.seg))
        ev.other.p <- p
        this.addEvent(ev.other)
        this.addSegment(ns, ev.primary)

    member this.beginPath() : unit =
        currentPath <- ResizeArray<SegmentBool>()

    member this.closePath() : unit =
        for seg: SegmentBool in currentPath do
            seg.closed <- true

    member this.addSegment(seg: SegmentBool, primary: bool) : EventBool =
        let evStart: EventBool = EventBool(true, seg.data.start(), seg, primary)
        let evEnd: EventBool = EventBool(false, seg.data.``end``(), seg, primary)
        evStart.other <- evEnd
        evEnd.other <- evStart
        this.addEvent(evStart)
        this.addEvent(evEnd)
        evStart

    member this.addLine(fromPoint: Vec2, toPoint: Vec2, ?primary: bool) : unit =
        let primary: bool = defaultArg primary true
        let f: int = this.geo.compareVec2(fromPoint, toPoint)

        if f <> 0 then
            let startPoint: Vec2 = if f < 0 then fromPoint else toPoint
            let endPoint: Vec2 = if f < 0 then toPoint else fromPoint
            let seg: SegmentBool =
                SegmentBoolLine(
                    SegmentLine(startPoint, endPoint, this.geo),
                    None,
                    false,
                    this.log
                )
                :> SegmentBool

            currentPath.Add(seg)
            this.addSegment(seg, primary) |> ignore

    member this.addCurve(fromPoint: Vec2, c1: Vec2, c2: Vec2, toPoint: Vec2, ?primary: bool) : unit =
        let primary: bool = defaultArg primary true
        let original: SegmentCurve = SegmentCurve(fromPoint, c1, c2, toPoint, this.geo)
        let curves: Segment[] = original.split(original.inflectionTValues())

        for curveSegment: Segment in curves do
            let curve: SegmentCurve = curveSegment :?> SegmentCurve
            let f: int = this.geo.compareVec2(curve.start(), curve.``end``())

            if f <> 0 then
                // convert horizontal/vertical curves to lines
                match curve.toLine() with
                | Some line ->
                    this.addLine(line.p0, line.p1, primary)
                | None ->
                    let curveValue: SegmentCurve =
                        if f < 0 then
                            curve
                        else
                            curve.reverse() :?> SegmentCurve

                    let seg: SegmentBool =
                        SegmentBoolCurve(
                            curveValue,
                            None,
                            false,
                            this.log
                        )
                        :> SegmentBool

                    currentPath.Add(seg)
                    this.addSegment(seg, primary) |> ignore

    member this.compareSegments(seg1: Segment, seg2: Segment) : int =
        // TODO:
        //  This is where some of the curve instability comes from... we need to reliably sort
        //  segments, but this is surprisingly hard when it comes to curves.
        //
        //  The easy case is something like:
        //
        //             C   A - - - D
        //               \
        //                 \
        //                   B
        //  A is clearly above line C-B, which is easily calculated... however, once curves are
        //  introduced, it's not so obvious without using some heuristic which will fail at times.
        //
        let mutable a: Vec2 = seg1.start()
        let mutable b: Vec2 = seg2.start2()
        let c: Vec2 = seg2.start()

        if seg2.pointOn(a) then
            // A intersects seg2 somehow (possibly sharing a start point, or maybe just splitting it)
            //
            //   AC - - - - D
            //      \
            //        \
            //          B
            //
            // so grab seg1's second point (D) instead
            a <- seg1.start2()

            if seg2.pointOn(a) then
                match seg1, seg2 with
                | :? SegmentLine, :? SegmentLine ->
                    // oh... D is on the line too... so these are the same
                    0
                | :? SegmentLine as seg1, :? SegmentCurve ->
                    a <- seg1.point(0.5)
                    0
                | :? SegmentCurve as seg1, _ ->
                    a <- seg1.``end``()
                    0
                | _ ->
                    0
                |> ignore

            match seg2 with
            | :? SegmentCurve as seg2 when this.geo.snap0(a.[0] - c.[0]) = 0.0 && this.geo.snap0(b.[0] - c.[0]) = 0.0 ->
                // seg2 is a curve, but the tangent line (C-B) at the start point is vertical, and
                // collinear with A... so... just sort based on the Y values I guess?
                Math.Sign(c.[1] - a.[1])
            | _ ->
                let ax: float = a.[0]
                let ay: float = a.[1]
                let bx: float = b.[0]
                let by: float = b.[1]
                let cx: float = c.[0]
                let cy: float = c.[1]
                Math.Sign((bx - ax) * (cy - ay) - (by - ay) * (cx - ax))
        else
            match seg2 with
            | :? SegmentCurve as seg2 ->
                // find seg2's position at A[0] and see if it's above or below A[1]
                match seg2.mapXtoY(a.[0], true) with
                | Some y ->
                    Math.Sign(y - a.[1])
                | None ->
                    let mutable finalB: Vec2 = b

                    match seg1 with
                    | :? SegmentCurve as seg1 ->
                        // unfortunately, in order to sort against curved segments, we need to check the
                        // intersection point... this means a lot more intersection tests, but I'm not sure how else
                        // to sort correctly
                        match SegmentFunctions.segmentsIntersect(seg1, seg2, true) with
                        | Some (:? SegmentTValuePairs as i) ->
                            let mutable found: bool = false
                            let mutable index: int = 0

                            while index < i.tValuePairs.Length && not found do
                                let pair: Vec2 = i.tValuePairs.[index]
                                let t: float = this.geo.snap01(pair.[0])

                                if t > 0.0 && t < 1.0 then
                                    finalB <- seg1.point(t)
                                    found <- true

                                index <- index + 1
                        | _ ->
                            ()
                    | _ ->
                        ()

                    // fallthrough to this calculation which determines if A is on one side or another of C-B
                    let ax: float = a.[0]
                    let ay: float = a.[1]
                    let bx: float = finalB.[0]
                    let by: float = finalB.[1]
                    let cx: float = c.[0]
                    let cy: float = c.[1]
                    Math.Sign((bx - ax) * (cy - ay) - (by - ay) * (cx - ax))
            | _ ->
                let mutable finalB: Vec2 = b

                match seg1 with
                | :? SegmentCurve as seg1 ->
                    match SegmentFunctions.segmentsIntersect(seg1, seg2, true) with
                    | Some (:? SegmentTValuePairs as i) ->
                        let mutable found: bool = false
                        let mutable index: int = 0

                        while index < i.tValuePairs.Length && not found do
                            let pair: Vec2 = i.tValuePairs.[index]
                            let t: float = this.geo.snap01(pair.[0])

                            if t > 0.0 && t < 1.0 then
                                finalB <- seg1.point(t)
                                found <- true

                            index <- index + 1
                    | _ ->
                        ()
                | _ ->
                    ()

                // fallthrough to this calculation which determines if A is on one side or another of C-B
                let ax: float = a.[0]
                let ay: float = a.[1]
                let bx: float = finalB.[0]
                let by: float = finalB.[1]
                let cx: float = c.[0]
                let cy: float = c.[1]
                Math.Sign((bx - ax) * (cy - ay) - (by - ay) * (cx - ax))

    member this.statusFindSurrounding(ev: EventBool) : ListBoolTransition<EventBool> =
        this.status.findTransition(
            ev,
            fun here ->
                if obj.ReferenceEquals(ev, here) then
                    0
                else
                    let c: int = this.compareSegments(ev.seg.data, here.seg.data)
                    if c = 0 then -1 else c
        )

    member this.checkIntersection(ev1: EventBool, ev2: EventBool) : EventBool option =
        // returns the segment equal to ev1, or null if nothing equal
        let seg1: SegmentBool = ev1.seg
        let seg2: SegmentBool = ev2.seg

        this.log |> Option.iter (fun log -> log.checkIntersection(seg1, seg2))

        match SegmentFunctions.segmentsIntersect(seg1.data, seg2.data, false) with
        | None ->
            // no intersections
            None
        | Some (:? SegmentTRangePairs as i) ->
            // segments are parallel or coincident
            let tA1: float = i.tStart.[0]
            let tB1: float = i.tStart.[1]
            let tA2: float = i.tEnd.[0]
            let tB2: float = i.tEnd.[1]

            if
                (tA1 = 1.0 && tA2 = 1.0 && tB1 = 0.0 && tB2 = 0.0)
                || (tA1 = 0.0 && tA2 = 0.0 && tB1 = 1.0 && tB2 = 1.0)
            then
                None // segments touch at endpoints... no intersection
            elif tA1 = 0.0 && tA2 = 1.0 && tB1 = 0.0 && tB2 = 1.0 then
                Some ev2 // segments are exactly equal
            else
                let a1: Vec2 = seg1.data.start()
                let a2: Vec2 = seg1.data.``end``()
                let b2: Vec2 = seg2.data.``end``()

                if tA1 = 0.0 && tB1 = 0.0 then
                    if tA2 = 1.0 then
                        //  (a1)---(a2)
                        //  (b1)----------(b2)
                        this.divideEvent(ev2, tB2, a2) |> ignore
                    else
                        //  (a1)----------(a2)
                        //  (b1)---(b2)
                        this.divideEvent(ev1, tA2, b2) |> ignore

                    Some ev2
                elif tB1 > 0.0 && tB1 < 1.0 then
                    if tA2 = 1.0 && tB2 = 1.0 then
                        //         (a1)---(a2)
                        //  (b1)----------(b2)
                        this.divideEvent(ev2, tB1, a1) |> ignore
                    else
                        // make a2 equal to b2
                        if tA2 = 1.0 then
                            //         (a1)---(a2)
                            //  (b1)-----------------(b2)
                            this.divideEvent(ev2, tB2, a2) |> ignore
                        else
                            //         (a1)----------(a2)
                            //  (b1)----------(b2)
                            this.divideEvent(ev1, tA2, b2) |> ignore

                        //         (a1)---(a2)
                        //  (b1)----------(b2)
                        this.divideEvent(ev2, tB1, a1) |> ignore

                    None
                else
                    None
        | Some (:? SegmentTValuePairs as i) ->
            if i.tValuePairs.Length <= 0 then
                None
            else
                // process a single intersection
                // skip intersections where endpoints meet
                let mutable minPair: Vec2 = i.tValuePairs.[0]
                let mutable j: int = 1
                let isEndpointPair (pair: Vec2) : bool =
                    (pair.[0] = 0.0 && pair.[1] = 0.0)
                    || (pair.[0] = 0.0 && pair.[1] = 1.0)
                    || (pair.[0] = 1.0 && pair.[1] = 0.0)
                    || (pair.[0] = 1.0 && pair.[1] = 1.0)

                while j < i.tValuePairs.Length && isEndpointPair minPair do
                    minPair <- i.tValuePairs.[j]
                    j <- j + 1

                let tA: float = minPair.[0]
                let tB: float = minPair.[1]

                // even though *in theory* seg1.data.point(tA) === seg2.data.point(tB), that isn't exactly
                // correct in practice because intersections aren't exact... so we need to calculate a single
                // intersection point that everyone can share
                let p: Vec2 =
                    if tB = 0.0 then
                        seg2.data.start()
                    elif tB = 1.0 then
                        seg2.data.``end``()
                    elif tA = 0.0 then
                        seg1.data.start()
                    elif tA = 1.0 then
                        seg1.data.``end``()
                    else
                        seg1.data.point(tA)

                // is A divided between its endpoints? (exclusive)
                if tA > 0.0 && tA < 1.0 then
                    this.divideEvent(ev1, tA, p) |> ignore

                // is B divided between its endpoints? (exclusive)
                if tB > 0.0 && tB < 1.0 then
                    this.divideEvent(ev2, tB, p) |> ignore

                None
        | Some _ ->
            failwith "PolyBool: Unknown intersection type"

    member this.calculate() : SegmentBool[] =
        let segments: ResizeArray<SegmentBool> = ResizeArray<SegmentBool>()

        while not (this.events.isEmpty()) do
            let ev: EventBool = this.events.getHead()
            let mutable shouldRemoveHead: bool = true
            this.log |> Option.iter (fun log -> log.vert(ev.p.[0]))

            if ev.isStart then
                this.log |> Option.iter (fun log -> log.segmentNew(ev.seg, ev.primary))

                let surrounding: ListBoolTransition<EventBool> = this.statusFindSurrounding(ev)
                let above: EventBool option = surrounding.before
                let below: EventBool option = surrounding.after

                this.log
                |> Option.iter (fun log -> log.tempStatus(ev.seg, this.segmentOrFalse above, this.segmentOrFalse below))

                let checkBothIntersections() : EventBool option =
                    match above with
                    | Some above ->
                        match this.checkIntersection(ev, above) with
                        | Some eve ->
                            Some eve
                        | None ->
                            match below with
                            | Some below ->
                                this.checkIntersection(ev, below)
                            | None ->
                                None
                    | None ->
                        match below with
                        | Some below ->
                            this.checkIntersection(ev, below)
                        | None ->
                            None

                match checkBothIntersections() with
                | Some eve ->
                    // ev and eve are equal
                    // we'll keep eve and throw away ev
                    // merge ev.seg's fill information into eve.seg
                    if this.selfIntersection then
                        let toggle: bool =
                            match ev.seg.myFill.below with
                            | None ->
                                ev.seg.closed
                            | Some below ->
                                ev.seg.myFill.above <> Some below

                        // merge two segments that belong to the same polygon
                        // think of this as sandwiching two segments together, where
                        // `eve.seg` is the bottom -- this will cause the above fill flag to
                        // toggle
                        if toggle then
                            eve.seg.myFill <-
                                {
                                    eve.seg.myFill with
                                        above = Some(not (Option.defaultValue false eve.seg.myFill.above))
                                }
                    else
                        // merge two segments that belong to different polygons
                        // each segment has distinct knowledge, so no special logic is
                        // needed
                        // note that this can only happen once per segment in this phase,
                        // because we are guaranteed that all self-intersections are gone
                        eve.seg.otherFill <- Some ev.seg.myFill

                    this.log |> Option.iter (fun log -> log.segmentUpdate(eve.seg))
                    this.events.remove(ev.other)
                    this.events.remove(ev)
                | None ->
                    ()

                if this.events.isEmpty() || not (obj.ReferenceEquals(this.events.getHead(), ev)) then
                    // something was inserted before us in the event queue, so loop back
                    // around and process it before continuing
                    this.log |> Option.iter (fun log -> log.rewind(ev.seg))
                    shouldRemoveHead <- false
                else
                    //
                    // calculate fill flags
                    //
                    if this.selfIntersection then
                        let toggle: bool =
                            if ev.seg.myFill.below.IsNone then
                                // if we are new then we toggle if we're part of a closed path
                                ev.seg.closed
                            else
                                // we are a segment that has previous knowledge from a division
                                // calculate toggle
                                ev.seg.myFill.above <> ev.seg.myFill.below

                        // next, calculate whether we are filled below us
                        if below.IsNone then
                            // if nothing is below us, then we're not filled
                            ev.seg.myFill <- { ev.seg.myFill with below = Some false }
                        else
                            // otherwise, we know the answer -- it's the same if whatever is
                            // below us is filled above it
                            ev.seg.myFill <-
                                {
                                    ev.seg.myFill with
                                        below = Some(Option.defaultValue false below.Value.seg.myFill.above)
                                }

                        // since now we know if we're filled below us, we can calculate
                        // whether we're filled above us by applying toggle to whatever is
                        // below us
                        let belowValue: bool = Option.defaultValue false ev.seg.myFill.below

                        ev.seg.myFill <-
                            {
                                ev.seg.myFill with
                                    above =
                                        Some(
                                            if toggle then
                                                not belowValue
                                            else
                                                belowValue
                                        )
                            }
                    else
                        // now we fill in any missing transition information, since we are
                        // all-knowing at this point
                        if ev.seg.otherFill.IsNone then
                            // if we don't have other information, then we need to figure out if
                            // we're inside the other polygon
                            let inside: bool =
                                match below with
                                | None ->
                                    // if nothing is below us, then we're not filled
                                    false
                                | Some below ->
                                    // otherwise, something is below us
                                    // so copy the below segment's other polygon's above
                                    if ev.primary = below.primary then
                                        match below.seg.otherFill with
                                        | Some otherFill ->
                                            Option.defaultValue false otherFill.above
                                        | None ->
                                            failwith "PolyBool: Unexpected state of otherFill (null)"
                                    else
                                        Option.defaultValue false below.seg.myFill.above

                            ev.seg.otherFill <- Some { above = Some inside; below = Some inside }

                    this.log
                    |> Option.iter (fun log -> log.status(ev.seg, this.segmentOrFalse above, this.segmentOrFalse below))

                    // insert the status and remember it for later removal
                    ev.other.status <- surrounding.insert(ev)
            else
                // end
                let st: EventBool = ev.status

                if isNull st then
                    failwith "PolyBool: Zero-length segment detected; your epsilon is probably too small or too large"

                // removing the status will create two new adjacent edges, so we'll need
                // to check for those
                let i: int = this.status.getIndex(st)

                if i > 0 && i < this.status.nodes.Count - 1 then
                    let before: EventBool = this.status.nodes.[i - 1]
                    let after: EventBool = this.status.nodes.[i + 1]
                    this.checkIntersection(before, after) |> ignore

                this.log |> Option.iter (fun log -> log.statusRemove(st.seg))

                // remove the status
                this.status.remove(st)

                // if we've reached this point, we've calculated everything there is to
                // know, so save the segment for reporting
                if not ev.primary then
                    // make sure `seg.myFill` actually points to the primary polygon
                    // though
                    match ev.seg.otherFill with
                    | Some otherFill ->
                        let s: SegmentBoolFill = ev.seg.myFill
                        ev.seg.myFill <- otherFill
                        ev.seg.otherFill <- Some s
                    | None ->
                        failwith "PolyBool: Unexpected state of otherFill (null)"

                segments.Add(ev.seg)

            // remove the event and continue
            if shouldRemoveHead && not (this.events.isEmpty()) then
                this.events.removeHead()

        this.log |> Option.iter (fun log -> log.``done``())
        segments.ToArray()
