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

module IntersecterFunctions =
    let copySegmentBool(seg: SegmentBool, log: BuildLog option) : SegmentBool =
        SegmentBool(seg.data, Some seg.myFill, seg.closed, log)

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

type Intersecter(selfIntersection: bool,  ?log: BuildLog) =
    let eventsValue: ListBool<EventBool> = ListBool<EventBool>()
    let statusValue: ListBool<EventBool> = ListBool<EventBool>()
    let mutable currentPath: ResizeArray<SegmentBool> = ResizeArray<SegmentBool>()
    let logValue: BuildLog option = log

    member this.selfIntersection: bool = selfIntersection

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
        let comp: int = Geometry.compareVec2(a1, b1)

        if comp <> 0 then
            comp
        elif Geometry.isEqualVec2(a2, b2) then
            0
        elif aStart <> bStart then
            if aStart then 1 else -1
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

        left.setEnd(p)
        right.setStart(p)

        let ns: SegmentBool = SegmentBool(right, Some ev.seg.myFill, ev.seg.closed, this.log)

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
        let f: int = Geometry.compareVec2(fromPoint, toPoint)

        if f <> 0 then
            let startPoint: Vec2 = if f < 0 then fromPoint else toPoint
            let endPoint: Vec2 = if f < 0 then toPoint else fromPoint
            let seg: SegmentBool =
                SegmentBool(
                    Segment(startPoint, endPoint),
                    None,
                    false,
                    this.log
                )

            currentPath.Add(seg)
            this.addSegment(seg, primary) |> ignore

    member this.compareSegments(seg1: Segment, seg2: Segment) : int =
        let b: Vec2 = seg2.``end``()
        let c: Vec2 = seg2.start()

        let orientation(a: Vec2) : int =
            let ax: float = a.[0]
            let ay: float = a.[1]
            let bx: float = b.[0]
            let by: float = b.[1]
            let cx: float = c.[0]
            let cy: float = c.[1]
            Math.Sign((bx - ax) * (cy - ay) - (by - ay) * (cx - ax))

        let mutable a: Vec2 = seg1.start()

        if seg2.pointOn(a) then
            a <- seg1.``end``()

            if seg2.pointOn(a) then
                0
            else
                orientation a
        else
            orientation a

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
        let seg1: SegmentBool = ev1.seg
        let seg2: SegmentBool = ev2.seg

        this.log |> Option.iter (fun log -> log.checkIntersection(seg1, seg2))

        match SegmentFunctions.segmentsIntersect(seg1.data, seg2.data, false) with
        | None ->
            None
        | Some intersection ->
            match intersection.kind with
            | TRangePairsKind ->
                let tA1: float = intersection.tStart.[0]
                let tB1: float = intersection.tStart.[1]
                let tA2: float = intersection.tEnd.[0]
                let tB2: float = intersection.tEnd.[1]

                if
                    (tA1 = 1.0 && tA2 = 1.0 && tB1 = 0.0 && tB2 = 0.0)
                    || (tA1 = 0.0 && tA2 = 0.0 && tB1 = 1.0 && tB2 = 1.0)
                then
                    None
                elif tA1 = 0.0 && tA2 = 1.0 && tB1 = 0.0 && tB2 = 1.0 then
                    Some ev2
                else
                    let a1: Vec2 = seg1.data.start()
                    let a2: Vec2 = seg1.data.``end``()
                    let b2: Vec2 = seg2.data.``end``()

                    if tA1 = 0.0 && tB1 = 0.0 then
                        if tA2 = 1.0 then
                            this.divideEvent(ev2, tB2, a2) |> ignore
                        else
                            this.divideEvent(ev1, tA2, b2) |> ignore

                        Some ev2
                    elif tB1 > 0.0 && tB1 < 1.0 then
                        if tA2 = 1.0 && tB2 = 1.0 then
                            this.divideEvent(ev2, tB1, a1) |> ignore
                        else
                            if tA2 = 1.0 then
                                this.divideEvent(ev2, tB2, a2) |> ignore
                            else
                                this.divideEvent(ev1, tA2, b2) |> ignore

                            this.divideEvent(ev2, tB1, a1) |> ignore

                        None
                    else
                        None
            | TValuePairsKind ->
                if intersection.tValuePairs.Length <= 0 then
                    None
                else
                    let mutable minPair: Vec2 = intersection.tValuePairs.[0]
                    let mutable j: int = 1

                    let isEndpointPair(pair: Vec2) : bool =
                        (pair.[0] = 0.0 && pair.[1] = 0.0)
                        || (pair.[0] = 0.0 && pair.[1] = 1.0)
                        || (pair.[0] = 1.0 && pair.[1] = 0.0)
                        || (pair.[0] = 1.0 && pair.[1] = 1.0)

                    while j < intersection.tValuePairs.Length && isEndpointPair minPair do
                        minPair <- intersection.tValuePairs.[j]
                        j <- j + 1

                    let tA: float = minPair.[0]
                    let tB: float = minPair.[1]

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

                    if tA > 0.0 && tA < 1.0 then
                        this.divideEvent(ev1, tA, p) |> ignore

                    if tB > 0.0 && tB < 1.0 then
                        this.divideEvent(ev2, tB, p) |> ignore

                    None

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
                    if this.selfIntersection then
                        let toggle: bool =
                            match ev.seg.myFill.below with
                            | None ->
                                ev.seg.closed
                            | Some below ->
                                ev.seg.myFill.above <> Some below

                        if toggle then
                            eve.seg.myFill <-
                                {
                                    eve.seg.myFill with
                                        above = Some(not (Option.defaultValue false eve.seg.myFill.above))
                                }
                    else
                        eve.seg.otherFill <- Some ev.seg.myFill

                    this.log |> Option.iter (fun log -> log.segmentUpdate(eve.seg))
                    this.events.remove(ev.other)
                    this.events.remove(ev)
                | None ->
                    ()

                if this.events.isEmpty() || not (obj.ReferenceEquals(this.events.getHead(), ev)) then
                    this.log |> Option.iter (fun log -> log.rewind(ev.seg))
                    shouldRemoveHead <- false
                else
                    if this.selfIntersection then
                        let toggle: bool =
                            if ev.seg.myFill.below.IsNone then
                                ev.seg.closed
                            else
                                ev.seg.myFill.above <> ev.seg.myFill.below

                        if below.IsNone then
                            ev.seg.myFill <- { ev.seg.myFill with below = Some false }
                        else
                            ev.seg.myFill <-
                                {
                                    ev.seg.myFill with
                                        below = Some(Option.defaultValue false below.Value.seg.myFill.above)
                                }

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
                        if ev.seg.otherFill.IsNone then
                            let inside: bool =
                                match below with
                                | None ->
                                    false
                                | Some below ->
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

                    ev.other.status <- surrounding.insert(ev)
            else
                let st: EventBool = ev.status

                if isNull st then
                    failwith "PolyBool: Zero-length segment detected; your epsilon is probably too small or too large"

                let i: int = this.status.getIndex(st)

                if i > 0 && i < this.status.nodes.Count - 1 then
                    let before: EventBool = this.status.nodes.[i - 1]
                    let after: EventBool = this.status.nodes.[i + 1]
                    this.checkIntersection(before, after) |> ignore

                this.log |> Option.iter (fun log -> log.statusRemove(st.seg))
                this.status.remove(st)

                if not ev.primary then
                    match ev.seg.otherFill with
                    | Some otherFill ->
                        let s: SegmentBoolFill = ev.seg.myFill
                        ev.seg.myFill <- otherFill
                        ev.seg.otherFill <- Some s
                    | None ->
                        failwith "PolyBool: Unexpected state of otherFill (null)"

                segments.Add(ev.seg)

            if shouldRemoveHead && not (this.events.isEmpty()) then
                this.events.removeHead()

        this.log |> Option.iter (fun log -> log.``done``())
        segments.ToArray()