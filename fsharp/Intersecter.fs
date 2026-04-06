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



type SegmentBool(data: Segment, fill: SegmentBoolFill option, closed: bool, log: BuildLog option) =
    let idValue: int =
        match log with
        | Some log ->
            log.SegmentId()
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

    member this.Id: int = idValue

    member this.Data
        with get () : Segment = dataValue
        and set (value: Segment) : unit = dataValue <- value

    member this.MyFill
        with get () : SegmentBoolFill = myFillValue
        and set (value: SegmentBoolFill) : unit = myFillValue <- value

    member this.OtherFill
        with get () : SegmentBoolFill option = otherFillValue
        and set (value: SegmentBoolFill option) : unit = otherFillValue <- value

    member this.Closed
        with get () : bool = closedValue
        and set (value: bool) : unit = closedValue <- value

module IntersecterFunctions =
    let copySegmentBool(seg: SegmentBool, log: BuildLog option) : SegmentBool =
        SegmentBool(seg.Data, Some seg.MyFill, seg.Closed, log)

[<AllowNullLiteral>]
type EventBool(isStart: bool, px: float, py: float, seg: SegmentBool, primary: bool) =
    let mutable pxValue: float = px
    let mutable pyValue: float = py
    let mutable otherValue: EventBool = null
    let mutable statusValue: EventBool = null

    member this.IsStart: bool = isStart

    member this.PX
        with get () : float = pxValue
        and set (value: float) : unit = pxValue <- value

    member this.PY
        with get () : float = pyValue
        and set (value: float) : unit = pyValue <- value

    member this.Seg: SegmentBool = seg
    member this.Primary: bool = primary

    member this.Other
        with get () : EventBool = otherValue
        and set (value: EventBool) : unit = otherValue <- value

    member this.Status
        with get () : EventBool = statusValue
        and set (value: EventBool) : unit = statusValue <- value


type ListBoolTransition =
    {
        before: EventBool option
        after: EventBool option
        insert: EventBool -> EventBool
    }

type ListBool() =
    let nodesValue: ResizeArray<EventBool> = ResizeArray<EventBool>()

    member this.Nodes: ResizeArray<EventBool> = nodesValue

    member this.Remove(node: EventBool) : unit =
        let i: int = nodesValue.IndexOf(node)

        if i >= 0 then
            nodesValue.RemoveAt(i)

    member this.GetIndex(node: EventBool) : int =
        nodesValue.IndexOf(node)

    member this.IsEmpty() : bool =
        nodesValue.Count <= 0

    member this.GetHead() : EventBool =
        nodesValue.[0]

    member this.RemoveHead() : unit =
        nodesValue.RemoveAt(0)

    member this.InsertBefore(node: EventBool, check: EventBool -> int) : unit =
        this.FindTransition(node, check).insert(node) |> ignore

    member this.FindTransition(node: EventBool, check: EventBool -> int) : ListBoolTransition =
        let compare (a: EventBool) (b: EventBool) : int =
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
    let eventsValue = ListBool()
    let statusValue = ListBool()
    let mutable currentPath: ResizeArray<SegmentBool> = ResizeArray<SegmentBool>()
    let logValue: BuildLog option = log

    member this.SelfIntersection: bool = selfIntersection

    member this.Events: ListBool = eventsValue
    member this.Status: ListBool = statusValue
    member this.Log: BuildLog option = logValue

    member private this.SegmentOrFalse(ev: EventBool option) : obj =
        match ev with
        | Some ev ->
            box ev.Seg
        | None ->
            box false

    member this.CompareEvents(
        aStart: bool,
        a1x: float,
        a1y: float,
        a2x: float,
        a2y: float,
        aSeg: Segment,
        bStart: bool,
        b1x: float,
        b1y: float,
        b2x: float,
        b2y: float,
        bSeg: Segment
    ) : int =
        let comp: int = Geometry.compareVec2(a1x, a1y, b1x, b1y)

        if comp <> 0 then
            comp
        elif Geometry.isEqualVec2(a2x, a2y, b2x, b2y) then
            0
        elif aStart <> bStart then
            if aStart then 1 else -1
        else
            this.CompareSegments(bSeg, aSeg)

    member this.AddEvent(ev: EventBool) : unit =
        this.Events.InsertBefore(
            ev,
            fun here ->
                if obj.ReferenceEquals(here, ev) then
                    0
                else
                    this.CompareEvents(
                        ev.IsStart,
                        ev.PX,
                        ev.PY,
                        ev.Other.PX,
                        ev.Other.PY,
                        ev.Seg.Data,
                        here.IsStart,
                        here.PX,
                        here.PY,
                        here.Other.PX,
                        here.Other.PY,
                        here.Seg.Data
                    )
        )

    member this.DivideEvent(ev: EventBool, t: float, px: float, py: float) : EventBool =
        this.Log |> Option.iter (fun log -> log.SegmentDivide(ev.Seg, px, py))

        let split: Segment[] = ev.Seg.Data.Split([| t |])
        let left: Segment = split.[0]
        let right: Segment = split.[1]

        left.SetEnd(px, py)
        right.SetStart(px, py)

        let ns: SegmentBool = SegmentBool(right, Some ev.Seg.MyFill, ev.Seg.Closed, this.Log)

        this.Events.Remove(ev.Other)
        ev.Seg.Data <- left
        this.Log |> Option.iter (fun log -> log.SegmentChop(ev.Seg))
        ev.Other.PX <- px
        ev.Other.PY <- py
        this.AddEvent(ev.Other)
        this.AddSegment(ns, ev.Primary)

    member this.BeginPath() : unit =
        currentPath <- ResizeArray<SegmentBool>()

    member this.ClosePath() : unit =
        for seg: SegmentBool in currentPath do
            seg.Closed <- true

    member this.AddSegment(seg: SegmentBool, primary: bool) : EventBool =
        let evStart: EventBool = EventBool(true, seg.Data.P0X, seg.Data.P0Y, seg, primary)
        let evEnd: EventBool = EventBool(false, seg.Data.P1X, seg.Data.P1Y, seg, primary)
        evStart.Other <- evEnd
        evEnd.Other <- evStart
        this.AddEvent(evStart)
        this.AddEvent(evEnd)
        evStart

    member this.AddLine(fromX: float, fromY: float, toX: float, toY: float, ?primary: bool) : unit =
        let primary: bool = defaultArg primary true
        let f: int = Geometry.compareVec2(fromX, fromY, toX, toY)

        if f <> 0 then
            let startX: float = if f < 0 then fromX else toX
            let startY: float = if f < 0 then fromY else toY
            let endX: float = if f < 0 then toX else fromX
            let endY: float = if f < 0 then toY else fromY
            let seg: SegmentBool =
                SegmentBool(
                    Segment(startX, startY, endX, endY),
                    None,
                    false,
                    this.Log
                )

            currentPath.Add(seg)
            this.AddSegment(seg, primary) |> ignore

    member this.CompareSegments(seg1: Segment, seg2: Segment) : int =
        let bx: float = seg2.P1X
        let by: float = seg2.P1Y
        let cx: float = seg2.P0X
        let cy: float = seg2.P0Y

        let orientation(ax: float, ay: float) : int =
            Math.Sign((bx - ax) * (cy - ay) - (by - ay) * (cx - ax))

        let mutable ax: float = seg1.P0X
        let mutable ay: float = seg1.P0Y

        if seg2.PointOn(ax, ay) then
            ax <- seg1.P1X
            ay <- seg1.P1Y

            if seg2.PointOn(ax, ay) then
                0
            else
                orientation(ax, ay)
        else
            orientation(ax, ay)

    member this.StatusFindSurrounding(ev: EventBool) : ListBoolTransition =
        this.Status.FindTransition(
            ev,
            fun here ->
                if obj.ReferenceEquals(ev, here) then
                    0
                else
                    let c: int = this.CompareSegments(ev.Seg.Data, here.Seg.Data)
                    if c = 0 then -1 else c
        )

    member this.CheckIntersection(ev1: EventBool, ev2: EventBool) : EventBool option =
        let seg1: SegmentBool = ev1.Seg
        let seg2: SegmentBool = ev2.Seg

        this.Log |> Option.iter (fun log -> log.CheckIntersection(seg1, seg2))

        match SegmentFunctions.segmentsIntersect(seg1.Data, seg2.Data, false) with
        | None ->
            None
        | Some intersection ->
            match intersection.Kind with
            | TRangePairsKind ->
                let tA1: float = intersection.TStartA
                let tB1: float = intersection.TStartB
                let tA2: float = intersection.TEndA
                let tB2: float = intersection.TEndB

                if
                    (tA1 = 1.0 && tA2 = 1.0 && tB1 = 0.0 && tB2 = 0.0)
                    || (tA1 = 0.0 && tA2 = 0.0 && tB1 = 1.0 && tB2 = 1.0)
                then
                    None
                elif tA1 = 0.0 && tA2 = 1.0 && tB1 = 0.0 && tB2 = 1.0 then
                    Some ev2
                else
                    let a1x: float = seg1.Data.P0X
                    let a1y: float = seg1.Data.P0Y
                    let a2x: float = seg1.Data.P1X
                    let a2y: float = seg1.Data.P1Y
                    let b2x: float = seg2.Data.P1X
                    let b2y: float = seg2.Data.P1Y

                    if tA1 = 0.0 && tB1 = 0.0 then
                        if tA2 = 1.0 then
                            this.DivideEvent(ev2, tB2, a2x, a2y) |> ignore
                        else
                            this.DivideEvent(ev1, tA2, b2x, b2y) |> ignore

                        Some ev2
                    elif tB1 > 0.0 && tB1 < 1.0 then
                        if tA2 = 1.0 && tB2 = 1.0 then
                            this.DivideEvent(ev2, tB1, a1x, a1y) |> ignore
                        else
                            if tA2 = 1.0 then
                                this.DivideEvent(ev2, tB2, a2x, a2y) |> ignore
                            else
                                this.DivideEvent(ev1, tA2, b2x, b2y) |> ignore

                            this.DivideEvent(ev2, tB1, a1x, a1y) |> ignore

                        None
                    else
                        None
            | TValuePairsKind ->
                if intersection.TValuePairsCount <= 0 then
                    None
                else
                    let tvp: float[] = intersection.TValuePairs
                    let mutable minIdx: int = 0
                    let mutable j: int = 1

                    let isEndpointPair(idx: int) : bool =
                        let t1: float = tvp.[idx * 2]
                        let t2: float = tvp.[idx * 2 + 1]
                        (t1 = 0.0 && t2 = 0.0)
                        || (t1 = 0.0 && t2 = 1.0)
                        || (t1 = 1.0 && t2 = 0.0)
                        || (t1 = 1.0 && t2 = 1.0)

                    while j < intersection.TValuePairsCount && isEndpointPair minIdx do
                        minIdx <- j
                        j <- j + 1

                    let tA: float = tvp.[minIdx * 2]
                    let tB: float = tvp.[minIdx * 2 + 1]

                    let mutable px: float = 0.0
                    let mutable py: float = 0.0

                    if tB = 0.0 then
                        px <- seg2.Data.P0X
                        py <- seg2.Data.P0Y
                    elif tB = 1.0 then
                        px <- seg2.Data.P1X
                        py <- seg2.Data.P1Y
                    elif tA = 0.0 then
                        px <- seg1.Data.P0X
                        py <- seg1.Data.P0Y
                    elif tA = 1.0 then
                        px <- seg1.Data.P1X
                        py <- seg1.Data.P1Y
                    else
                        seg1.Data.PointTo(tA)
                        px <- Geometry.resultX
                        py <- Geometry.resultY

                    if tA > 0.0 && tA < 1.0 then
                        this.DivideEvent(ev1, tA, px, py) |> ignore

                    if tB > 0.0 && tB < 1.0 then
                        this.DivideEvent(ev2, tB, px, py) |> ignore

                    None

    member this.Calculate() : SegmentBool[] =
        let segments: ResizeArray<SegmentBool> = ResizeArray<SegmentBool>()

        while not (this.Events.IsEmpty()) do
            let ev: EventBool = this.Events.GetHead()
            let mutable shouldRemoveHead: bool = true
            this.Log |> Option.iter (fun log -> log.Vert(ev.PX))

            if ev.IsStart then
                this.Log |> Option.iter (fun log -> log.SegmentNew(ev.Seg, ev.Primary))

                let surrounding: ListBoolTransition = this.StatusFindSurrounding(ev)
                let above: EventBool option = surrounding.before
                let below: EventBool option = surrounding.after

                this.Log
                |> Option.iter (fun log -> log.TempStatus(ev.Seg, this.SegmentOrFalse above, this.SegmentOrFalse below))

                let checkBothIntersections() : EventBool option =
                    match above with
                    | Some above ->
                        match this.CheckIntersection(ev, above) with
                        | Some eve ->
                            Some eve
                        | None ->
                            match below with
                            | Some below ->
                                this.CheckIntersection(ev, below)
                            | None ->
                                None
                    | None ->
                        match below with
                        | Some below ->
                            this.CheckIntersection(ev, below)
                        | None ->
                            None

                match checkBothIntersections() with
                | Some eve ->
                    if this.SelfIntersection then
                        let toggle: bool =
                            match ev.Seg.MyFill.below with
                            | None ->
                                ev.Seg.Closed
                            | Some below ->
                                ev.Seg.MyFill.above <> Some below

                        if toggle then
                            eve.Seg.MyFill <-
                                {
                                    eve.Seg.MyFill with
                                        above = Some(not (Option.defaultValue false eve.Seg.MyFill.above))
                                }
                    else
                        eve.Seg.OtherFill <- Some ev.Seg.MyFill

                    this.Log |> Option.iter (fun log -> log.SegmentUpdate(eve.Seg))
                    this.Events.Remove(ev.Other)
                    this.Events.Remove(ev)
                | None ->
                    ()

                if this.Events.IsEmpty() || not (obj.ReferenceEquals(this.Events.GetHead(), ev)) then
                    this.Log |> Option.iter (fun log -> log.Rewind(ev.Seg))
                    shouldRemoveHead <- false
                else
                    if this.SelfIntersection then
                        let toggle: bool =
                            if ev.Seg.MyFill.below.IsNone then
                                ev.Seg.Closed
                            else
                                ev.Seg.MyFill.above <> ev.Seg.MyFill.below

                        if below.IsNone then
                            ev.Seg.MyFill <- { ev.Seg.MyFill with below = Some false }
                        else
                            ev.Seg.MyFill <-
                                {
                                    ev.Seg.MyFill with
                                        below = Some(Option.defaultValue false below.Value.Seg.MyFill.above)
                                }

                        let belowValue: bool = Option.defaultValue false ev.Seg.MyFill.below

                        ev.Seg.MyFill <-
                            {
                                ev.Seg.MyFill with
                                    above =
                                        Some(
                                            if toggle then
                                                not belowValue
                                            else
                                                belowValue
                                        )
                            }
                    else
                        if ev.Seg.OtherFill.IsNone then
                            let inside: bool =
                                match below with
                                | None ->
                                    false
                                | Some below ->
                                    if ev.Primary = below.Primary then
                                        match below.Seg.OtherFill with
                                        | Some otherFill ->
                                            Option.defaultValue false otherFill.above
                                        | None ->
                                            failwith "PolyBool: Unexpected state of otherFill (null)"
                                    else
                                        Option.defaultValue false below.Seg.MyFill.above

                            ev.Seg.OtherFill <- Some { above = Some inside; below = Some inside }

                    this.Log
                    |> Option.iter (fun log -> log.Status(ev.Seg, this.SegmentOrFalse above, this.SegmentOrFalse below))

                    ev.Other.Status <- surrounding.insert(ev)
            else
                let st: EventBool = ev.Status

                if isNull st then
                    failwith "PolyBool: Zero-length segment detected; your epsilon is probably too small or too large"

                let i: int = this.Status.GetIndex(st)

                if i > 0 && i < this.Status.Nodes.Count - 1 then
                    let before: EventBool = this.Status.Nodes.[i - 1]
                    let after: EventBool = this.Status.Nodes.[i + 1]
                    this.CheckIntersection(before, after) |> ignore

                this.Log |> Option.iter (fun log -> log.StatusRemove(st.Seg))
                this.Status.Remove(st)

                if not ev.Primary then
                    match ev.Seg.OtherFill with
                    | Some otherFill ->
                        let s: SegmentBoolFill = ev.Seg.MyFill
                        ev.Seg.MyFill <- otherFill
                        ev.Seg.OtherFill <- Some s
                    | None ->
                        failwith "PolyBool: Unexpected state of otherFill (null)"

                segments.Add(ev.Seg)

            if shouldRemoveHead && not (this.Events.IsEmpty()) then
                this.Events.RemoveHead()

        this.Log |> Option.iter (fun log -> log.``done``())
        segments.ToArray()
