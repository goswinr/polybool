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
type EventBool(isStart: bool, p: Vec2, seg: SegmentBool, primary: bool) =
    let mutable pValue: Vec2 = p
    let mutable otherValue: EventBool = null
    let mutable statusValue: EventBool = null

    member this.IsStart: bool = isStart

    member this.P
        with get () : Vec2 = pValue
        and set (value: Vec2) : unit = pValue <- value

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
                        ev.P,
                        ev.Other.P,
                        ev.Seg.Data,
                        here.IsStart,
                        here.P,
                        here.Other.P,
                        here.Seg.Data
                    )
        )

    member this.DivideEvent(ev: EventBool, t: float, p: Vec2) : EventBool =
        this.Log |> Option.iter (fun log -> log.SegmentDivide(ev.Seg, p))

        let split: Segment[] = ev.Seg.Data.Split([| t |])
        let left: Segment = split.[0]
        let right: Segment = split.[1]

        left.SetEnd(p)
        right.SetStart(p)

        let ns: SegmentBool = SegmentBool(right, Some ev.Seg.MyFill, ev.Seg.Closed, this.Log)

        this.Events.Remove(ev.Other)
        ev.Seg.Data <- left
        this.Log |> Option.iter (fun log -> log.SegmentChop(ev.Seg))
        ev.Other.P <- p
        this.AddEvent(ev.Other)
        this.AddSegment(ns, ev.Primary)

    member this.BeginPath() : unit =
        currentPath <- ResizeArray<SegmentBool>()

    member this.ClosePath() : unit =
        for seg: SegmentBool in currentPath do
            seg.Closed <- true

    member this.AddSegment(seg: SegmentBool, primary: bool) : EventBool =
        let evStart: EventBool = EventBool(true, seg.Data.Start(), seg, primary)
        let evEnd: EventBool = EventBool(false, seg.Data.``end``(), seg, primary)
        evStart.Other <- evEnd
        evEnd.Other <- evStart
        this.AddEvent(evStart)
        this.AddEvent(evEnd)
        evStart

    member this.AddLine(fromPoint: Vec2, toPoint: Vec2, ?primary: bool) : unit =
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
                    this.Log
                )

            currentPath.Add(seg)
            this.AddSegment(seg, primary) |> ignore

    member this.CompareSegments(seg1: Segment, seg2: Segment) : int =
        let b: Vec2 = seg2.``end``()
        let c: Vec2 = seg2.Start()

        let orientation(a: Vec2) : int =
            let ax: float = a.[0]
            let ay: float = a.[1]
            let bx: float = b.[0]
            let by: float = b.[1]
            let cx: float = c.[0]
            let cy: float = c.[1]
            Math.Sign((bx - ax) * (cy - ay) - (by - ay) * (cx - ax))

        let mutable a: Vec2 = seg1.Start()

        if seg2.PointOn(a) then
            a <- seg1.``end``()

            if seg2.PointOn(a) then
                0
            else
                orientation a
        else
            orientation a

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
                let tA1: float = intersection.TStart.[0]
                let tB1: float = intersection.TStart.[1]
                let tA2: float = intersection.TEnd.[0]
                let tB2: float = intersection.TEnd.[1]

                if
                    (tA1 = 1.0 && tA2 = 1.0 && tB1 = 0.0 && tB2 = 0.0)
                    || (tA1 = 0.0 && tA2 = 0.0 && tB1 = 1.0 && tB2 = 1.0)
                then
                    None
                elif tA1 = 0.0 && tA2 = 1.0 && tB1 = 0.0 && tB2 = 1.0 then
                    Some ev2
                else
                    let a1: Vec2 = seg1.Data.Start()
                    let a2: Vec2 = seg1.Data.``end``()
                    let b2: Vec2 = seg2.Data.``end``()

                    if tA1 = 0.0 && tB1 = 0.0 then
                        if tA2 = 1.0 then
                            this.DivideEvent(ev2, tB2, a2) |> ignore
                        else
                            this.DivideEvent(ev1, tA2, b2) |> ignore

                        Some ev2
                    elif tB1 > 0.0 && tB1 < 1.0 then
                        if tA2 = 1.0 && tB2 = 1.0 then
                            this.DivideEvent(ev2, tB1, a1) |> ignore
                        else
                            if tA2 = 1.0 then
                                this.DivideEvent(ev2, tB2, a2) |> ignore
                            else
                                this.DivideEvent(ev1, tA2, b2) |> ignore

                            this.DivideEvent(ev2, tB1, a1) |> ignore

                        None
                    else
                        None
            | TValuePairsKind ->
                if intersection.TValuePairs.Length <= 0 then
                    None
                else
                    let mutable minPair: Vec2 = intersection.TValuePairs.[0]
                    let mutable j: int = 1

                    let isEndpointPair(pair: Vec2) : bool =
                        (pair.[0] = 0.0 && pair.[1] = 0.0)
                        || (pair.[0] = 0.0 && pair.[1] = 1.0)
                        || (pair.[0] = 1.0 && pair.[1] = 0.0)
                        || (pair.[0] = 1.0 && pair.[1] = 1.0)

                    while j < intersection.TValuePairs.Length && isEndpointPair minPair do
                        minPair <- intersection.TValuePairs.[j]
                        j <- j + 1

                    let tA: float = minPair.[0]
                    let tB: float = minPair.[1]

                    let p: Vec2 =
                        if tB = 0.0 then
                            seg2.Data.Start()
                        elif tB = 1.0 then
                            seg2.Data.``end``()
                        elif tA = 0.0 then
                            seg1.Data.Start()
                        elif tA = 1.0 then
                            seg1.Data.``end``()
                        else
                            seg1.Data.Point(tA)

                    if tA > 0.0 && tA < 1.0 then
                        this.DivideEvent(ev1, tA, p) |> ignore

                    if tB > 0.0 && tB < 1.0 then
                        this.DivideEvent(ev2, tB, p) |> ignore

                    None

    member this.Calculate() : SegmentBool[] =
        let segments: ResizeArray<SegmentBool> = ResizeArray<SegmentBool>()

        while not (this.Events.IsEmpty()) do
            let ev: EventBool = this.Events.GetHead()
            let mutable shouldRemoveHead: bool = true
            this.Log |> Option.iter (fun log -> log.Vert(ev.P.[0]))

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