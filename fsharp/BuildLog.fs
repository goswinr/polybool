namespace PolyBool

open System

//
// polybool - Boolean operations on polygons (union, intersection, etc)
// by Sean Connelly (@velipso), https://sean.fun
// Project Home: https://github.com/velipso/polybool
// SPDX-License-Identifier: 0BSD
//

type SegFill =
    {
        seg: obj
        fill: bool
    }

type BuildLogEntry =
    {
        typ: string
        data: obj
    }

type BuildLog() =
    let listValue: ResizeArray<BuildLogEntry> = ResizeArray<BuildLogEntry>()
    let mutable nextSegmentIdValue: int = 0
    let mutable curVertValue: float = Double.NaN

    member this.list: ResizeArray<BuildLogEntry> = listValue

    member this.nextSegmentId
        with get () : int = nextSegmentIdValue
        and set (value: int) : unit = nextSegmentIdValue <- value

    member this.curVert
        with get () : float = curVertValue
        and set (value: float) : unit = curVertValue <- value

    member this.push(typ: string, data: obj) : unit =
        listValue.Add({
            typ = typ
            data = data
        })

    member this.info(msg: string, ?data: obj) : unit =
        this.push("info", box {| msg = msg; data = defaultArg data null |})

    member this.segmentId() : int =
        let id: int = nextSegmentIdValue
        nextSegmentIdValue <- nextSegmentIdValue + 1
        id

    member this.checkIntersection(seg1: obj, seg2: obj) : unit =
        this.push("check", box {| seg1 = seg1; seg2 = seg2 |})

    member this.segmentDivide(seg: obj, p: Vec2) : unit =
        this.push("div_seg", box {| seg = seg; p = p |})

    member this.segmentChop(seg: obj) : unit =
        this.push("chop", box {| seg = seg |})

    member this.statusRemove(seg: obj) : unit =
        this.push("pop_seg", box {| seg = seg |})

    member this.segmentUpdate(seg: obj) : unit =
        this.push("seg_update", box {| seg = seg |})

    member this.segmentNew(seg: obj, primary: bool) : unit =
        this.push("new_seg", box {| seg = seg; primary = primary |})

    member this.tempStatus(seg: obj, above: obj, below: obj) : unit =
        this.push("temp_status", box {| seg = seg; above = above; below = below |})

    member this.rewind(seg: obj) : unit =
        this.push("rewind", box {| seg = seg |})

    member this.status(seg: obj, above: obj, below: obj) : unit =
        this.push("status", box {| seg = seg; above = above; below = below |})

    member this.vert(x: float) : unit =
        if x <> curVertValue then
            this.push("vert", box {| x = x |})
            curVertValue <- x

    member this.selected(segs: obj) : unit =
        this.push("selected", box {| segs = segs |})

    member this.chainStart(sf: SegFill, closed: bool) : unit =
        this.push("chain_start", box {| sf = sf; closed = closed |})

    member this.chainNew(sf: SegFill, closed: bool) : unit =
        this.push("chain_new", box {| sf = sf; closed = closed |})

    member this.chainMatch(index: int, closed: bool) : unit =
        this.push("chain_match", box {| index = index; closed = closed |})

    member this.chainClose(index: int, closed: bool) : unit =
        this.push("chain_close", box {| index = index; closed = closed |})

    member this.chainAddHead(index: int, sf: SegFill, closed: bool) : unit =
        this.push("chain_add_head", box {| index = index; sf = sf; closed = closed |})

    member this.chainAddTail(index: int, sf: SegFill, closed: bool) : unit =
        this.push("chain_add_tail", box {| index = index; sf = sf; closed = closed |})

    member this.chainSimplifyHead(index: int, sf: SegFill, closed: bool) : unit =
        this.push("chain_simp_head", box {| index = index; sf = sf; closed = closed |})

    member this.chainSimplifyTail(index: int, sf: SegFill, closed: bool) : unit =
        this.push("chain_simp_tail", box {| index = index; sf = sf; closed = closed |})

    member this.chainSimplifyClose(index: int, sf: SegFill, closed: bool) : unit =
        this.push("chain_simp_close", box {| index = index; sf = sf; closed = closed |})

    member this.chainSimplifyJoin(index1: int, index2: int, sf: SegFill, closed: bool) : unit =
        this.push(
            "chain_simp_join",
            box {| index1 = index1; index2 = index2; sf = sf; closed = closed |}
        )

    member this.chainConnect(index1: int, index2: int, closed: bool) : unit =
        this.push("chain_con", box {| index1 = index1; index2 = index2; closed = closed |})

    member this.chainReverse(index: int, closed: bool) : unit =
        this.push("chain_rev", box {| index = index; closed = closed |})

    member this.chainJoin(index1: int, index2: int, closed: bool) : unit =
        this.push("chain_join", box {| index1 = index1; index2 = index2; closed = closed |})

    member this.``done``() : unit =
        this.push("done", null)
