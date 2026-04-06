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

    member this.List: ResizeArray<BuildLogEntry> = listValue

    member this.NextSegmentId
        with get () : int = nextSegmentIdValue
        and set (value: int) : unit = nextSegmentIdValue <- value

    member this.CurVert
        with get () : float = curVertValue
        and set (value: float) : unit = curVertValue <- value

    member this.Push(typ: string, data: obj) : unit =
        listValue.Add({
            typ = typ
            data = data
        })

    member this.Info(msg: string, ?data: obj) : unit =
        this.Push("info", box {| msg = msg; data = defaultArg data null |})

    member this.SegmentId() : int =
        let id: int = nextSegmentIdValue
        nextSegmentIdValue <- nextSegmentIdValue + 1
        id

    member this.CheckIntersection(seg1: obj, seg2: obj) : unit =
        this.Push("check", box {| seg1 = seg1; seg2 = seg2 |})

    member this.SegmentDivide(seg: obj, p: Vec2) : unit =
        this.Push("div_seg", box {| seg = seg; p = p |})

    member this.SegmentChop(seg: obj) : unit =
        this.Push("chop", box {| seg = seg |})

    member this.StatusRemove(seg: obj) : unit =
        this.Push("pop_seg", box {| seg = seg |})

    member this.SegmentUpdate(seg: obj) : unit =
        this.Push("seg_update", box {| seg = seg |})

    member this.SegmentNew(seg: obj, primary: bool) : unit =
        this.Push("new_seg", box {| seg = seg; primary = primary |})

    member this.TempStatus(seg: obj, above: obj, below: obj) : unit =
        this.Push("temp_status", box {| seg = seg; above = above; below = below |})

    member this.Rewind(seg: obj) : unit =
        this.Push("rewind", box {| seg = seg |})

    member this.Status(seg: obj, above: obj, below: obj) : unit =
        this.Push("status", box {| seg = seg; above = above; below = below |})

    member this.Vert(x: float) : unit =
        if x <> curVertValue then
            this.Push("vert", box {| x = x |})
            curVertValue <- x

    member this.Selected(segs: obj) : unit =
        this.Push("selected", box {| segs = segs |})

    member this.ChainStart(sf: SegFill, closed: bool) : unit =
        this.Push("chain_start", box {| sf = sf; closed = closed |})

    member this.ChainNew(sf: SegFill, closed: bool) : unit =
        this.Push("chain_new", box {| sf = sf; closed = closed |})

    member this.ChainMatch(index: int, closed: bool) : unit =
        this.Push("chain_match", box {| index = index; closed = closed |})

    member this.ChainClose(index: int, closed: bool) : unit =
        this.Push("chain_close", box {| index = index; closed = closed |})

    member this.ChainAddHead(index: int, sf: SegFill, closed: bool) : unit =
        this.Push("chain_add_head", box {| index = index; sf = sf; closed = closed |})

    member this.ChainAddTail(index: int, sf: SegFill, closed: bool) : unit =
        this.Push("chain_add_tail", box {| index = index; sf = sf; closed = closed |})

    member this.ChainSimplifyHead(index: int, sf: SegFill, closed: bool) : unit =
        this.Push("chain_simp_head", box {| index = index; sf = sf; closed = closed |})

    member this.ChainSimplifyTail(index: int, sf: SegFill, closed: bool) : unit =
        this.Push("chain_simp_tail", box {| index = index; sf = sf; closed = closed |})

    member this.ChainSimplifyClose(index: int, sf: SegFill, closed: bool) : unit =
        this.Push("chain_simp_close", box {| index = index; sf = sf; closed = closed |})

    member this.ChainSimplifyJoin(index1: int, index2: int, sf: SegFill, closed: bool) : unit =
        this.Push(
            "chain_simp_join",
            box {| index1 = index1; index2 = index2; sf = sf; closed = closed |}
        )

    member this.ChainConnect(index1: int, index2: int, closed: bool) : unit =
        this.Push("chain_con", box {| index1 = index1; index2 = index2; closed = closed |})

    member this.ChainReverse(index: int, closed: bool) : unit =
        this.Push("chain_rev", box {| index = index; closed = closed |})

    member this.ChainJoin(index1: int, index2: int, closed: bool) : unit =
        this.Push("chain_join", box {| index1 = index1; index2 = index2; closed = closed |})

    member this.``done``() : unit =
        this.Push("done", null)
