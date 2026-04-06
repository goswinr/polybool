namespace PolyBool

open System

//
// polybool - Boolean operations on polygons (union, intersection, etc)
// by Sean Connelly (@velipso), https://sean.fun
// Project Home: https://github.com/velipso/polybool
// SPDX-License-Identifier: 0BSD
//

module SegmentChainer =
    type ChainMatch =
        {
            mutable index: int
            mutable matchesHead: bool
            mutable matchesPt1: bool
        }

    type SegsFill =
        {
            mutable segs: ResizeArray<Segment>
            mutable fill: bool
        }

    let private segFill(seg: Segment, fill: bool) : SegFill =
        {
            seg = box seg
            fill = fill
        }

    let private tryGet(chain: ResizeArray<Segment>, index: int) : Segment option =
        if index >= 0 && index < chain.Count then
            Some chain.[index]
        else
            None

    let joinLines(seg1: Segment, seg2: Segment) : Segment option =
        if Geometry.isCollinear(seg1.P0X, seg1.P0Y, seg1.P1X, seg1.P1Y, seg2.P1X, seg2.P1Y) then
            Some(Segment(seg1.P0X, seg1.P0Y, seg2.P1X, seg2.P1Y))
        else
            None

    let joinSegments(seg1: Segment option, seg2: Segment option) : Segment option =
        match seg1, seg2 with
        | Some seg1, Some seg2 when not (obj.ReferenceEquals(seg1, seg2)) ->
            joinLines(seg1, seg2)
        | _ ->
            None

    let segmentChainer(segments: SegmentBool[],  log: BuildLog option) : Segment[][] =
        let closedChains: ResizeArray<SegsFill> = ResizeArray<SegsFill>()
        let openChains: ResizeArray<SegsFill> = ResizeArray<SegsFill>()
        let regions: ResizeArray<Segment[]> = ResizeArray<Segment[]>()

        for segb: SegmentBool in segments do
            let mutable seg: Segment = segb.Data
            let closed: bool = segb.Closed
            let chains: ResizeArray<SegsFill> = if closed then closedChains else openChains
            let pt1x: float = seg.P0X
            let pt1y: float = seg.P0Y
            let pt2x: float = seg.P1X
            let pt2y: float = seg.P1Y

            let reverseChain(index: int) : ResizeArray<Segment> =
                log |> Option.iter (fun log -> log.ChainReverse(index, closed))

                let newChain: ResizeArray<Segment> = ResizeArray<Segment>()

                for seg: Segment in chains.[index].segs do
                    newChain.Insert(0, seg.Reverse())

                chains.[index] <- {
                    segs = newChain
                    fill = not chains.[index].fill
                }

                newChain

            if Geometry.isEqualVec2(pt1x, pt1y, pt2x, pt2y) then
                Console.WriteLine(
                    "PolyBool: Warning: Zero-length segment detected; your epsilon is probably too small or too large"
                )
            else
                log |> Option.iter (fun log -> log.ChainStart(segFill(seg, Option.defaultValue false segb.MyFill.above), closed))

                let firstMatch: ChainMatch = { index = 0; matchesHead = false; matchesPt1 = false }
                let secondMatch: ChainMatch = { index = 0; matchesHead = false; matchesPt1 = false }
                let mutable nextMatch: ChainMatch option = Some firstMatch

                let setMatch(index: int, matchesHead: bool, matchesPt1: bool) : bool =
                    match nextMatch with
                    | Some next ->
                        next.index <- index
                        next.matchesHead <- matchesHead
                        next.matchesPt1 <- matchesPt1

                        if obj.ReferenceEquals(next, firstMatch) then
                            nextMatch <- Some secondMatch
                            false
                        else
                            nextMatch <- None
                            true
                    | None ->
                        true

                for i = 0 to chains.Count - 1 do
                    let chain: ResizeArray<Segment> = chains.[i].segs
                    let headX: float = chain.[0].P0X
                    let headY: float = chain.[0].P0Y
                    let tailX: float = chain.[chain.Count - 1].P1X
                    let tailY: float = chain.[chain.Count - 1].P1Y

                    if Geometry.isEqualVec2(headX, headY, pt1x, pt1y) then
                        if setMatch(i, true, true) then
                            ()
                    elif Geometry.isEqualVec2(headX, headY, pt2x, pt2y) then
                        if setMatch(i, true, false) then
                            ()
                    elif Geometry.isEqualVec2(tailX, tailY, pt1x, pt1y) then
                        if setMatch(i, false, true) then
                            ()
                    elif Geometry.isEqualVec2(tailX, tailY, pt2x, pt2y) then
                        if setMatch(i, false, false) then
                            ()

                if nextMatch |> Option.exists (fun m -> obj.ReferenceEquals(m, firstMatch)) then
                    let fill: bool = Option.defaultValue false segb.MyFill.above
                    chains.Add({ segs = ResizeArray<Segment>([| seg |]); fill = fill })
                    log |> Option.iter (fun log -> log.ChainNew(segFill(seg, fill), closed))
                elif nextMatch |> Option.exists (fun m -> obj.ReferenceEquals(m, secondMatch)) then
                    let index: int = firstMatch.index
                    log |> Option.iter (fun log -> log.ChainMatch(index, closed))

                    let chain: ResizeArray<Segment> = chains.[index].segs
                    let fill: bool = chains.[index].fill

                    if firstMatch.matchesHead then
                        if firstMatch.matchesPt1 then
                            seg <- seg.Reverse()
                            log |> Option.iter (fun log -> log.ChainAddHead(index, segFill(seg, fill), closed))
                            chain.Insert(0, seg)
                        else
                            log |> Option.iter (fun log -> log.ChainAddHead(index, segFill(seg, fill), closed))
                            chain.Insert(0, seg)
                    else
                        if firstMatch.matchesPt1 then
                            log |> Option.iter (fun log -> log.ChainAddTail(index, segFill(seg, fill), closed))
                            chain.Add(seg)
                        else
                            seg <- seg.Reverse()
                            log |> Option.iter (fun log -> log.ChainAddTail(index, segFill(seg, fill), closed))
                            chain.Add(seg)

                    if firstMatch.matchesHead then
                        let next: Segment option = tryGet(chain, 1)

                        match joinSegments(Some seg, next) with
                        | Some newSeg ->
                            chain.RemoveAt(0)
                            chain.[0] <- newSeg
                            log |> Option.iter (fun log -> log.ChainSimplifyHead(index, segFill(newSeg, fill), closed))
                        | None ->
                            ()
                    else
                        let next: Segment option = tryGet(chain, chain.Count - 2)

                        match joinSegments(next, Some seg) with
                        | Some newSeg ->
                            chain.RemoveAt(chain.Count - 1)
                            chain.[chain.Count - 1] <- newSeg
                            log |> Option.iter (fun log -> log.ChainSimplifyTail(index, segFill(newSeg, fill), closed))
                        | None ->
                            ()

                    if closed then
                        let mutable finalChain: ResizeArray<Segment> = chain
                        let mutable segS: Segment = finalChain.[0]
                        let mutable segE: Segment = finalChain.[finalChain.Count - 1]

                        if finalChain.Count > 0 && Geometry.isEqualVec2(segS.P0X, segS.P0Y, segE.P1X, segE.P1Y) then
                            let mutable winding: float = 0.0
                            let mutable lastX: float = finalChain.[0].P0X
                            let mutable lastY: float = finalChain.[0].P0Y

                            for seg: Segment in finalChain do
                                let hereX: float = seg.P1X
                                let hereY: float = seg.P1Y
                                winding <- winding + hereY * lastX - hereX * lastY
                                lastX <- hereX
                                lastY <- hereY

                            let isClockwise: bool = winding < 0.0

                            if isClockwise = fill then
                                finalChain <- reverseChain(index)
                                segS <- finalChain.[0]
                                segE <- finalChain.[finalChain.Count - 1]

                            match joinSegments(Some segE, Some segS) with
                            | Some newStart ->
                                finalChain.RemoveAt(finalChain.Count - 1)
                                finalChain.[0] <- newStart
                                log |> Option.iter (fun log -> log.ChainSimplifyClose(index, segFill(newStart, fill), closed))
                            | None ->
                                ()

                            log |> Option.iter (fun log -> log.ChainClose(index, closed))
                            chains.RemoveAt(index)
                            regions.Add(finalChain.ToArray())
                else
                    let appendChain(index1: int, index2: int) : unit =
                        let chain1: ResizeArray<Segment> = chains.[index1].segs
                        let fill: bool = chains.[index1].fill
                        let chain2: ResizeArray<Segment> = chains.[index2].segs

                        log |> Option.iter (fun log -> log.ChainAddTail(index1, segFill(seg, fill), closed))
                        chain1.Add(seg)

                        let next: Segment option = tryGet(chain1, chain1.Count - 2)

                        match joinSegments(next, Some seg) with
                        | Some newEnd ->
                            chain1.RemoveAt(chain1.Count - 1)
                            chain1.[chain1.Count - 1] <- newEnd
                            log |> Option.iter (fun log -> log.ChainSimplifyTail(index1, segFill(newEnd, fill), closed))
                        | None ->
                            ()

                        let tail: Segment option = tryGet(chain1, chain1.Count - 1)
                        let head: Segment option = tryGet(chain2, 0)

                        match joinSegments(tail, head) with
                        | Some newJoin ->
                            chain2.RemoveAt(0)
                            chain1.[chain1.Count - 1] <- newJoin
                            log
                            |> Option.iter (fun log -> log.ChainSimplifyJoin(index1, index2, segFill(newJoin, fill), closed))
                        | None ->
                            ()

                        log |> Option.iter (fun log -> log.ChainJoin(index1, index2, closed))

                        for part: Segment in chain2 do
                            chain1.Add(part)

                        chains.[index1].segs <- chain1
                        chains.RemoveAt(index2)

                    let fIndex: int = firstMatch.index
                    let sIndex: int = secondMatch.index

                    log |> Option.iter (fun log -> log.ChainConnect(fIndex, sIndex, closed))

                    let reverseF: bool = chains.[fIndex].segs.Count < chains.[sIndex].segs.Count

                    if firstMatch.matchesHead then
                        if secondMatch.matchesHead then
                            if reverseF then
                                if not firstMatch.matchesPt1 then
                                    seg <- seg.Reverse()

                                reverseChain(fIndex) |> ignore
                                appendChain(fIndex, sIndex)
                            else
                                if firstMatch.matchesPt1 then
                                    seg <- seg.Reverse()

                                reverseChain(sIndex) |> ignore
                                appendChain(sIndex, fIndex)
                        else
                            if firstMatch.matchesPt1 then
                                seg <- seg.Reverse()

                            appendChain(sIndex, fIndex)
                    else
                        if secondMatch.matchesHead then
                            if not firstMatch.matchesPt1 then
                                seg <- seg.Reverse()

                            appendChain(fIndex, sIndex)
                        else
                            if reverseF then
                                if firstMatch.matchesPt1 then
                                    seg <- seg.Reverse()

                                reverseChain(fIndex) |> ignore
                                appendChain(sIndex, fIndex)
                            else
                                if not firstMatch.matchesPt1 then
                                    seg <- seg.Reverse()

                                reverseChain(sIndex) |> ignore
                                appendChain(fIndex, sIndex)

        for chain in openChains do
            regions.Add(chain.segs.ToArray())

        regions.ToArray()

    let internal segmentsToReceiver(
        segments: Segment[][],

        receiver: PolyBoolReceiver
    ) : PolyBoolReceiver =
        receiver.BeginPath()

        for region: Segment[] in segments do
            if region.Length > 0 then
                for i = 0 to region.Length - 1 do
                    let seg: Segment = region.[i]

                    if i = 0 then
                        receiver.MoveTo(seg.P0X, seg.P0Y)

                    receiver.LineTo(seg.P1X, seg.P1Y)

                let first: Segment = region.[0]
                let last: Segment = region.[region.Length - 1]

                if Geometry.isEqualVec2(first.P0X, first.P0Y, last.P1X, last.P1Y) then
                    receiver.ClosePath()

        receiver
