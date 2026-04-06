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

    type ISegsFill =
        {
            mutable segs: ResizeArray<Segment>
            mutable fill: bool
        }

    let private segFill(seg: Segment, fill: bool) : ISegFill =
        {
            seg = box seg
            fill = fill
        }

    let private tryGet(chain: ResizeArray<Segment>, index: int) : Segment option =
        if index >= 0 && index < chain.Count then
            Some chain.[index]
        else
            None

    let joinLines(seg1: SegmentLine, seg2: SegmentLine, geo: Geometry) : SegmentLine option =
        if geo.isCollinear(seg1.p0, seg1.p1, seg2.p1) then
            Some(SegmentLine(seg1.p0, seg2.p1, geo))
        else
            None

    let joinSegments(seg1: Segment option, seg2: Segment option, geo: Geometry) : Segment option =
        match seg1, seg2 with
        | Some (:? SegmentLine as seg1), Some (:? SegmentLine as seg2) when not (obj.ReferenceEquals(seg1, seg2)) ->
            joinLines(seg1, seg2, geo) |> Option.map (fun seg -> seg :> Segment)
        | Some _, Some _ ->
            failwith "PolyBool: Unknown segment instance"
        | _ ->
            None

    let segmentChainer(segments: SegmentBool[], geo: Geometry, log: BuildLog option) : Segment[][] =
        let closedChains: ResizeArray<ISegsFill> = ResizeArray<ISegsFill>()
        let openChains: ResizeArray<ISegsFill> = ResizeArray<ISegsFill>()
        let regions: ResizeArray<Segment[]> = ResizeArray<Segment[]>()

        for segb: SegmentBool in segments do
            let mutable seg: Segment = segb.data
            let closed: bool = segb.closed
            let chains: ResizeArray<ISegsFill> = if closed then closedChains else openChains
            let pt1: Vec2 = seg.start()
            let pt2: Vec2 = seg.``end``()

            let reverseChain(index: int) : ResizeArray<Segment> =
                log |> Option.iter (fun log -> log.chainReverse(index, closed))

                let newChain: ResizeArray<Segment> = ResizeArray<Segment>()

                for seg: Segment in chains.[index].segs do
                    newChain.Insert(0, seg.reverse())

                chains.[index] <- {
                    segs = newChain
                    fill = not chains.[index].fill
                }

                newChain

            if geo.isEqualVec2(pt1, pt2) then
                Console.WriteLine(
                    "PolyBool: Warning: Zero-length segment detected; your epsilon is probably too small or too large"
                )
            else
                log |> Option.iter (fun log -> log.chainStart(segFill(seg, Option.defaultValue false segb.myFill.above), closed))

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
                    let head: Vec2 = chain.[0].start()
                    let tail: Vec2 = chain.[chain.Count - 1].``end``()

                    if geo.isEqualVec2(head, pt1) then
                        if setMatch(i, true, true) then
                            ()
                    elif geo.isEqualVec2(head, pt2) then
                        if setMatch(i, true, false) then
                            ()
                    elif geo.isEqualVec2(tail, pt1) then
                        if setMatch(i, false, true) then
                            ()
                    elif geo.isEqualVec2(tail, pt2) then
                        if setMatch(i, false, false) then
                            ()

                if nextMatch |> Option.exists (fun m -> obj.ReferenceEquals(m, firstMatch)) then
                    let fill: bool = Option.defaultValue false segb.myFill.above
                    chains.Add({ segs = ResizeArray<Segment>([| seg |]); fill = fill })
                    log |> Option.iter (fun log -> log.chainNew(segFill(seg, fill), closed))
                elif nextMatch |> Option.exists (fun m -> obj.ReferenceEquals(m, secondMatch)) then
                    let index: int = firstMatch.index
                    log |> Option.iter (fun log -> log.chainMatch(index, closed))

                    let chain: ResizeArray<Segment> = chains.[index].segs
                    let fill: bool = chains.[index].fill

                    if firstMatch.matchesHead then
                        if firstMatch.matchesPt1 then
                            seg <- seg.reverse()
                            log |> Option.iter (fun log -> log.chainAddHead(index, segFill(seg, fill), closed))
                            chain.Insert(0, seg)
                        else
                            log |> Option.iter (fun log -> log.chainAddHead(index, segFill(seg, fill), closed))
                            chain.Insert(0, seg)
                    else
                        if firstMatch.matchesPt1 then
                            log |> Option.iter (fun log -> log.chainAddTail(index, segFill(seg, fill), closed))
                            chain.Add(seg)
                        else
                            seg <- seg.reverse()
                            log |> Option.iter (fun log -> log.chainAddTail(index, segFill(seg, fill), closed))
                            chain.Add(seg)

                    if firstMatch.matchesHead then
                        let next: Segment option = tryGet(chain, 1)

                        match joinSegments(Some seg, next, geo) with
                        | Some newSeg ->
                            chain.RemoveAt(0)
                            chain.[0] <- newSeg
                            log |> Option.iter (fun log -> log.chainSimplifyHead(index, segFill(newSeg, fill), closed))
                        | None ->
                            ()
                    else
                        let next: Segment option = tryGet(chain, chain.Count - 2)

                        match joinSegments(next, Some seg, geo) with
                        | Some newSeg ->
                            chain.RemoveAt(chain.Count - 1)
                            chain.[chain.Count - 1] <- newSeg
                            log |> Option.iter (fun log -> log.chainSimplifyTail(index, segFill(newSeg, fill), closed))
                        | None ->
                            ()

                    if closed then
                        let mutable finalChain: ResizeArray<Segment> = chain
                        let mutable segS: Segment = finalChain.[0]
                        let mutable segE: Segment = finalChain.[finalChain.Count - 1]

                        if finalChain.Count > 0 && geo.isEqualVec2(segS.start(), segE.``end``()) then
                            let mutable winding: float = 0.0
                            let mutable last: Vec2 = finalChain.[0].start()

                            for seg: Segment in finalChain do
                                let here: Vec2 = seg.``end``()
                                winding <- winding + here.[1] * last.[0] - here.[0] * last.[1]
                                last <- here

                            let isClockwise: bool = winding < 0.0

                            if isClockwise = fill then
                                finalChain <- reverseChain(index)
                                segS <- finalChain.[0]
                                segE <- finalChain.[finalChain.Count - 1]

                            match joinSegments(Some segE, Some segS, geo) with
                            | Some newStart ->
                                finalChain.RemoveAt(finalChain.Count - 1)
                                finalChain.[0] <- newStart
                                log |> Option.iter (fun log -> log.chainSimplifyClose(index, segFill(newStart, fill), closed))
                            | None ->
                                ()

                            log |> Option.iter (fun log -> log.chainClose(index, closed))
                            chains.RemoveAt(index)
                            regions.Add(finalChain.ToArray())
                else
                    let appendChain(index1: int, index2: int) : unit =
                        let chain1: ResizeArray<Segment> = chains.[index1].segs
                        let fill: bool = chains.[index1].fill
                        let chain2: ResizeArray<Segment> = chains.[index2].segs

                        log |> Option.iter (fun log -> log.chainAddTail(index1, segFill(seg, fill), closed))
                        chain1.Add(seg)

                        let next: Segment option = tryGet(chain1, chain1.Count - 2)

                        match joinSegments(next, Some seg, geo) with
                        | Some newEnd ->
                            chain1.RemoveAt(chain1.Count - 1)
                            chain1.[chain1.Count - 1] <- newEnd
                            log |> Option.iter (fun log -> log.chainSimplifyTail(index1, segFill(newEnd, fill), closed))
                        | None ->
                            ()

                        let tail: Segment option = tryGet(chain1, chain1.Count - 1)
                        let head: Segment option = tryGet(chain2, 0)

                        match joinSegments(tail, head, geo) with
                        | Some newJoin ->
                            chain2.RemoveAt(0)
                            chain1.[chain1.Count - 1] <- newJoin
                            log
                            |> Option.iter (fun log -> log.chainSimplifyJoin(index1, index2, segFill(newJoin, fill), closed))
                        | None ->
                            ()

                        log |> Option.iter (fun log -> log.chainJoin(index1, index2, closed))

                        for part: Segment in chain2 do
                            chain1.Add(part)

                        chains.[index1].segs <- chain1
                        chains.RemoveAt(index2)

                    let fIndex: int = firstMatch.index
                    let sIndex: int = secondMatch.index

                    log |> Option.iter (fun log -> log.chainConnect(fIndex, sIndex, closed))

                    let reverseF: bool = chains.[fIndex].segs.Count < chains.[sIndex].segs.Count

                    if firstMatch.matchesHead then
                        if secondMatch.matchesHead then
                            if reverseF then
                                if not firstMatch.matchesPt1 then
                                    seg <- seg.reverse()

                                reverseChain(fIndex) |> ignore
                                appendChain(fIndex, sIndex)
                            else
                                if firstMatch.matchesPt1 then
                                    seg <- seg.reverse()

                                reverseChain(sIndex) |> ignore
                                appendChain(sIndex, fIndex)
                        else
                            if firstMatch.matchesPt1 then
                                seg <- seg.reverse()

                            appendChain(sIndex, fIndex)
                    else
                        if secondMatch.matchesHead then
                            if not firstMatch.matchesPt1 then
                                seg <- seg.reverse()

                            appendChain(fIndex, sIndex)
                        else
                            if reverseF then
                                if firstMatch.matchesPt1 then
                                    seg <- seg.reverse()

                                reverseChain(fIndex) |> ignore
                                appendChain(sIndex, fIndex)
                            else
                                if not firstMatch.matchesPt1 then
                                    seg <- seg.reverse()

                                reverseChain(sIndex) |> ignore
                                appendChain(fIndex, sIndex)

        for chain in openChains do
            regions.Add(chain.segs.ToArray())

        regions.ToArray()

    let segmentsToReceiver<'T when 'T :> IPolyBoolReceiver>(
        segments: Segment[][],
        geo: Geometry,
        receiver: 'T,
        matrix: Transform
    ) : 'T =
        let applyPoint(point: Vec2) : Vec2 =
            TransformFunctions.apply matrix point.[0] point.[1]

        receiver.beginPath()

        for region: Segment[] in segments do
            if region.Length > 0 then
                for i = 0 to region.Length - 1 do
                    let seg: Segment = region.[i]

                    if i = 0 then
                        let p0: Vec2 = applyPoint (seg.start())
                        receiver.moveTo(p0.[0], p0.[1])

                    match seg with
                    | :? SegmentLine as seg ->
                        let p1: Vec2 = applyPoint seg.p1
                        receiver.lineTo(p1.[0], p1.[1])
                    | _ ->
                        failwith "PolyBool: Unknown segment instance"

                let first: Segment = region.[0]
                let last: Segment = region.[region.Length - 1]

                if geo.isEqualVec2(first.start(), last.``end``()) then
                    receiver.closePath()

        receiver