namespace PolyBool

//
// polybool - Boolean operations on polygons (union, intersection, etc)
// by Sean Connelly (@velipso), https://sean.fun
// Project Home: https://github.com/velipso/polybool
// SPDX-License-Identifier: 0BSD
//

//
// filter a list of segments based on boolean operations
//

type SegmentSelector private () =
    static member private select(segments: SegmentBool[], selection: int[], log: BuildLog option) : SegmentBool[] =
        let result: ResizeArray<SegmentBool> = ResizeArray<SegmentBool>()

        for seg: SegmentBool in segments do
            let index: int =
                (if Option.defaultValue false seg.myFill.above then 8 else 0)
                + (if Option.defaultValue false seg.myFill.below then 4 else 0)
                + (
                    match seg.otherFill with
                    | Some otherFill when Option.defaultValue false otherFill.above -> 2
                    | _ -> 0
                )
                + (
                    match seg.otherFill with
                    | Some otherFill when Option.defaultValue false otherFill.below -> 1
                    | _ -> 0
                )

            let flags: int = selection.[index]
            let above: bool = (flags &&& 1) <> 0
            let below: bool = (flags &&& 2) <> 0

            if ((not seg.closed) && flags <> 0) || (seg.closed && above <> below) then
                let fill: SegmentBoolFill = { above = Some above; below = Some below }
                result.Add(SegmentBool(seg.data, Some fill, seg.closed, log))

        log |> Option.iter (fun log -> log.selected(box (result.ToArray())))
        result.ToArray()

    static member union(segments: SegmentBool[], log: BuildLog option) : SegmentBool[] =
        SegmentSelector.select(
            segments,
            [|
                4; 2; 1; 0
                2; 2; 0; 0
                1; 0; 1; 0
                0; 0; 0; 0
            |],
            log
        )

    static member intersect(segments: SegmentBool[], log: BuildLog option) : SegmentBool[] =
        SegmentSelector.select(
            segments,
            [|
                0; 0; 0; 4
                0; 2; 0; 2
                0; 0; 1; 1
                4; 2; 1; 0
            |],
            log
        )

    static member difference(segments: SegmentBool[], log: BuildLog option) : SegmentBool[] =
        SegmentSelector.select(
            segments,
            [|
                4; 0; 0; 0
                2; 0; 2; 0
                1; 1; 0; 0
                0; 1; 2; 0
            |],
            log
        )

    static member differenceRev(segments: SegmentBool[], log: BuildLog option) : SegmentBool[] =
        SegmentSelector.select(
            segments,
            [|
                4; 2; 1; 0
                0; 0; 1; 1
                0; 2; 0; 2
                0; 0; 0; 0
            |],
            log
        )

    static member xor(segments: SegmentBool[], log: BuildLog option) : SegmentBool[] =
        SegmentSelector.select(
            segments,
            [|
                4; 2; 1; 0
                2; 0; 0; 1
                1; 0; 0; 2
                0; 1; 2; 0
            |],
            log
        )