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
            let above: bool = (flags &&& 1) <> 0 // bit 1 if filled above
            let below: bool = (flags &&& 2) <> 0 // bit 2 if filled below

            if ((not seg.closed) && flags <> 0) || (seg.closed && above <> below) then
                // copy the segment to the results, while also calculating the fill status
                let fill: SegmentBoolFill = { above = Some above; below = Some below }

                match seg with
                | :? SegmentBoolLine as seg ->
                    result.Add(SegmentBoolLine(seg.data :?> SegmentLine, Some fill, seg.closed, log) :> SegmentBool)
                | :? SegmentBoolCurve as seg ->
                    result.Add(SegmentBoolCurve(seg.data :?> SegmentCurve, Some fill, seg.closed, log) :> SegmentBool)
                | _ ->
                    failwith "PolyBool: Unknown SegmentBool type in SegmentSelector"

        log |> Option.iter (fun log -> log.selected(box (result.ToArray())))
        result.ToArray()

    // prettier-ignore
    static member union(segments: SegmentBool[], log: BuildLog option) : SegmentBool[] =
        // primary | secondary
        // above1 below1 above2 below2    Keep?               Value
        //    0      0      0      0   =>   yes if open         4
        //    0      0      0      1   =>   yes filled below    2
        //    0      0      1      0   =>   yes filled above    1
        //    0      0      1      1   =>   no                  0
        //    0      1      0      0   =>   yes filled below    2
        //    0      1      0      1   =>   yes filled below    2
        //    0      1      1      0   =>   no                  0
        //    0      1      1      1   =>   no                  0
        //    1      0      0      0   =>   yes filled above    1
        //    1      0      0      1   =>   no                  0
        //    1      0      1      0   =>   yes filled above    1
        //    1      0      1      1   =>   no                  0
        //    1      1      0      0   =>   no                  0
        //    1      1      0      1   =>   no                  0
        //    1      1      1      0   =>   no                  0
        //    1      1      1      1   =>   no                  0
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

    // prettier-ignore
    static member intersect(segments: SegmentBool[], log: BuildLog option) : SegmentBool[] =
        // primary & secondary
        // above1 below1 above2 below2    Keep?               Value
        //    0      0      0      0   =>   no                  0
        //    0      0      0      1   =>   no                  0
        //    0      0      1      0   =>   no                  0
        //    0      0      1      1   =>   yes if open         4
        //    0      1      0      0   =>   no                  0
        //    0      1      0      1   =>   yes filled below    2
        //    0      1      1      0   =>   no                  0
        //    0      1      1      1   =>   yes filled below    2
        //    1      0      0      0   =>   no                  0
        //    1      0      0      1   =>   no                  0
        //    1      0      1      0   =>   yes filled above    1
        //    1      0      1      1   =>   yes filled above    1
        //    1      1      0      0   =>   yes if open         4
        //    1      1      0      1   =>   yes filled below    2
        //    1      1      1      0   =>   yes filled above    1
        //    1      1      1      1   =>   no                  0
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

    // prettier-ignore
    static member difference(segments: SegmentBool[], log: BuildLog option) : SegmentBool[] =
        // primary - secondary
        // above1 below1 above2 below2    Keep?               Value
        //    0      0      0      0   =>   yes if open         4
        //    0      0      0      1   =>   no                  0
        //    0      0      1      0   =>   no                  0
        //    0      0      1      1   =>   no                  0
        //    0      1      0      0   =>   yes filled below    2
        //    0      1      0      1   =>   no                  0
        //    0      1      1      0   =>   yes filled below    2
        //    0      1      1      1   =>   no                  0
        //    1      0      0      0   =>   yes filled above    1
        //    1      0      0      1   =>   yes filled above    1
        //    1      0      1      0   =>   no                  0
        //    1      0      1      1   =>   no                  0
        //    1      1      0      0   =>   no                  0
        //    1      1      0      1   =>   yes filled above    1
        //    1      1      1      0   =>   yes filled below    2
        //    1      1      1      1   =>   no                  0
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

    // prettier-ignore
    static member differenceRev(segments: SegmentBool[], log: BuildLog option) : SegmentBool[] =
        // secondary - primary
        // above1 below1 above2 below2    Keep?               Value
        //    0      0      0      0   =>   yes if open         4
        //    0      0      0      1   =>   yes filled below    2
        //    0      0      1      0   =>   yes filled above    1
        //    0      0      1      1   =>   no                  0
        //    0      1      0      0   =>   no                  0
        //    0      1      0      1   =>   no                  0
        //    0      1      1      0   =>   yes filled above    1
        //    0      1      1      1   =>   yes filled above    1
        //    1      0      0      0   =>   no                  0
        //    1      0      0      1   =>   yes filled below    2
        //    1      0      1      0   =>   no                  0
        //    1      0      1      1   =>   yes filled below    2
        //    1      1      0      0   =>   no                  0
        //    1      1      0      1   =>   no                  0
        //    1      1      1      0   =>   no                  0
        //    1      1      1      1   =>   no                  0
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

    // prettier-ignore
    static member xor(segments: SegmentBool[], log: BuildLog option) : SegmentBool[] =
        // primary ^ secondary
        // above1 below1 above2 below2    Keep?               Value
        //    0      0      0      0   =>   yes if open         4
        //    0      0      0      1   =>   yes filled below    2
        //    0      0      1      0   =>   yes filled above    1
        //    0      0      1      1   =>   no                  0
        //    0      1      0      0   =>   yes filled below    2
        //    0      1      0      1   =>   no                  0
        //    0      1      1      0   =>   no                  0
        //    0      1      1      1   =>   yes filled above    1
        //    1      0      0      0   =>   yes filled above    1
        //    1      0      0      1   =>   no                  0
        //    1      0      1      0   =>   no                  0
        //    1      0      1      1   =>   yes filled below    2
        //    1      1      0      0   =>   no                  0
        //    1      1      0      1   =>   yes filled above    1
        //    1      1      1      0   =>   yes filled below    2
        //    1      1      1      1   =>   no                  0
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
