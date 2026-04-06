#r "bin/Debug/net10.0/PolyBool.dll"

open System
open PolyBool

let polybool: PolyBool = PolyBoolExports.polybool

let triangle1: Polygon =
    {
        regions =
            [|
                [|
                    [| 0.0; 0.0 |]
                    [| 5.0; 10.0 |]
                    [| 10.0; 0.0 |]
                |]
            |]
        inverted = false
    }

let triangle2: Polygon =
    {
        regions =
            [|
                [|
                    [| 5.0; 0.0 |]
                    [| 10.0; 10.0 |]
                    [| 15.0; 0.0 |]
                |]
            |]
        inverted = false
    }

let box1: Polygon =
    {
        regions =
            [|
                [|
                    [| 0.0; 0.0 |]
                    [| 5.0; 0.0 |]
                    [| 5.0; -5.0 |]
                    [| 0.0; -5.0 |]
                |]
            |]
        inverted = false
    }

type Receiver() =
    let log: ResizeArray<obj> = ResizeArray<obj>()

    member this.``done``() : obj[] =
        log.ToArray()

    interface IPolyBoolReceiver with
        member _.beginPath() : unit =
            log.Add(box "beginPath")

        member _.moveTo(x: float, y: float) : unit =
            log.Add(box "moveTo")
            log.Add(box x)
            log.Add(box y)

        member _.lineTo(x: float, y: float) : unit =
            log.Add(box "lineTo")
            log.Add(box x)
            log.Add(box y)

        member _.closePath() : unit =
            log.Add(box "closePath")

let assertEqual<'T when 'T: equality>(a: 'T, b: 'T) : unit =
    if a <> b then
        printfn "Values do not match:\n%A\n%A" a b
        failwith "Values not equal"

let assertThrowsContaining(expected: string, action: unit -> unit) : unit =
    let mutable caught: exn option = None

    try
        action()
    with err ->
        caught <- Some err

    match caught with
    | Some err when err.Message.Contains(expected) ->
        ()
    | Some err ->
        printfn "Unexpected exception message:\n%s" err.Message
        failwith "Unexpected exception message"
    | None ->
        failwith "Expected an exception"

let tests: (string * (unit -> unit))[] =
    [|
        "basic intersection",
        (fun () ->
            assertEqual(
                polybool.intersect(triangle1, triangle2),
                {
                    regions =
                        [|
                            [|
                                [| 10.0; 0.0 |]
                                [| 5.0; 0.0 |]
                                [| 7.5; 5.0 |]
                            |]
                        |]
                    inverted = false
                }
            ))
        "basic union",
        (fun () ->
            assertEqual(
                polybool.union(triangle1, triangle2),
                {
                    regions =
                        [|
                            [|
                                [| 10.0; 10.0 |]
                                [| 7.5; 5.0 |]
                                [| 5.0; 10.0 |]
                                [| 0.0; 0.0 |]
                                [| 15.0; 0.0 |]
                            |]
                        |]
                    inverted = false
                }
            ))
        "rejects non-polygon vertex",
        (fun () ->
            assertThrowsContaining(
                "only polygon vertices are supported",
                fun () ->
                    polybool.segments(
                        {
                            regions =
                                [|
                                    [|
                                        [| 0.0; 0.0 |]
                                        [| 0.0; -5.0; 10.0; -5.0; 10.0; 0.0 |]
                                    |]
                                |]
                            inverted = false
                        }
                    )
                    |> ignore
            ))
        "example",
        (fun () ->
            let receiver: Receiver = Receiver()

            let log =
                polybool
                    .shape()
                    .beginPath()
                    .moveTo(50.0, 50.0)
                    .lineTo(150.0, 150.0)
                    .lineTo(190.0, 50.0)
                    .closePath()
                    .moveTo(130.0, 50.0)
                    .lineTo(290.0, 150.0)
                    .lineTo(290.0, 50.0)
                    .closePath()
                    .combine(
                        polybool
                            .shape()
                            .beginPath()
                            .moveTo(110.0, 20.0)
                            .lineTo(110.0, 110.0)
                            .lineTo(20.0, 20.0)
                            .closePath()
                            .moveTo(130.0, 170.0)
                            .lineTo(130.0, 20.0)
                            .lineTo(260.0, 20.0)
                            .lineTo(260.0, 170.0)
                            .closePath()
                    )
                    .intersect()
                    .output(receiver)
                    .``done``()

            assertEqual(
                log,
                [|
                    box "beginPath"
                    box "moveTo"; box 110.0; box 110.0
                    box "lineTo"; box 50.0; box 50.0
                    box "lineTo"; box 110.0; box 50.0
                    box "lineTo"; box 110.0; box 110.0
                    box "closePath"
                    box "moveTo"; box 150.0; box 150.0
                    box "lineTo"; box 178.0; box 80.0
                    box "lineTo"; box 130.0; box 50.0
                    box "lineTo"; box 130.0; box 130.0
                    box "lineTo"; box 150.0; box 150.0
                    box "closePath"
                    box "moveTo"; box 260.0; box 131.25
                    box "lineTo"; box 178.0; box 80.0
                    box "lineTo"; box 190.0; box 50.0
                    box "lineTo"; box 260.0; box 50.0
                    box "lineTo"; box 260.0; box 131.25
                    box "closePath"
                |]
            ))
        "transforms",
        (fun () ->
            let receiver: Receiver = Receiver()

            let log =
                polybool
                    .shape()
                    .setTransform(3.0, 0.0, 0.0, 2.0, 100.0, 200.0)
                    .beginPath()
                    .moveTo(50.0, 50.0)
                    .lineTo(-10.0, 50.0)
                    .lineTo(10.0, 10.0)
                    .closePath()
                    .output(receiver)
                    .``done``()

            assertEqual(
                log,
                [|
                    box "beginPath"
                    box "moveTo"; box 250.0; box 300.0
                    box "lineTo"; box 70.0; box 300.0
                    box "lineTo"; box 130.0; box 220.0
                    box "lineTo"; box 250.0; box 300.0
                    box "closePath"
                |]
            ))
    |]

let mutable pass: int = 0
let mutable fail: int = 0

for name, func in tests do
    try
        func()
        printfn "pass   %s" name
        pass <- pass + 1
    with err ->
        printfn "FAIL   %s" name
        printfn "%s failed: %O" name err
        fail <- fail + 1

printfn "\nPass: %d\nFail: %d" pass fail

if fail > 0 then
    Environment.ExitCode <- 1
