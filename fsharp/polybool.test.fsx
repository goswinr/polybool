#r "bin/Debug/net10.0/PolyBool.dll"

open System
open PolyBool

let polybool: PolyBool = PolyBoolExports.polybool

let triangle1Regions: Vec2[][] =
    [|
        [|
            [| 0.0; 0.0 |]
            [| 5.0; 10.0 |]
            [| 10.0; 0.0 |]
        |]
    |]

let triangle2Regions: Vec2[][] =
    [|
        [|
            [| 5.0; 0.0 |]
            [| 10.0; 10.0 |]
            [| 15.0; 0.0 |]
        |]
    |]

let example1Regions: Vec2[][] =
    [|
        [|
            [| 50.0; 50.0 |]
            [| 150.0; 150.0 |]
            [| 190.0; 50.0 |]
        |]
        [|
            [| 130.0; 50.0 |]
            [| 290.0; 150.0 |]
            [| 290.0; 50.0 |]
        |]
    |]

let example2Regions: Vec2[][] =
    [|
        [|
            [| 110.0; 20.0 |]
            [| 110.0; 110.0 |]
            [| 20.0; 20.0 |]
        |]
        [|
            [| 130.0; 170.0 |]
            [| 130.0; 20.0 |]
            [| 260.0; 20.0 |]
            [| 260.0; 170.0 |]
        |]
    |]

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
                polybool.intersect(triangle1Regions, triangle2Regions),
                [|
                    [|
                        [| 10.0; 0.0 |]
                        [| 5.0; 0.0 |]
                        [| 7.5; 5.0 |]
                    |]
                |]
            ))
        "basic union",
        (fun () ->
            assertEqual(
                polybool.union(triangle1Regions, triangle2Regions),
                [|
                    [|
                        [| 10.0; 10.0 |]
                        [| 7.5; 5.0 |]
                        [| 5.0; 10.0 |]
                        [| 0.0; 0.0 |]
                        [| 15.0; 0.0 |]
                    |]
                |]
            ))
        "rejects non-polygon vertex",
        (fun () ->
            assertThrowsContaining(
                "only polygon vertices are supported",
                fun () ->
                    polybool.union(
                        [|
                            [|
                                [| 0.0; 0.0 |]
                                [| 0.0; -5.0; 10.0; -5.0; 10.0; 0.0 |]
                            |]
                        |],
                        triangle1Regions
                    )
                    |> ignore
            ))
        "array input",
        (fun () ->
            assertEqual(
                polybool.intersect(triangle1Regions, triangle2Regions),
                [|
                    [|
                        [| 10.0; 0.0 |]
                        [| 5.0; 0.0 |]
                        [| 7.5; 5.0 |]
                    |]
                |]
            ))
        "example",
        (fun () ->
            assertEqual(
                polybool.intersect(example1Regions, example2Regions),
                [|
                    [|
                        [| 50.0; 50.0 |]
                        [| 110.0; 50.0 |]
                        [| 110.0; 110.0 |]
                    |]
                    [|
                        [| 178.0; 80.0 |]
                        [| 130.0; 50.0 |]
                        [| 130.0; 130.0 |]
                        [| 150.0; 150.0 |]
                    |]
                    [|
                        [| 178.0; 80.0 |]
                        [| 190.0; 50.0 |]
                        [| 260.0; 50.0 |]
                        [| 260.0; 131.25 |]
                    |]
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
