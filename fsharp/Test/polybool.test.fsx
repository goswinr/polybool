// adapted from https://github.com/velipso/polybool/blob/main/src/polybool.test.ts

#r "../PolyBool/bin/Debug/netstandard2.0/PolyBool.dll"

open System
open PolyBool

let polybool = PolyBool()

let triangle1Regions: float[][] =
    [|
        [| 0.0; 0.0; 5.0; 10.0; 10.0; 0.0 |]
    |]

let triangle2Regions: float[][] =
    [|
        [| 5.0; 0.0; 10.0; 10.0; 15.0; 0.0 |]
    |]

let example1Regions: float[][] =
    [|
        [| 50.0; 50.0; 150.0; 150.0; 190.0; 50.0 |]
        [| 130.0; 50.0; 290.0; 150.0; 290.0; 50.0 |]
    |]

let example2Regions: float[][] =
    [|
        [| 110.0; 20.0; 110.0; 110.0; 20.0; 20.0 |]
        [| 130.0; 170.0; 130.0; 20.0; 260.0; 20.0; 260.0; 170.0 |]
    |]

let assertEqual<'T when 'T: equality>(a: 'T, b: 'T) : unit =
    if a <> b then
        printfn "Values do not match:\n%A\n%A" a b
        failwith "Values not equal"

let tests: (string * (unit -> unit))[] =
    [|
        "basic intersection",
        (fun () ->
            assertEqual(
                polybool.Intersect(triangle1Regions, triangle2Regions),
                [|
                    [| 10.0; 0.0; 5.0; 0.0; 7.5; 5.0 |]
                |]
            ))
        "basic union",
        (fun () ->
            assertEqual(
                polybool.Union(triangle1Regions, triangle2Regions),
                [|
                    [| 10.0; 10.0; 7.5; 5.0; 5.0; 10.0; 0.0; 0.0; 15.0; 0.0 |]
                |]
            ))
        "array input",
        (fun () ->
            assertEqual(
                polybool.Intersect(triangle1Regions, triangle2Regions),
                [|
                    [| 10.0; 0.0; 5.0; 0.0; 7.5; 5.0 |]
                |]
            ))
        "example",
        (fun () ->
            assertEqual(
                polybool.Intersect(example1Regions, example2Regions),
                [|
                    [| 50.0; 50.0; 110.0; 50.0; 110.0; 110.0 |]
                    [| 178.0; 80.0; 130.0; 50.0; 130.0; 130.0; 150.0; 150.0 |]
                    [| 178.0; 80.0; 190.0; 50.0; 260.0; 50.0; 260.0; 131.25 |]
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
