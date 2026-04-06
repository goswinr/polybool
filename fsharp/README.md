# polybool (F# port)

Boolean operations on polygons (union, intersection, difference, xor).

This folder contains an F# port of the TypeScript implementation from the root
of the repository. If you want the type-by-type translation notes, see
[PORT-NOTES.md](d:/Git/polybool/fsharp/PORT-NOTES.md).

The F# port is polygon-only. Regions must be lists of 2D vertices, and the
instructional API only supports `moveTo`, `lineTo`, and `closePath`.

# Features

1. Clips polygons for all boolean operations
2. Removes unnecessary vertices
3. Handles segments that are coincident (overlap perfectly, share vertices, one
   inside the other, etc)
4. Uses formulas that take floating point irregularities into account (via
   configurable epsilon)
5. Provides an API for constructing efficient sequences of operations
6. Outputs exterior paths as counter-clockwise, and holes as clockwise (right-hand rule)
7. Supports polygon vertices and line segments only
8. F# implementation

# Resources

* [Demo + Animation](https://unpkg.com/@velipso/polybool@2.0.11/demo/demo.html)
* [Companion Tutorial](https://sean.fun/a/polygon-clipping-pt2)
* Based somewhat on the F. Martinez (2008) algorithm:
  [Paper](http://www.cs.ucr.edu/~vbz/cs230papers/martinez_boolean.pdf),
  [Code](https://github.com/akavel/martinez-src)

### Ports

Other kind souls have ported this library:

* [Flutter/Dart port by @mohammedX6](https://github.com/mohammedX6/poly_bool_dart)
* [Java port by @the3deers](https://github.com/the3deers/polybool-java)
* [Java port by @Menecats](https://github.com/Menecats/polybool-java)
* [Kotlin port by @StefanOltmann](https://github.com/StefanOltmann/polybool-kotlin)
* [.NET port by @idormenco](https://github.com/idormenco/PolyBool.Net)
* [Python port by @KaivnD](https://github.com/KaivnD/pypolybool)
* [Roblox (Luau/Typescript) port by @codyduong](https://github.com/codyduong/rbxts-polybool)
* Please make a ticket if you'd like to be added here :-)

# Installing

This F# port is currently source-first. Add a project reference to
[PolyBool.fsproj](d:/Git/polybool/fsharp/PolyBool.fsproj):

```powershell
dotnet add <your-project>.fsproj reference path\to\PolyBool.fsproj
```

Or build it directly:

```powershell
dotnet build fsharp/PolyBool.fsproj
```

Then use it from F# with:

```fsharp
open PolyBool

let polybool = PolyBoolExports.polybool
```

# Example

![Example](../example.png)

Using the Simplified Polygonal API:

```fsharp
open PolyBool

let polybool = PolyBoolExports.polybool

let result =
    polybool.intersect(
        {
            regions =
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
            inverted = false
        },
        {
            regions =
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
            inverted = false
        }
    )

printfn "%A" result

// output:
// { regions =
//    [|[|[|50.0; 50.0|]; [|110.0; 50.0|]; [|110.0; 110.0|]|]
//      [|[|178.0; 80.0|]; [|130.0; 50.0|]; [|130.0; 130.0|]; [|150.0; 150.0|]|]
//      [|[|178.0; 80.0|]; [|190.0; 50.0|]; [|260.0; 50.0|]; [|260.0; 131.25|]|]|]
//   inverted = false }
```

Using the Polygonal API:

```fsharp
open PolyBool

let polybool = PolyBoolExports.polybool

let poly1 =
    {
        regions =
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
        inverted = false
    }

let poly2 =
    {
        regions =
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
        inverted = false
    }

let segs1 = polybool.segments(poly1)
let segs2 = polybool.segments(poly2)
let combined = polybool.combine(segs1, segs2)
let segs3 = polybool.selectIntersect(combined)
let result = polybool.polygon(segs3)

printfn "%A" result
```

Using the Instructional API:

```fsharp
open PolyBool

let polybool = PolyBoolExports.polybool

let shape1 =
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

let shape2 =
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

let receiver =
    { new IPolyBoolReceiver with
        member _.beginPath() = printfn "beginPath"
        member _.moveTo(x, y) = printfn $"moveTo {x} {y}"
        member _.lineTo(x, y) = printfn $"lineTo {x} {y}"

        member _.closePath() = printfn "closePath" }

shape1
    .combine(shape2)
    .intersect()
    .output(receiver)
|> ignore
```

# API Design

There are three different APIs, each of which use the same underlying algorithms:

1. Simplified Polygonal API
2. Polygonal API
3. Instructional API

The Simplified Polygonal API is implemented on top of the Polygonal API, and the Polygonal API is
implemented on top of the Instructional API.

The reason for multiple APIs is to maintain backwards compatibility and to make it easier to use.

# Simplified Polygonal API

```fsharp
open PolyBool

let polybool = PolyBoolExports.polybool

let unionPoly = polybool.union(poly1, poly2)
let intersectPoly = polybool.intersect(poly1, poly2)
let differencePoly = polybool.difference(poly1, poly2) // poly1 - poly2
let differenceRevPoly = polybool.differenceRev(poly1, poly2) // poly2 - poly1
let xorPoly = polybool.xor(poly1, poly2)
```

Where `poly1`, `poly2`, and the return value are `Polygon` records, in the format of:

```fsharp
let polygon: Polygon =
    {
        regions =
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
        inverted = false
    }
```

Each vertex must contain exactly two coordinates, `[| x; y |]`. Values with
more than two coordinates are rejected by `polybool.segments(...)`.

# Polygonal API

```fsharp
let segments1 = polybool.segments(polygon1)
let segments2 = polybool.segments(polygon2)
let combined = polybool.combine(segments1, segments2)
let unionSegments = polybool.selectUnion(combined)
let intersectSegments = polybool.selectIntersect(combined)
let differenceSegments = polybool.selectDifference(combined)
let differenceRevSegments = polybool.selectDifferenceRev(combined)
let xorSegments = polybool.selectXor(combined)
let polygon = polybool.polygon(unionSegments)
```

Depending on your needs, it might be more efficient to construct your own
sequence of operations using the Polygonal API. Note that `polybool.union`,
`polybool.intersect`, etc, are just thin wrappers for convenience.

There are three types of objects you will encounter in the Polygonal API:

1. Polygons (discussed above, this is a list of regions and an `inverted` flag)
2. Segments
3. Combined Segments

The basic flow chart of the API is:

![API Flow Chart](../flowchart.png)

You start by converting Polygons to Segments using `polybool.segments(poly)`.

You convert Segments to Combined Segments using `polybool.combine(seg1, seg2)`.

You select the resulting Segments from the Combined Segments using one of the
selection operators `polybool.selectUnion(combined)`,
`polybool.selectIntersect(combined)`, etc. These selection functions return
Segments.

Once you're done, you convert the Segments back to Polygons using
`polybool.polygon(segments)`.

Each transition is costly, so you want to navigate wisely. The selection
transition is the least costly.

## Advanced Example 1

Suppose you wanted to union a list of polygons together. The naive way to do it
would be:

```fsharp
// works but not efficient
let mutable result = polygons.[0]

for i = 1 to polygons.Length - 1 do
    result <- polybool.union(result, polygons.[i])

result
```

Instead, it's more efficient to use the Polygonal API directly, like this:

```fsharp
// works AND efficient
let mutable segments = polybool.segments(polygons.[0])

for i = 1 to polygons.Length - 1 do
    let seg2 = polybool.segments(polygons.[i])
    let comb = polybool.combine(segments, seg2)
    segments <- polybool.selectUnion(comb)

polybool.polygon(segments)
```

## Advanced Example 2

Suppose you want to calculate all operations on two polygons. The naive way to
do it would be:

```fsharp
// works but not efficient
let result =
    {| union = polybool.union(poly1, poly2)
       intersect = polybool.intersect(poly1, poly2)
       difference = polybool.difference(poly1, poly2)
       differenceRev = polybool.differenceRev(poly1, poly2)
       xor = polybool.xor(poly1, poly2) |}
```

Instead, it's more efficient to use the Polygonal API directly, like this:

```fsharp
// works AND efficient
let seg1 = polybool.segments(poly1)
let seg2 = polybool.segments(poly2)
let comb = polybool.combine(seg1, seg2)

let result =
    {| union = polybool.polygon(polybool.selectUnion(comb))
       intersect = polybool.polygon(polybool.selectIntersect(comb))
       difference = polybool.polygon(polybool.selectDifference(comb))
       differenceRev = polybool.polygon(polybool.selectDifferenceRev(comb))
       xor = polybool.polygon(polybool.selectXor(comb)) |}
```

## Advanced Example 3

As an added bonus, just going from Polygon to Segments and back performs
simplification on the polygon.

Suppose you have garbage polygon data and just want to clean it up. The naive
way to do it would be:

```fsharp
// union the polygon with nothing in order to clean up the data
// works but not efficient
let cleaned =
    polybool.union(
        polygon,
        {
            regions = [||]
            inverted = false
        }
    )
```

Instead, skip the combination and selection phase:

```fsharp
// works AND efficient
let cleaned = polybool.polygon(polybool.segments(polygon))
```

# Instructional API

The Instructional API does not have an intermediate data format (like the Polygon from before), and
does not support an `inverted` flag.

Instead, the Instructional API is modeled after the
[CanvasRenderingContext2D](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D)
API, but the F# port only exposes the polygonal subset.

Shapes are created using `beginPath`, `moveTo`, `lineTo`, and `closePath`, then
combined together, operated on, and output to a _receiver_.

The receiver is an object with `beginPath`, `moveTo`, `lineTo`, and `closePath`
defined, and those methods are called in order to output the result.

Unlike the other APIs, the Instructional API supports _open paths_, which can be used by skipping
the call to `closePath` at the end. This could be useful for intersecting a rectangle with a
line segment, for example.

```fsharp
type IPolyBoolReceiver =
    abstract beginPath: unit -> unit
    abstract moveTo: float * float -> unit
    abstract lineTo: float * float -> unit
    abstract closePath: unit -> unit
```

## Shapes

The first step is to create shapes:

```fsharp
let shape =
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
```

Note that shapes can have multiple regions by calling `moveTo` more than once. Shapes support
open and closed paths, so calling `closePath` is required if the path is filled.

```fsharp
type Shape with
    member beginPath: unit -> Shape
    member moveTo: float * float -> Shape
    member lineTo: float * float -> Shape
    member closePath: unit -> Shape
```

## Combining Shapes

Once you have multiple shapes, you can combine them:

```fsharp
let combinedShape1 = shape1.combine(shape2)
let combinedShape2 = shape1.combine(shape3)
```

Notice that you can use shapes in multiple operations, but once you use a shape in an operation,
you can't add more line segments to it.

```fsharp
type Shape with
    member combine: Shape -> ShapeCombined
```

## Performing an Operation

Once you have a combined shape, you can generate new shapes by performing a boolean operation:

```fsharp
let intersectionShape = combinedShape1.intersect()
let unionShape = combinedShape1.union()
```

Notice that you can use a combined shape more than once, to produce different boolean operations.

```fsharp
type ShapeCombined with
    member union: unit -> Shape
    member intersect: unit -> Shape
    member difference: unit -> Shape
    member differenceRev: unit -> Shape
    member xor: unit -> Shape
```

## Outputting Results

_Any_ shape can be output to a _receiver_:

```fsharp
shape.output(receiver) |> ignore
```

Notice that `shape` could be the result of a boolean operation, but it doesn't have to be.

```fsharp
type Shape with
    member output<'T when 'T :> IPolyBoolReceiver> : receiver: 'T -> 'T
```

The `receiver` object is returned.

## Implementing Inversion

If you need to perform logic on inverted shapes like the Polygonal API supports, a key observation
is that you can represent the same result by shuffling around inversion flags and choosing the right
operation.

For example, if you are intersecting two shapes, and the first one is inverted, then that is
equivalent to the `differenceRev` operation.

This is how inversion is supported in the Polygonal API, even though it is built on top of the
Instructional API which does not support inversion.

Please check the source code to see how inversion is calculated. Here is intersection, for example:

```fsharp
member this.selectIntersect(combined: CombinedSegments) : Segments =
    {
        shape =
            if combined.inverted1 then
                if combined.inverted2 then
                    combined.shape.union()
                else
                    combined.shape.differenceRev()
            elif combined.inverted2 then
                combined.shape.difference()
            else
                combined.shape.intersect()
        inverted = combined.inverted1 && combined.inverted2
    }
```

Essentially, this represents observations like `intersect(~A, ~B) = ~union(A, B)`,
[etc](https://en.wikipedia.org/wiki/De_Morgan's_laws).

## Advanced Example 1

How to union a list of shapes together:

```fsharp
let mutable result = shapes.[0]

for i = 1 to shapes.Length - 1 do
    result <- result.combine(shapes.[i]).union()

result.output(receiver) |> ignore
```

## Advanced Example 2

How to calculate all operations on two shapes:

```fsharp
let combined = shape1.combine(shape2)

combined.union().output(receiverUnion) |> ignore
combined.intersect().output(receiverIntersect) |> ignore
combined.difference().output(receiverDifference) |> ignore
combined.differenceRev().output(receiverDifferenceRev) |> ignore
combined.xor().output(receiverXor) |> ignore
```

## Advanced Example 3

As an added bonus, you can simplify shapes by outputting them directly.

Suppose you have garbage polygon data and just want to clean it up. The naive
way to do it would be:

```fsharp
// union the shape with nothing in order to clean up the data
// works but not efficient
shape1.combine(polybool.shape()).union().output(receiver) |> ignore
```

Instead, skip the combination and operation:

```fsharp
// works AND efficient
shape1.output(receiver) |> ignore
```

# Epsilon

Due to the beauty of floating point reality, floating point calculations are not
exactly perfect. This is a problem when trying to detect whether lines are on
top of each other, or if vertices are exactly the same.

Normally you would expect this to work:

```fsharp
if a = b then
    // a and b are equal
    ()
else
    // a and b are not equal
    ()
```

But for inexact floating point math, instead we use:

```fsharp
if abs (a - b) < epsilon then
    // a and b are equal
    ()
else
    // a and b are not equal
    ()
```

You can set the epsilon value using:

```fsharp
open PolyBool

let polybool =
    PolyBool(
        geo = (GeometryEpsilon(0.000001) :> Geometry)
    )
```

The default epsilon value is `0.0000000001`.

If your polygons are really really large or really really tiny, then you will
probably have to come up with your own epsilon value -- otherwise, the default
should be fine.

If `PolyBool` detects that your epsilon is too small or too large, it will throw
an error:

```text
PolyBool: Zero-length segment detected; your epsilon is probably too small or too large
```
