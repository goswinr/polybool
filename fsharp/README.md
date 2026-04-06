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

Public F# API note:

The current F# port publicly exposes only the simplified polygonal API shown above.
The lower polygon/shape layers from the original TypeScript implementation still
exist inside the port, but they are internal implementation details rather than a
supported public surface.

# API Design

The original TypeScript project is layered, and the F# port still follows the
same internal pipeline, but the supported public surface is intentionally much
smaller: polygon-only boolean operations over `Vec2[][]` or `Vec2 list list`.

The public entry points are the `PolyBool` overloads for:

1. `union`
2. `intersect`
3. `difference`
4. `differenceRev`
5. `xor`

Each vertex must contain exactly two coordinates, `[| x; y |]`. Values with
more than two coordinates are rejected.

You can also provide optional infrastructure objects when constructing
`PolyBool`:

```fsharp
open PolyBool

let polybool =
    PolyBool(
        geo = Geometry(0.000001),
        log = BuildLog()
    )
```

Internally, the port still uses segment construction, intersection handling,
and path reconstruction to mirror the original algorithm. Those lower layers are
kept as implementation details rather than a supported public API.

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
        geo = Geometry(0.000001)
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
