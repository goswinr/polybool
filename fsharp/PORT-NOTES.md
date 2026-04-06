# F# Port Notes

This folder contains a direct F# translation of the TypeScript implementation in `src/`.

The goal of the port was to keep the original shape recognizable:

- TypeScript files became F# files/modules with similar names.
- Classes stayed classes where that helped preserve the original API.
- Mutable algorithmic code stayed mutable instead of being redesigned into a more idiomatic F# style.
- Optional and nullable TypeScript values were mapped explicitly so the control flow is still easy to compare with the source.

## Main Type Mapping

| TypeScript | F# |
| --- | --- |
| `type Vec2 = [number, number]` | `type Vec2 = float[]` |
| `type Vec6 = [number, number, number, number, number, number]` | `type Vec6 = float[]` |
| `[Vec2, Vec2]` bounding box pairs | `type BBox = Vec2 * Vec2` |
| `number` | `float` |
| `Array<T>` | `T[]` or `ResizeArray<T>` when mutation is required |
| `boolean \| null` | `bool option` |
| `T \| null` | `T option` |
| `interface Polygon` | `type Polygon = { regions: float[][][]; inverted: bool }` |
| `interface Segments` | `type Segments = { shape: Shape; inverted: bool }` |
| `interface CombinedSegments` | `type CombinedSegments = { shape: ShapeCombined; inverted1: bool; inverted2: bool }` |
| `abstract class Geometry` | concrete `type Geometry(?epsilon: float)` |
| `class GeometryEpsilon extends Geometry` | compatibility alias `type GeometryEpsilon = Geometry` |
| `default class BuildLog` | `type BuildLog()` |
| `interface SegmentBoolFill { above: boolean \| null; below: boolean \| null }` | `type SegmentBoolFill = { mutable above: bool option; mutable below: bool option }` |
| `interface ListBoolTransition<T>` | `type ListBoolTransition<'T> = { before: 'T option; after: 'T option; insert: 'T -> 'T }` |
| `abstract class SegmentBase<T>` | concrete `type Segment(...)` in the polygon-only port |
| `class SegmentLine extends SegmentBase<SegmentLine>` | folded into concrete `type Segment(...)` |
| `class SegmentCurve extends SegmentBase<SegmentCurve>` | `type SegmentCurve(... ) = inherit Segment(...)` |
| `type Segment = SegmentLine \| SegmentCurve` | polygon-only port keeps a single concrete `Segment` class |
| `interface SegmentTValuePairs` / `interface SegmentTRangePairs` with `kind` | concrete `SegmentIntersection` helper class plus `SegmentIntersectionKind` |
| `class SegmentTValuesBuilder` | `type SegmentTValuesBuilder(...)` |
| `class SegmentTValuePairsBuilder` | `type SegmentTValuePairsBuilder(...)` |
| `class SegmentBoolBase<T>` | abstract base type `SegmentBool` |
| `class SegmentBoolLine extends SegmentBoolBase<SegmentLine>` | `type SegmentBoolLine(... ) = inherit SegmentBool(...)` |
| `class SegmentBoolCurve extends SegmentBoolBase<SegmentCurve>` | `type SegmentBoolCurve(... ) = inherit SegmentBool(...)` |
| `type SegmentBool = SegmentBoolLine \| SegmentBoolCurve` | abstract base type `SegmentBool` with runtime subtype checks |
| `class EventBool` | `type EventBool(...)` |
| `class ListBool<T>` | `type ListBool<'T when 'T: equality>()` |
| `class Intersecter` | `type Intersecter(...)` |
| `interface IPolyBoolReceiver` | concrete callback class `type PolyBoolReceiver(...)` |
| `function SegmentChainer(...)` | `module SegmentChainer` with `segmentChainer(...)` |
| `class SegmentSelector` with static methods | `type SegmentSelector private ()` with static members |
| path-state tagged union objects in `Shape.ts` | private F# discriminated unions `PathState` and `ShapeResultState` |
| `class Shape` | `type Shape(...)` |
| `class ShapeCombined` | `and ShapeCombined(...)` |
| `class PolyBool` | `type PolyBool(?geo: Geometry, ?log: BuildLog)` |
| `export default polybool` | `module PolyBoolExports` with `let polybool = PolyBool()` |

## Common Translation Patterns

| TypeScript Pattern | F# Port Pattern |
| --- | --- |
| `foo?: T` or `foo: T \| null` | `foo: T option` |
| `fill?.above ?? null` | `fill |> Option.bind (fun x -> x.above)` |
| `instanceof SegmentLine` | no runtime subtype check needed in the polygon-only port |
| `Array<T>.push/splice/shift/unshift` | `ResizeArray<T>.Add/Insert/RemoveAt` |
| anonymous object payloads for logging | `box {| ... |}` |
| `export function ...` | `let ...` inside an F# module |
| `export class ...` | top-level `type ...` |
| `false` sentinel values in logs | boxed `false` values where needed for parity |

## A Few Intentional Shape Differences

These are the main places where the F# port differs slightly from the TypeScript surface:

1. `Vec2` and `Vec6` are stored as `float[]` instead of fixed-length tuples.
   This keeps indexing behavior close to the original TypeScript code.

2. Earlier iterations of the F# port represented `SegmentLine | SegmentCurve` as class hierarchies.
   The current polygon-only port removes that hierarchy and keeps a single concrete `Segment` type.

3. Nullable TypeScript fields became `option` values.
   This is most visible in fill state, optional logs, and intermediate lookup results.

4. Some TypeScript free functions became F# module functions.
   Examples: `GeometryFunctions`, `SegmentFunctions`, `SegmentChainer.segmentChainer`, and `PolyBoolExports`.

## File Guide

| TypeScript Source | F# Port |
| --- | --- |
| `src/Geometry.ts` | `Geometry.fs` |
| `src/BuildLog.ts` | `BuildLog.fs` |
| `src/Segment.ts` | `Segment.fs` |
| `src/Intersecter.ts` | `Intersecter.fs` |
| `src/SegmentSelector.ts` | `SegmentSelector.fs` |
| `src/SegmentChainer.ts` | `SegmentChainer.fs` |
| `src/Shape.ts` | `Shape.fs` |
| `src/polybool.ts` | `PolyBool.fs` |
| `src/polybool.test.ts` | `polybool.test.fsx` |

## Running The Port

```powershell
dotnet build fsharp/fsharp.fsproj
dotnet fsi fsharp/polybool.test.fsx
```
