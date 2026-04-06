# Vec2 Removal Refactor

Removed the `Vec2` type (`float[]`) and inlined x/y coordinates as separate floats throughout the codebase. Functions that returned `Vec2` now write to `Geometry.resultX`/`resultY`. Polygon regions use flat `float[]` with interleaved x,y coordinates.

## Changes per file

- **Geometry.fs**: Removed `Vec2` and `BBox` types. Added `resultX`/`resultY` as global return mechanism. Changed `isCollinear`, `isEqualVec2`, `compareVec2` to take individual `float` parameters.

- **BuildLog.fs**: Changed `SegmentDivide` from `p: Vec2` to `px: float, py: float`.

- **Segment.fs**: `Segment` now stores 4 mutable floats (`P0X`, `P0Y`, `P1X`, `P1Y`). `PointTo(t)` writes to `Geometry.resultX/resultY`. `SegmentIntersection` uses separate `TStartA/TStartB/TEndA/TEndB` floats and flat `float[]` for `TValuePairs` (interleaved t1,t2). `SegmentTValuePairsBuilder` uses flat `ResizeArray<float>`.

- **Intersecter.fs**: `EventBool` stores `PX`/`PY` instead of `P: Vec2`. `CompareEvents` and `AddLine` take individual x,y floats. `DivideEvent` takes `px, py`.

- **SegmentChainer.fs**: All point access uses `P0X/P0Y/P1X/P1Y` directly instead of `Start()/end()` returning Vec2.

- **Shape.fs**: `PathState.MoveTo` stores 4 floats instead of 2 Vec2s.

- **PolyBool.fs**: `Polygon.regions` is now `float[][]` with interleaved x,y coordinates. Public API methods (`Union`, `Intersect`, etc.) use `float[][]`.
