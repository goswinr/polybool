namespace PolyBool

open System

//
// polybool - Boolean operations on polygons (union, intersection, etc)
// by Sean Connelly (@velipso), https://sean.fun
// Project Home: https://github.com/velipso/polybool
// SPDX-License-Identifier: 0BSD
//

type Vec2 = float[]
type Vec6 = float[]
type BBox = Vec2 * Vec2

module GeometryFunctions =
    let lerp (a: float) (b: float) (t: float) : float =
        a + (b - a) * t

    let lerpVec2 (a: Vec2) (b: Vec2) (t: float) : Vec2 =
        [| lerp a.[0] b.[0] t; lerp a.[1] b.[1] t |]

    let boundingBoxesIntersect (bbox1: BBox) (bbox2: BBox) : bool =
        let b1min, b1max = bbox1
        let b2min, b2max = bbox2

        not (
            b1min.[0] > b2max.[0]
            || b1max.[0] < b2min.[0]
            || b1min.[1] > b2max.[1]
            || b1max.[1] < b2min.[1]
        )

[<AbstractClass>]
type Geometry() =
    abstract snap0: float -> float
    abstract snap01: float -> float
    abstract isCollinear: Vec2 * Vec2 * Vec2 -> bool
    abstract solveCubic: float * float * float * float -> float[]
    abstract isEqualVec2: Vec2 * Vec2 -> bool
    abstract compareVec2: Vec2 * Vec2 -> int

type GeometryEpsilon(?epsilon: float) =
    inherit Geometry()

    let epsilonValue: float = defaultArg epsilon 0.0000000001

    member this.epsilon: float = epsilonValue

    override this.snap0(v: float) : float =
        if Math.Abs(v) < this.epsilon then
            0.0
        else
            v

    override this.snap01(v: float) : float =
        if Math.Abs(v) < this.epsilon then
            0.0
        elif Math.Abs(1.0 - v) < this.epsilon then
            1.0
        else
            v

    override this.isCollinear(p1: Vec2, p2: Vec2, p3: Vec2) : bool =
        // does pt1->pt2->pt3 make a straight line?
        // essentially this is just checking to see if
        //   slope(pt1->pt2) === slope(pt2->pt3)
        // if slopes are equal, then they must be collinear, because they share pt2
        let dx1: float = p1.[0] - p2.[0]
        let dy1: float = p1.[1] - p2.[1]
        let dx2: float = p2.[0] - p3.[0]
        let dy2: float = p2.[1] - p3.[1]
        Math.Abs(dx1 * dy2 - dx2 * dy1) < this.epsilon

    member private this.solveCubicNormalized(a: float, b: float, c: float) : float[] =
        // based somewhat on gsl_poly_solve_cubic from GNU Scientific Library
        let a3: float = a / 3.0
        let b3: float = b / 3.0
        let q: float = a3 * a3 - b3
        let r: float = a3 * (a3 * a3 - b / 2.0) + c / 2.0

        if Math.Abs(r) < this.epsilon && Math.Abs(q) < this.epsilon then
            [| -a3 |]
        else
            let f: float =
                a3 * (a3 * (4.0 * a3 * c - b3 * b) - 2.0 * b * c)
                + 4.0 * b3 * b3 * b3
                + c * c

            if Math.Abs(f) < this.epsilon then
                let sqrtQ: float = Math.Sqrt(q)

                if r > 0.0 then
                    [| -2.0 * sqrtQ - a / 3.0; sqrtQ - a / 3.0 |]
                else
                    [| -sqrtQ - a / 3.0; 2.0 * sqrtQ - a / 3.0 |]
            else
                let q3: float = q * q * q
                let r2: float = r * r

                if r2 < q3 then
                    let ratio: float = (if r < 0.0 then -1.0 else 1.0) * Math.Sqrt(r2 / q3)
                    let theta: float = Math.Acos(ratio)
                    let norm: float = -2.0 * Math.Sqrt(q)
                    let x0: float = norm * Math.Cos(theta / 3.0) - a3
                    let x1: float = norm * Math.Cos((theta + 2.0 * Math.PI) / 3.0) - a3
                    let x2: float = norm * Math.Cos((theta - 2.0 * Math.PI) / 3.0) - a3
                    let result: float[] = [| x0; x1; x2 |]
                    Array.sortInPlace result
                    result
                else
                    let aValue: float =
                        (if r < 0.0 then 1.0 else -1.0)
                        * Math.Pow(Math.Abs(r) + Math.Sqrt(r2 - q3), 1.0 / 3.0)

                    let bValue: float =
                        if Math.Abs(aValue) >= this.epsilon then
                            q / aValue
                        else
                            0.0

                    [| aValue + bValue - a3 |]

    override this.solveCubic(a: float, b: float, c: float, d: float) : float[] =
        if Math.Abs(a) < this.epsilon then
            // quadratic
            if Math.Abs(b) < this.epsilon then
                // linear case
                if Math.Abs(c) < this.epsilon then
                    // horizontal line
                    if Math.Abs(d) < this.epsilon then [| 0.0 |] else [||]
                else
                    [| -d / c |]
            else
                let b2: float = 2.0 * b
                let mutable dValue: float = c * c - 4.0 * b * d

                if Math.Abs(dValue) < this.epsilon then
                    [| -c / b2 |]
                elif dValue > 0.0 then
                    dValue <- Math.Sqrt(dValue)
                    let result: float[] = [| (-c + dValue) / b2; (-c - dValue) / b2 |]
                    Array.sortInPlace result
                    result
                else
                    [||]
        else
            this.solveCubicNormalized(b / a, c / a, d / a)

    override this.isEqualVec2(a: Vec2, b: Vec2) : bool =
        Math.Abs(a.[0] - b.[0]) < this.epsilon
        && Math.Abs(a.[1] - b.[1]) < this.epsilon

    override this.compareVec2(a: Vec2, b: Vec2) : int =
        // returns -1 if a is smaller, 1 if b is smaller, 0 if equal
        if Math.Abs(b.[0] - a.[0]) < this.epsilon then
            if Math.Abs(b.[1] - a.[1]) < this.epsilon then
                0
            elif a.[1] < b.[1] then
                -1
            else
                1
        elif a.[0] < b.[0] then
            -1
        else
            1
