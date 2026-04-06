namespace PolyBool

open System

//
// polybool - Boolean operations on polygons (union, intersection, etc)
// by Sean Connelly (@velipso), https://sean.fun
// Project Home: https://github.com/velipso/polybool
// SPDX-License-Identifier: 0BSD
//

type Vec2 = float[]
type BBox = Vec2 * Vec2

type Transform =
    {
        a: float
        b: float
        c: float
        d: float
        e: float
        f: float
    }

module TransformFunctions =
    let identity: Transform =
        {
            a = 1.0
            b = 0.0
            c = 0.0
            d = 1.0
            e = 0.0
            f = 0.0
        }

    let apply (transform: Transform) (x: float) (y: float) : Vec2 =
        [|
            transform.a * x + transform.c * y + transform.e
            transform.b * x + transform.d * y + transform.f
        |]

[<AbstractClass>]
type Geometry() =
    abstract snap0: float -> float
    abstract snap01: float -> float
    abstract isCollinear: Vec2 * Vec2 * Vec2 -> bool
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
        let dx1: float = p1.[0] - p2.[0]
        let dy1: float = p1.[1] - p2.[1]
        let dx2: float = p2.[0] - p3.[0]
        let dy2: float = p2.[1] - p3.[1]
        Math.Abs(dx1 * dy2 - dx2 * dy1) < this.epsilon

    override this.isEqualVec2(a: Vec2, b: Vec2) : bool =
        Math.Abs(a.[0] - b.[0]) < this.epsilon
        && Math.Abs(a.[1] - b.[1]) < this.epsilon

    override this.compareVec2(a: Vec2, b: Vec2) : int =
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