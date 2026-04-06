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

module Geometry =

    let mutable epsilon: float =  1e-9

    let inline snap0(v: float) : float =
        if Math.Abs(v) < epsilon then
            0.0
        else
            v

    let inline snap01(v: float) : float =
        if Math.Abs(v) < epsilon then
            0.0
        elif Math.Abs(1.0 - v) < epsilon then
            1.0
        else
            v

    let inline isCollinear(p1: Vec2, p2: Vec2, p3: Vec2) : bool =
        let dx1: float = p1.[0] - p2.[0]
        let dy1: float = p1.[1] - p2.[1]
        let dx2: float = p2.[0] - p3.[0]
        let dy2: float = p2.[1] - p3.[1]
        Math.Abs(dx1 * dy2 - dx2 * dy1) < epsilon

    let inline isEqualVec2(a: Vec2, b: Vec2) : bool =
        Math.Abs(a.[0] - b.[0]) < epsilon
        && Math.Abs(a.[1] - b.[1]) < epsilon

    let inline compareVec2(a: Vec2, b: Vec2) : int =
        if Math.Abs(b.[0] - a.[0]) < epsilon then
            if Math.Abs(b.[1] - a.[1]) < epsilon then
                0
            elif a.[1] < b.[1] then
                -1
            else
                1
        elif a.[0] < b.[0] then
            -1
        else
            1

