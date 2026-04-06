namespace PolyBool

open System

//
// polybool - Boolean operations on polygons (union, intersection, etc)
// by Sean Connelly (@velipso), https://sean.fun
// Project Home: https://github.com/velipso/polybool
// SPDX-License-Identifier: 0BSD
//

module Geometry =

    let mutable epsilon: float =  1e-9

    let mutable resultX: float = 0.0
    let mutable resultY: float = 0.0

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

    let inline isCollinear(p1x: float, p1y: float, p2x: float, p2y: float, p3x: float, p3y: float) : bool =
        let dx1: float = p1x - p2x
        let dy1: float = p1y - p2y
        let dx2: float = p2x - p3x
        let dy2: float = p2y - p3y
        Math.Abs(dx1 * dy2 - dx2 * dy1) < epsilon

    let inline isEqualVec2(ax: float, ay: float, bx: float, by: float) : bool =
        Math.Abs(ax - bx) < epsilon
        && Math.Abs(ay - by) < epsilon

    let inline compareVec2(ax: float, ay: float, bx: float, by: float) : int =
        if Math.Abs(bx - ax) < epsilon then
            if Math.Abs(by - ay) < epsilon then
                0
            elif ay < by then
                -1
            else
                1
        elif ax < bx then
            -1
        else
            1
