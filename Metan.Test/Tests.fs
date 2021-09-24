module Tests

open System
open System.Collections.Generic
open Xunit
open Metan.Core

let b9 = Body 9
let d9 = Dmg 9
let damage dmg b =
    match b with
    | Body h -> Body (h - dmg)
    | _ -> b

module Projection =
    let createWith kind ps =
        ps |> List.map (fun p -> { Segment.pos = p; kind = kind })
        |> Projection

[<Fact>]
let ``Position add should return proper value`` () =
    Assert.Equal((0, 0), Position.add (0, 0) (0, 0))
    Assert.Equal((1, 0), Position.add (0, 0) (1, 0))
    Assert.Equal((0, 1), Position.add (0, 0) (0, 1))
    Assert.Equal((1, 1), Position.add (0, 0) (1, 1))
    Assert.Equal((-1, -1), Position.add (0, 0) (-1, -1))
    
[<Fact>]
let ``Position sub should return proper value`` () =
    Assert.Equal((0, 0), Position.sub (0, 0) (0, 0))
    Assert.Equal((-1, 0), Position.sub (0, 0) (1, 0))
    Assert.Equal((0, -1), Position.sub (0, 0) (0, 1))
    Assert.Equal((-1, -1), Position.sub (0, 0) (1, 1))
    Assert.Equal((1, 1), Position.sub (0, 0) (-1, -1))
    Assert.Equal((-1, -1), Position.sub (-2, -2) (-1, -1))
    
[<Fact>]
let ``Position move should return proper value`` () =
    Assert.Equal((0, 1), Position.move Up (0, 0))
    Assert.Equal((0, -1), Position.move Down (0, 0))
    Assert.Equal((-1, 0), Position.move Left (0, 0))
    Assert.Equal((1, 0), Position.move Right (0, 0))

[<Fact>]
let ``Reflection single should create reflection`` () =
    Assert.Equal(Reflection.createWith b9 [(10, 20)], Reflection.singleWith b9 (10, 20))

[<Fact>]
let ``Reflection create should create reflection and sort positions`` () =
    Assert.Equal(Reflection.createWith b9 [(0, 0); (1, 1); (2, 2)], Reflection.createWith b9 [(1, 1); (0, 0); (2, 2)])
    Assert.Equal(Reflection.createWith b9 [(1, 3); (2, 2); (3, 1)], Reflection.createWith b9 [(1, 3); (2, 2); (3, 1)])


[<Fact>]
let ``Reflection normalize should remove negative positions`` () =
    Assert.Equal(Reflection.createWith b9 [(0, 1); (1, 0); (1, 1)],
                 Reflection.normalize (Reflection.createWith b9 [(-1, -1); (0, -1); (0, -2)]))

[<Fact>]
let ``Reflection join should join and normalize reflections`` () =
    Assert.Equal(Reflection.createWith b9 [(0, 0); (1, 0); (2, 1)],
        Reflection.join
            (Reflection.createWith b9 [(-2, -1); (-1, -1)])
            (Reflection.createWith b9 [(0, 0) ]))
    
[<Fact>]
let ``Reflection topLeft should return top left position`` () =
    Assert.Equal((0, 0), Reflection.topLeft (Reflection.createWith b9 [(0, 0); (0, 1); (1, 0)]))
    Assert.Equal((0, 1), Reflection.topLeft (Reflection.createWith b9 [(0, 1); (1, 0); (1, 1)]))
    Assert.Equal((10, 0), Reflection.topLeft (Reflection.createWith b9 [(11, 1); (10, 0); (12, 1)]))
    Assert.Equal((0, 10), Reflection.topLeft (Reflection.createWith b9 [(1, 1); (0, 10); (2, 1)]))
    
[<Fact>]
let ``HitBox single should return one point hitBox`` () =
    Assert.Equal(HitBox ((2, 1), (2, 1)), HitBox.single (2, 1))
    
[<Fact>]
let ``HitBox bottomRight should return bottom right position`` () =
     Assert.Equal((2, 1), HitBox.bottomRight (HitBox((5, 6), (2, 1))))
  
[<Fact>]
let ``HitBox topLeft should return top left position`` () =
    Assert.Equal((5, 6), HitBox.topLeft (HitBox((5, 6), (2, 1))))
    
[<Fact>]
let ``HitBox intersect should return proper value`` () =
    Assert.True(HitBox.intersect (HitBox((0, 0), (0, 0))) (HitBox((0, 0), (0, 0))))
    Assert.True(HitBox.intersect (HitBox((0, 0), (1, 1))) (HitBox((0, 0), (0, 0))))
    Assert.True(HitBox.intersect (HitBox((0, 0), (1, 1))) (HitBox((1, 1), (1, 1))))
    Assert.True(HitBox.intersect (HitBox((0, 0), (2, 2))) (HitBox((1, 0), (3, 2))))
    Assert.True(HitBox.intersect (HitBox((1, 0), (3, 2))) (HitBox((0, 0), (2, 2))))
    Assert.True(HitBox.intersect (HitBox((1, 1), (2, 2))) (HitBox((0, 0), (2, 1))))
    
    Assert.False(HitBox.intersect (HitBox((0, 0), (0, 0))) (HitBox((1, 1), (1, 1))))
    Assert.False(HitBox.intersect (HitBox((0, 0), (0, 2))) (HitBox((2, 0), (2, 2))))
    
[<Fact>]
let ``HitBox move should return proper value`` () =
    Assert.Equal(HitBox((0, 1), (0, 1)), HitBox.move Up (HitBox((0, 0), (0, 0))))
    Assert.Equal(HitBox((0, -1), (0, -1)), HitBox.move Down (HitBox((0, 0), (0, 0))))
    Assert.Equal(HitBox((-1, 0), (-1, 0)), HitBox.move Left (HitBox((0, 0), (0, 0))))
    Assert.Equal(HitBox((1, 0), (1, 0)), HitBox.move Right (HitBox((0, 0), (0, 0))))
    
    Assert.Equal(HitBox((1, 2), (3, 4)), HitBox.move Up (HitBox((1, 1), (3, 3))))
    Assert.Equal(HitBox((1, 0), (3, 2)), HitBox.move Down (HitBox((1, 1), (3, 3))))
    Assert.Equal(HitBox((0, 1), (2, 3)), HitBox.move Left (HitBox((1, 1), (3, 3))))
    Assert.Equal(HitBox((2, 1), (4, 3)), HitBox.move Right (HitBox((1, 1), (3, 3))))
    
[<Fact>]
let ``HitBox outBounds should return proper value`` () =
    Assert.True(HitBox.outBounds (10, 10) (HitBox((-1, -1), (0, 0))))
    Assert.True(HitBox.outBounds (10, 10) (HitBox((0, 0), (11, 11))))
    Assert.True(HitBox.outBounds (10, 10) (HitBox((1, 1), (09, 11))))
    Assert.True(HitBox.outBounds (10, 10) (HitBox((1, 1), (11, 09))))
    
[<Fact>]
let ``HitBox reflect should return proper value`` () =
    Assert.Equal<IEnumerable<int * int>>([(0, 0)], HitBox.reflect (HitBox((0, 0), (0, 0))) (HitBox((0, 0), (0, 0))))
    Assert.Equal<IEnumerable<int * int>>([(0, 0)], HitBox.reflect (HitBox((30, 20), (30, 20))) (HitBox((30, 20), (30, 20))))
    Assert.Equal<IEnumerable<int * int>>([(2, 2)], HitBox.reflect (HitBox((0, 0), (2, 2))) (HitBox((2, 2), (2, 2))))
    Assert.Equal<IEnumerable<int * int>>([(1, 1); (1, 2); (2, 1); (2, 2)], HitBox.reflect (HitBox((0, 0), (1, 1))) (HitBox((1, 1), (2, 2))))
    
[<Fact>]
let ``HitBox join should join two hitBoxes`` () =
    Assert.Equal(HitBox((0, 0), (0, 0)), HitBox.join (HitBox((0, 0), (0, 0))) (HitBox((0, 0), (0, 0))))
    Assert.Equal(HitBox((0, 0), (2, 2)), HitBox.join (HitBox((0, 0), (0, 0))) (HitBox((2, 2), (2, 2))))
    
[<Fact>]
let ``HitBox expand should return correct value`` () =
    Assert.Equal(HitBox((1, 1), (3, 3)), HitBox.expand 1 (HitBox((2, 2), (2, 2))))
    
[<Fact>]
let ``Projection project should project reflection`` () =
    Assert.Equal(
        Projection.createWith b9 [(10, 20); (10, 21); (11, 20)],
        Projection.project (HitBox.single (10, 20)) (Reflection.createWith b9 [(0, 0); (0, 1); (1, 0)]))
    Assert.Equal(
        Projection.createWith b9 [(10, 20); (10, 21); (11, 20)],
        Projection.project (HitBox ((10, 20), (30, 40))) (Reflection.createWith b9 [(0, 0); (0, 1); (1, 0)]))
    
[<Fact>]
let ``Projection reflect should reflect reflection`` () =
    Assert.Equal(
        Reflection.createWith b9 [(0, 0); (0, 1); (1, 0)],
        Projection.reflect (HitBox.single (10, 20)) (Projection.createWith b9 [(10, 20); (10, 21); (11, 20)]))
    
[<Fact>]
let ``Projection apply matched should affect matched segments`` () =
    Assert.Equal(
        Reflection.create [
            { pos = (0, 0); kind = Body 8 }
            { pos = (0, 1); kind = Body 9 }
        ],
        Reflection.applyMatched
            (Reflection.createWith b9 [(0, 0); (0, 1)])
            [(0, 0); (0, 2)]
            (damage 1))

[<Fact>]
let ``Projection applyAll should affect all segments`` () =
    Assert.Equal(
        Reflection.create [
            { pos = (0, 0); kind = Body 8 }
            { pos = (0, 1); kind = Body 8 }
        ],
        Reflection.applyAll 
            (Reflection.createWith b9 [(0, 0); (0, 1)])
            (damage 1))