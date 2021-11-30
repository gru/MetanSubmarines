module Tests

open System
open Metan.Core
open Xunit

exception UnexpectedBulletCount
exception UnexpectedSegmentKind

let rnd = new Random()
let defaultGame = Game.empty (Size(50, 25))
let applyCommand cmd g =
    g |> Game.tick rnd [ cmd ]
let tick commands =
    Game.tick rnd commands
let withVehicle id pos health g =
    let v = {
        id = id
        hitBox = HitBox.single pos
        dmg = 1
        shape = Reflection.singleWith (Body(health)) (0, 0)
        color = ConsoleColor.Blue }
    { g with vehicles = v::g.vehicles }
let withVehicleAt id pos g =
    withVehicle id pos 9 g
let withCrate id pos bonus g =
    let c = {
        id = id
        hitBox = HitBox.single pos
        shape = Reflection.singleOfNothing (0, 0)
        bonus = bonus }
    { g with crates = c::g.crates }

[<Fact>]
let ``Should move vehicle`` () =
    let x, y = (10, 10)
    let vu =
         defaultGame 
         |> withVehicleAt 1 (x, y) 
         |> applyCommand (Move(1, Up))
         |> fun g -> g.vehicles |> List.head
    Assert.Equal((x, y + 1), HitBox.topLeft vu.hitBox)
    let vd =
         defaultGame 
         |> withVehicleAt 1 (x, y) 
         |> applyCommand (Move(1, Down))
         |> fun g -> g.vehicles |> List.head
    Assert.Equal((x, y - 1), HitBox.topLeft vd.hitBox)
    let gl =
         defaultGame 
         |> withVehicleAt 1 (x, y) 
         |> applyCommand (Move(1, Left))
         |> fun g -> g.vehicles |> List.head
    Assert.Equal((x - 1, y), HitBox.topLeft gl.hitBox)
    let gr =
         defaultGame 
         |> withVehicleAt 1 (x, y) 
         |> applyCommand (Move(1, Right))
         |> fun g -> g.vehicles |> List.head
    Assert.Equal((x + 1, y), HitBox.topLeft gr.hitBox)
    let gn =
         defaultGame 
         |> withVehicleAt 1 (x, y) 
         |> applyCommand (Move(2, Right))
         |> fun g -> g.vehicles |> List.head
    Assert.Equal((x, y), HitBox.topLeft gn.hitBox)

[<Fact>]
let ``Should not move vehicle in corners`` () =
    let tl = (0, 0)
    let v1 =
         defaultGame 
         |> withVehicleAt 1 tl
         |> applyCommand (Move(1, Left))
         |> applyCommand (Move(1, Down))
         |> fun g -> g.vehicles |> List.head
    Assert.Equal(tl, HitBox.topLeft v1.hitBox)
    let br = defaultGame.size
    let v2 =
         defaultGame 
         |> withVehicleAt 1 br
         |> applyCommand (Move(1, Right))
         |> applyCommand (Move(1, Up))
         |> fun g -> g.vehicles |> List.head
    Assert.Equal(br, HitBox.topLeft v2.hitBox)
    
[<Fact>]
let ``Should not move vehicle blocked by another vehicle`` () =
    let p1 = (10, 10)
    let p2 = (11, 10)
    let v1 =
        defaultGame
        |> withVehicleAt 2 p2
        |> withVehicleAt 1 p1 
        |> applyCommand (Move(1, Right))
        |> fun g -> g.vehicles |> List.head
    Assert.Equal(p1, HitBox.topLeft v1.hitBox)
    
[<Fact>]
let ``Should fire bullet`` () =
     let x, y = (10, 10)
     let g =
         defaultGame 
         |> withVehicleAt 1 (x, y)
         |> applyCommand (Fire(1, Up))
         |> applyCommand (Fire(1, Down))
         |> applyCommand (Fire(1, Left))
         |> applyCommand (Fire(1, Right))
     match g.bullets with
     | [ u; d; l; r ] ->
         Assert.Equal((x, y + 1), HitBox.topLeft u.hitBox)
         Assert.Equal((x, y - 1), HitBox.topLeft d.hitBox)
         Assert.Equal((x - 1, y), HitBox.topLeft l.hitBox)
         Assert.Equal((x + 1, y), HitBox.topLeft r.hitBox)
     | _ -> raise UnexpectedBulletCount
     
[<Fact>]
let ``Should not fire out bounds`` () =
    let tl = (0, 0)
    let bs1 =
         defaultGame 
         |> withVehicleAt 1 tl
         |> applyCommand (Fire(1, Left))
         |> applyCommand (Fire(1, Down))
         |> fun g -> g.bullets
    Assert.Empty(bs1)
    let bs2 =
         defaultGame 
         |> withVehicleAt 1 tl
         |> applyCommand (Fire(1, Right))
         |> applyCommand (Fire(1, Up))
         |> fun g -> g.bullets
    Assert.Equal(2, bs2 |> List.length)
    let br = defaultGame.size
    let bs3 =
         defaultGame 
         |> withVehicleAt 1 br
         |> applyCommand (Fire(1, Right))
         |> applyCommand (Fire(1, Up))
         |> fun g -> g.bullets
    Assert.Empty(bs3)
    let bs4 =
         defaultGame 
         |> withVehicleAt 1 br
         |> applyCommand (Fire(1, Left))
         |> applyCommand (Fire(1, Down))
         |> fun g -> g.bullets
    Assert.Equal(2, bs4 |> List.length)
    
[<Fact>]
let ``Should move bullets`` () =
    let x, y = (10, 10)
    let b =
        defaultGame 
        |> withVehicleAt 1 (x, y)
        |> applyCommand (Fire(1, Right))
        |> tick [ Tick; ]
        |> tick [ Tick; ]
        |> fun g -> g.bullets |> List.head
    Assert.Equal((x + 5, y), HitBox.topLeft b.hitBox)

[<Fact>]
let ``Should stop bullets out bounds`` () =
    let g1 =
        Game.empty (Size(3, 3))
        |> withVehicleAt 1 (1, 1)
        |> applyCommand (Fire(1, Up))
        |> applyCommand (Fire(1, Down))
        |> applyCommand (Fire(1, Left))
        |> applyCommand (Fire(1, Right))
    let g2 =
        g1 |> tick [ Tick; ]
    Assert.Equal(4, g1 |> fun g -> g.bullets |> List.length)
    Assert.Empty(g2.bullets)
    
[<Fact>]
let ``Should hit vehicle`` () =
    let s1 =
        defaultGame
        |> withVehicleAt 2 (10, 10)
        |> withVehicle 1 (12, 10) 2
        |> applyCommand (Fire(2, Right))
        |> tick [ Tick ]
        |> fun g -> g.vehicles |> List.head
        |> fun v -> v.shape |> Reflection.head
    match s1.kind with
    | Body health -> Assert.Equal(1, health)
    | _ -> raise UnexpectedSegmentKind
        
[<Fact>]
let ``Should apply health bonus when vehicle took crate`` () =
    let vp = (10, 10)
    let cp = (11, 10)
    let s1 =
        defaultGame
        |> withCrate 1 cp (HealthBonus(1))
        |> withVehicle 1 vp 1
        |> applyCommand (Move(1, Right))
        |> fun g -> g.vehicles |> List.head
        |> fun v -> v.shape |> Reflection.head
    match s1.kind with
    | Body health -> Assert.Equal(2, health)
    | _ -> raise UnexpectedSegmentKind
    
[<Fact>]
let ``Should limit health bonus when vehicle took crate`` () =
    let vp = (10, 10)
    let cp = (11, 10)
    let s1 =
        defaultGame
        |> withCrate 1 cp (HealthBonus(2))
        |> withVehicle 1 vp 9
        |> applyCommand (Move(1, Right))
        |> fun g -> g.vehicles |> List.head
        |> fun v -> v.shape |> Reflection.head
    match s1.kind with
    | Body health -> Assert.Equal(9, health)
    | _ -> raise UnexpectedSegmentKind
    
[<Fact>]
let ``Should damage vehicle when damage bonus have been taken`` () =
    let vp = (10, 10)
    let cp = (11, 10)
    let s1 =
        defaultGame
        |> withCrate 1 cp (DamageBonus(1))
        |> withVehicle 1 vp 2
        |> applyCommand (Move(1, Right))
        |> fun g -> g.vehicles |> List.head
        |> fun v -> v.shape |> Reflection.head
    match s1.kind with
    | Body health -> Assert.Equal(1, health)
    | _ -> raise UnexpectedSegmentKind
    
[<Fact>]
let ``Should kill vehicle when damage bonus greater than health`` () =
    let vp = (10, 10)
    let cp = (11, 10)
    let vs =
        defaultGame
        |> withCrate 1 cp (DamageBonus(1))
        |> withVehicle 1 vp 1
        |> applyCommand (Move(1, Right))
        |> fun g -> g.vehicles
    Assert.Empty(vs)
   
open ChangeDetection
    
exception ChangeDetectionFailed
    
[<Fact>]
let ``Should detect vehicle added`` () =
    let previous = defaultGame
    let current = previous |> withVehicleAt 1 (10, 10)
    let changes = delta previous current
    match changes with
    | [ VehicleAdded({ Vehicle.id = vid }) ] -> Assert.Equal(1, vid)
    | _ -> raise ChangeDetectionFailed
    
[<Fact>]
let ``Should detect crate added`` () =
    let previous =
        defaultGame
        |> withCrate 1 (10, 20) (HealthBonus(1))
    let current =
        previous
        |> withCrate 2 (20, 20) (HealthBonus(2))
    let changes = delta previous current
    match changes with
    | [ CrateAdded(c2) ] -> Assert.Equal(2, c2.id)
    | _ -> raise ChangeDetectionFailed
    
[<Fact>]
let ``Should detect crate removed`` () =
    let previous =
        defaultGame
        |> withCrate 1 (10, 20) (HealthBonus(1))
    let current = defaultGame
    let changes = delta previous current
    match changes with
    | [ CrateRemoved(id) ] -> Assert.Equal(1, id)
    | _ -> raise ChangeDetectionFailed
    
[<Fact>]
let ``Should detect crate removed from start`` () =
    0
    
[<Fact>]
let ``Should detect vehicle removed from end`` () =
    let previous = defaultGame |> withVehicleAt 1 (10, 10)
    let current = defaultGame
    let changes = delta previous current
    match changes with
    | [ VehicleRemoved vid ] -> Assert.Equal(1, vid)
    | _ -> raise ChangeDetectionFailed
    
[<Fact>]  
let ``Should detect vehicle removed`` () =
    let previous =
        defaultGame
        |> withVehicleAt 1 (10, 10)
        |> withVehicleAt 2 (20, 20)
        |> withVehicleAt 3 (30, 30)
    let current =
        defaultGame
        |> withVehicleAt 1 (10, 10)
        |> withVehicleAt 3 (30, 30)
    let changes = delta previous current
    match changes with
    | [ VehicleRemoved vid ] -> Assert.Equal(2, vid)
    | _ -> raise ChangeDetectionFailed
    
[<Fact>]
let ``Should detect vehicle moved`` () =
     let pp = (10, 10)
     let cp = (20, 20)
     let previous =
        defaultGame
        |> withVehicleAt 1 pp
     let current =
        defaultGame
        |> withVehicleAt 1 cp
     let changes = delta previous current
     match changes with
     | [ VehicleMoved(vid, prevHitBox, curHitBox) ] ->
         Assert.Equal(01, vid)
         Assert.Equal(pp, HitBox.topLeft prevHitBox)
         Assert.Equal(cp, HitBox.topLeft curHitBox)
     | _ -> raise ChangeDetectionFailed
     
[<Fact>]
let ``Should detect vehicle health changed`` () =
    let previous =
        defaultGame
        |> withVehicle 1 (10, 10) 9
        |> withVehicle 2 (20, 20) 1
    let current =
        defaultGame
        |> withVehicle 1 (10, 10) 8
        |> withVehicle 2 (20, 20) 3
    let changes = delta previous current
    match changes with
     | [ VehicleDamaged(id1, health1); VehicleHealed(id2, health2) ] ->
         Assert.Equal(id1, 1)
         Assert.Equal(health1, 8)
         Assert.Equal(id2, 2)
         Assert.Equal(health2, 3)
     | _ -> raise ChangeDetectionFailed
     
[<Fact>]
let ``Should detect bullet added`` () =
    let previous =
        defaultGame
        |> withVehicleAt 1 (10, 10)
    let current =
        previous
        |> applyCommand (Fire(1, Up))
    let changes = delta previous current
    match changes with
    | [ BulletAdded b ] -> Assert.Equal(Up, b.dir)
    | _ -> raise ChangeDetectionFailed
    
[<Fact>]
let ``Should detect bullet moved`` () =
    let previous =
        defaultGame
        |> withVehicleAt 1 (10, 10)
        |> applyCommand (Fire(1, Up))
    let current = tick [ Tick ] previous
    let changes = delta previous current
    match changes with
    | [ BulletMoved(id, prevHitBox, curHitBox) ] ->
        Assert.True(id > 0)
        Assert.Equal(HitBox.topLeft prevHitBox, (10, 11))
        Assert.Equal(HitBox.topLeft curHitBox, (10, 13))
    | _ -> raise ChangeDetectionFailed    