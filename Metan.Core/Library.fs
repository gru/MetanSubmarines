namespace Metan.Core

open System

[<AutoOpen>]
module Core =
    type Size = int * int
    type Position = int * int
    type Direction = Up | Down | Left | Right
    type Damage = int
    type Health = int
    type Time = uint
    type SegmentKind =
        | Body of Health
        | Dmg of Damage
    type Segment =
        {
            pos: Position
            kind: SegmentKind
        }
    type Reflection = Reflection of Segment list
    type Projection = Projection of Segment list
    type HitBox = HitBox of Position * Position
    type Bullet =
        {
            hitBox:HitBox
            dir:Direction
            dmg:Damage
        }
    type VehicleId = int
    type Vehicle =
        {
            id:VehicleId
            hitBox:HitBox
            shape:Reflection
            dmg:Damage
            color: ConsoleColor
        }
    type Movable<'T> =
        | Movable of 'T
        | Blocked of 'T
    type Bonus =
        | HealthBonus of Health
        | DamageBonus of Damage
        | ShapeBonus
        | RandomBonus
    type Crate =
        {
            hitBox:HitBox
            bonus:Bonus
        }
    type GameCommand =
        | Move of VehicleId * Direction
        | Fire of VehicleId * Direction
        | Tick
    type Game =
        {
          bullets:Bullet list
          vehicles:Vehicle list
          crates:Crate list
          size: Size
          time: Time
        }
    type UserId = int
    type ConnectionId = string
    type UserEvent =
        | UserJoined of UserId
        | UserLeft
    type AreaEvent =
        | UserEvent of ConnectionId * UserEvent
        | State of Game * Game
    type UserCommand =
        | Join
        | Leave of UserId
    type AreaCommand =
        | JoinBot
        | UserCommand of ConnectionId * UserCommand
        | AreaCommand of GameCommand
    type Area =
        {
            users: UserId list
            commands: GameCommand list
        }
        
module Option =
    let ret v = Some v

module Seq =
    open System.Linq
    let intersect (xs:'a seq) (ys: 'a seq) =
        xs.Intersect(ys)

module List =
    let any f vs =
        vs |> List.tryFind f |> Option.isSome

module Position =
    let add (x1, y1) (x2, y2) =
        (x1 + x2, y1 + y2)
            
    let sub (x1, y1) (x2, y2) =
       (x1 - x2, y1 - y2)
       
    let subRev p1 p2 =
       sub p2 p1
         
    let move dir (x, y) =
        match dir with
        | Up -> (x, y + 1)
        | Down -> (x, y - 1)
        | Left -> (x - 1, y)
        | Right -> (x + 1, y)

    let getRandom (rnd:Random) ((w, h):Size) =
        (rnd.Next(0, w + 1), rnd.Next(0, h + 1))

module Reflection =
    exception EmptyReflection
    
    let getPos { Segment.pos = pos } = pos
    let getX { Segment.pos = (x, _) } = x
    let getY { Segment.pos = (_, y) } = y
    
    let create ps =
        if ps = []
        then raise EmptyReflection
        else Reflection (ps |> List.sort)
        
    let createWith kind ps =
        ps |> List.map (fun p -> { Segment.pos = p; kind = kind })
        |> create
            
    let single p =
        Reflection [p]
    
    let singleWith kind p =
        { Segment.pos = p; kind = kind }
        |> single
    
    let topLeft = function
        | Reflection sl ->
            sl |> List.head
            |> getPos

    let mapPos f = function
        | Reflection sl ->
            Reflection (sl |> List.map (fun s -> { s with pos = f s.pos }))
    
    let tryFind f = function
        | Reflection sl ->
            sl |> List.tryFind f
    
    let intersect ps = function
        | Reflection sl ->
            not <| (Seq.intersect (sl |> List.map getPos) ps |> Seq.isEmpty) 
    
    let normalize = function
        | Reflection sl as ref ->
             let minx = sl |> List.map getX |> List.min
             let miny = sl |> List.map getY |> List.min
             if minx < 0 || miny < 0
             then ref |> mapPos (Position.subRev (min 0 minx, min 0 miny))
             else ref

    let join (Reflection sl1) (Reflection sl2) =
        normalize (create (sl1 @ sl2))
       
    let applyAll ref f =
        match ref with
        | Reflection sl ->
            Reflection (sl |> List.map (fun s -> { s with kind = f s.kind }))
    
    let applyMatched ref1 ps f =
        match ref1 with
        | Reflection sl1 ->
            let sl =
                [ for s1 in sl1 do
                    match ps |> List.tryFind (fun s2 -> s2 = s1.pos) with
                    | Some _ -> { s1 with kind = f s1.kind } 
                    | None -> s1 ]
            create sl
       
    let filterAll ref f =
        match ref with
        | Reflection sl ->
            Reflection (sl |> List.filter (fun s -> f s.kind))
       
module HitBox =
    let single p = HitBox (p, p)
    let bottomRight (HitBox(_, rb)) = rb
    let topLeft (HitBox(lt, _)) = lt
    
    let intersect (HitBox((l1, t1), (r1, b1))) (HitBox((l2, t2), (r2, b2))) =
        not (l2 > r1 || r2 < l1 || t2 > b1 || b2 < t1)
        
    let move dir (HitBox((l, t), (r, b))) =
        match dir with
        | Up -> HitBox ((l, t + 1), (r, b + 1))
        | Down -> HitBox ((l, t - 1), (r, b - 1))
        | Left -> HitBox ((l - 1, t), (r - 1, b))
        | Right -> HitBox ((l + 1, t), (r + 1, b))
        
    let outBounds (w, h) (HitBox((l, t), (r, b))) =
        t < 0 || b > h || l < 0 || r > w
        
    let reflect hb1 (HitBox((l, t), (r, b))) =
        let tl1 = topLeft hb1
        [ for x = l to r do
              for y = t to b do
                 Position.sub (x, y) tl1 ]
        
    let join hb1 hb2 =
        let tl1, tl2 = topLeft hb1, topLeft hb2
        let tl = (min (fst tl1) (fst tl2), min (snd tl1) (snd tl2))
        let br1, br2 = bottomRight hb1, bottomRight hb2
        let br = (max (fst br1) (fst br2), max (snd br1) (snd br2))
        HitBox (tl, br)
        
    let expand l (HitBox(tl, br)) =
        HitBox (Position.sub tl (l, l), Position.add br (l, l))

    let crop (w, h) (HitBox((l1, t1), (r1, b1))) =
         HitBox ((max 0 l1, max 0 t1), (min w r1, min h b1))

module Projection =
    exception EmptyProjection

    let private mapPos f = function
        | Reflection sl ->
            Projection (sl |> List.map (fun s -> { s with pos = f s.pos }))
    
    let private mapPosBack f = function
        | Projection sl ->
            Reflection.create (sl |> List.map (fun s -> { s with pos = f s.pos }))
    
    let project hb ref =
        let tl = HitBox.topLeft hb
        ref |> mapPos (Position.add tl)

    let reflect hb ref =
        let tl = HitBox.topLeft hb
        ref |> mapPosBack (Position.subRev tl)
                
    let toHitBox proj =
        match proj with
        | Projection [] ->
            raise EmptyProjection
        | Projection [ s ] ->
            s
            |> Reflection.getPos
            |> HitBox.single
        | Projection sl ->
            let xs = sl |> List.map Reflection.getX   
            let ys = sl |> List.map Reflection.getY
            HitBox ((List.min xs, List.min ys), (List.max xs, List.max ys))

module SegmentKind =
    let damage dmg = function
        | Body health -> Body (max 0 (health - dmg))
        | b -> b
    let heal health = function
        | Body current -> Body (min 9 (current + health))
        | b -> b
   
module Crate =
    open SegmentKind
    
    let rec apply (rnd:Random) (dir:Direction) (c:Crate) (v:Vehicle) =
        match c.bonus with
        | HealthBonus health ->
            let ps = HitBox.reflect v.hitBox c.hitBox
            Some { v with shape = Reflection.applyMatched v.shape ps (heal health) }
        | DamageBonus dmg ->
            let shape = HitBox.reflect v.hitBox c.hitBox
            Some { v with shape = Reflection.applyMatched v.shape shape (damage dmg) }
        | ShapeBonus ->
            let ps =
                HitBox.reflect v.hitBox c.hitBox
                |> List.map (Position.move dir)
            let ref = Reflection.createWith (Body 9) ps
            let shape = Reflection.join v.shape ref
            let hb = Projection.toHitBox (Projection.project v.hitBox shape)
            Some { v with hitBox = hb; shape = shape }
        | RandomBonus ->
            if rnd.NextDouble() > 0.5
            then apply rnd dir { c with bonus = DamageBonus (rnd.Next(1, 5)) } v
            else apply rnd dir { c with bonus = HealthBonus (rnd.Next(1, 5)) } v
          
    let disappearOverVehicles (vs:Vehicle list) (c:Crate) =
        match vs |> List.tryFind (fun v -> HitBox.intersect v.hitBox c.hitBox) with
        | Some v ->
            let ps = HitBox.reflect v.hitBox c.hitBox
            if v.shape |> Reflection.intersect ps
            then None
            else Some c
        | None -> Some c
  
    let spawn (rnd:Random) size cs =
        let getCratePosition () =
            HitBox.single (Position.getRandom rnd size)
        if cs |> List.length < 5 then
            let pr = rnd.Next(0, 100)
            if pr >= 0 && pr < 1
            then Some { hitBox = getCratePosition(); bonus = HealthBonus(rnd.Next(1, 5)) }
            elif pr >= 1 && pr < 2
            then Some { hitBox = getCratePosition(); bonus = DamageBonus(1) }
            elif pr >= 2 && pr < 3
            then Some { hitBox = getCratePosition(); bonus = ShapeBonus }
            elif pr >= 4 && pr < 4
            then Some { hitBox = getCratePosition(); bonus = RandomBonus }
            else None
        else None
          
module Bullet =
    let moveBullet (b:Bullet) =
        Some { b with hitBox = HitBox.move b.dir b.hitBox }

    let stopOverBoundaries (s:Size) (b:Bullet) =
        let hb = HitBox.move b.dir b.hitBox
        if HitBox.outBounds s hb
        then None
        else Some b

    let rec stopOverVehicles (vs: Vehicle list) (b:Bullet) =
        match vs |> List.tryFind (fun v -> HitBox.intersect v.hitBox b.hitBox) with
        | Some v ->
            let ps = HitBox.reflect v.hitBox b.hitBox
            if v.shape |> Reflection.intersect ps
            then None
            else Some b
        | None -> Some b
    
    let rec moveBullets bullets pipe =
        let rec moveInternal bs acc =
            match bs with
            | b::rest ->
                match pipe b with
                | Some bx -> moveInternal rest (bx::acc)
                | None -> moveInternal rest acc
            | [] -> acc
        moveInternal bullets []    

module Vehicle =
    open SegmentKind
    
    let moveVehicle dir (m:Vehicle) =
        Movable { m with hitBox = HitBox.move dir m.hitBox }

    let stopOverBoundaries size dir (m:Vehicle) =
        if HitBox.outBounds size (HitBox.move dir m.hitBox)
        then Blocked m
        else Movable m
    
    let stopOverVehicles (vs:Vehicle list) dir (m:Vehicle) =
        let hb = HitBox.move dir m.hitBox
        if vs
           |> List.filter (fun v -> not (v.id = m.id))
           |> List.any (fun v -> HitBox.intersect v.hitBox hb) 
        then Blocked m
        else Movable m

    let moveById id (m:Vehicle) =
        if m.id = id
        then Movable m
        else Blocked m

    let fire dir (m:Vehicle) =
        let pos =
            match dir with
            | Left | Up -> Position.move dir (HitBox.topLeft m.hitBox)
            | Right | Down -> Position.move dir (HitBox.bottomRight m.hitBox)
        Some [{ Bullet.hitBox = HitBox.single pos; dmg = m.dmg; dir = dir }]

    let fireById id (m:Vehicle) =
        if m.id = id
        then Some m
        else None

    let fireOverBoundaries size dir (m:Vehicle) =
        let hb = HitBox.move dir m.hitBox
        if HitBox.outBounds size hb
        then None
        else Some m 

    let hitByBullet (bs:Bullet list) (m:Vehicle) =
        match bs |> List.tryFind (fun b -> HitBox.intersect b.hitBox m.hitBox) with
        | Some b ->
            let ps = HitBox.reflect m.hitBox b.hitBox
            Some { m with shape = Reflection.applyMatched m.shape ps (damage b.dmg) }
        | None -> Some m
        
    let takeCrate (rnd:Random) (dir:Direction) (cs:Crate list) (m:Vehicle) =
        match cs |> List.tryFind (fun c -> HitBox.intersect c.hitBox m.hitBox) with
        | Some c ->
            let ps = HitBox.reflect m.hitBox c.hitBox
            if Reflection.intersect ps m.shape 
            then Crate.apply rnd dir c m
            else Some m
        | None -> Some m
        
    let getColor (rnd:Random) =
        let ns = Enum.GetNames(typeof<ConsoleColor>)
        Enum.Parse<ConsoleColor>(ns.[rnd.Next(0, ns.Length - 1)]) 

module Movable =
    let ret v = Movable v
    let bind f m =
        match m with
        | Movable v -> f v
        | Blocked v -> Blocked v
    let unwind m =
        match m with
        | Movable v -> v
        | Blocked v -> v
        
module Game =
    let rec tick (rnd:Random) (game:Game) (cs:GameCommand list) =
        match cs with
        | Move(vid, dir)::rest ->
            let movePipe =
                Movable.ret
                >> Movable.bind (Vehicle.moveById vid)
                >> Movable.bind (Vehicle.stopOverBoundaries game.size dir)
                >> Movable.bind (Vehicle.stopOverVehicles game.vehicles dir)
                >> Movable.bind (Vehicle.moveVehicle dir)
            let vs = game.vehicles
                     |> List.map movePipe
                     |> List.map Movable.unwind
            let hitPipe =
                Option.ret
                >> Option.bind (Vehicle.takeCrate rnd dir game.crates)
            let vs = vs
                     |> List.map hitPipe
                     |> List.choose id
            tick rnd { game with vehicles = vs } rest
        | Fire(vid, dir)::rest ->
            let firePipe =
                Vehicle.fireById vid
                >> Option.bind (Vehicle.fireOverBoundaries game.size dir)
                >> Option.bind (Vehicle.fire dir)
            let bs = game.vehicles
                     |> List.map firePipe
                     |> List.choose id
                     |> List.concat
            tick rnd { game with bullets = game.bullets @ bs } rest
        | Tick::rest ->
            let movePipe =
                Option.ret
                >> Option.bind (Bullet.stopOverBoundaries game.size)
                >> Option.bind (Bullet.stopOverVehicles game.vehicles)
                >> Option.bind (Bullet.moveBullet)
            let bs = game.bullets
                     |> List.map movePipe
                     |> List.choose id
            let hitPipe =
                Option.ret
                >> Option.bind (Vehicle.hitByBullet bs)
            let vs = game.vehicles
                     |> List.map hitPipe
                     |> List.choose id
            let cratePipe =
                Option.ret
                >> Option.bind (Crate.disappearOverVehicles vs)
            let cx = Crate.spawn rnd game.size game.crates
            let cs = game.crates
                     |> List.map cratePipe
                     |> List.append [cx]
                     |> List.choose id
            tick rnd { game with bullets = bs; vehicles = vs; crates = cs; time = game.time + 1u } rest
        | [] -> game
        
    let remVehicle id vs =
        vs |> List.filter (fun v -> not (v.id = id))
        
    let addVehicle id hb cr vs =
        let vx = { id = id; dmg = 1; hitBox = hb; shape = Reflection.createWith (Body 9) [(0, 0)]; color = cr }
        vx::vs
        
    let empty size =
        { bullets = []; vehicles = []; crates = []; size = size; time = 0u }
       
module Area =
    let addUser us =
        if not (us |> List.isEmpty)
        then us |> List.max |> (+) 1
        else 1

    let remUser u us =
        us |> List.filter (fun ux -> not (ux = u))
        
    let empty = { users = []; commands = [] }