namespace Metan.Core

open System
open System.Threading

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
        | Nothing
    type Segment =
        {
            pos: Position
            kind: SegmentKind
        }
    type Reflection = Reflection of Segment list
    type Projection = Projection of Segment list
    type HitBox = HitBox of Position * Position
    type BulletId = int
    type Bullet =
        {
            id: BulletId
            hitBox:HitBox
            shape: Reflection
            dir:Direction
            dmg:Damage
            spd:int
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
    type CrateId = int
    type Crate =
        {
            id: CrateId
            hitBox:HitBox
            shape:Reflection
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
            connections: Map<ConnectionId, UserId>
        }
        
    let nextId =
        let current = ref 0
        fun () -> Interlocked.Increment(current)
        
module Option =
    let ret v = Some v

module Seq =
    open System.Linq
    let intersect (xs:'a seq) (ys: 'a seq) =
        xs.Intersect(ys)

module List =
    let any f vs =
        vs |> List.tryFind f |> Option.isSome

module Direction =
    let reverse = function
        | Up -> Down
        | Down -> Up
        | Left -> Right
        | Right -> Left

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
        else Reflection (ps |> List.sortBy getPos)
        
    let createWith kind ps =
        ps |> List.map (fun p -> { Segment.pos = p; kind = kind })
        |> create
            
    let single p =
        Reflection [p]
    
    let singleWith kind p =
        { Segment.pos = p; kind = kind }
        |> single
    
    let singleOfNothing p =
        { Segment.pos = p; kind = Nothing }
        |> single

    let head = function
        | Reflection sl -> sl |> List.head
    
    let len = function
        | Reflection sl -> sl |> List.length
    
    let isEmpty = function
        | Reflection [] -> true
        | _ -> false
    
    let applyAll ref f =
        match ref with
        | Reflection sl ->
            Reflection (sl |> List.map (fun s -> { s with kind = f s.kind }))
    
    let applyMatched f ref1 ref2 =
        match ref1, ref2 with
        | Reflection sl1, Reflection sl2 ->
            let sl =
                [ for s1 in sl1 do
                    match sl2 |> Seq.tryFind (fun s2 -> s2.pos = s1.pos) with
                    | Some _ -> { s1 with kind = f s1.kind } 
                    | None -> s1 ]
            create sl
       
    let filterAll f ref =
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
        
    let private outBounds (w, h) (HitBox((l, t), (r, b))) =
        t < 0 || b > h || l < 0 || r > w
        
    let tryMove size dir hb =
        not (hb |> move dir |> outBounds size)
        
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
    
    let topLeft = function
        | Projection sl ->
            sl |> List.head
            |> Reflection.getPos
            
    let project hb ref =
        let tl = HitBox.topLeft hb
        ref |> mapPos (Position.add tl)

    let reflect hb proj =
        let tl = HitBox.topLeft hb
        proj |> mapPosBack (Position.subRev tl)
             
    let contains proj1 proj2 =
        match proj1, proj2 with
        | _, Projection [] ->
            false
        | Projection sl, Projection [ s ] ->
            sl |> List.map Reflection.getPos |> List.contains s.pos
        | Projection sl1, Projection sl2 ->
            let p1 = sl1 |> List.map Reflection.getPos
            let p2 = sl2 |> List.map Reflection.getPos
            not (Seq.isEmpty <| Seq.intersect p1 p2)
     
    let join proj1 proj2 =
        match proj1, proj2 with
        | Projection sl1, Projection sl2 ->
            Projection (sl1 @ sl2)
                
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
    let dead = function
        | Body current -> current > 0
        | _ -> false
    
module Shape =
    exception InvalidShape;
    
    let grow ref =
        let body pos =
            { pos = pos; kind = Body 1 }
        let push dir s =
            { s with Segment.pos = Position.move dir s.pos; }
        match ref with
        | Reflection [ ]  -> 
            raise InvalidShape 
        | Reflection [ s ] -> 
            Reflection.create [ s; body (1, 0) ] 
        | Reflection [ l; r ] -> 
            Reflection.create [ l; r; body (2, 0) ]
        | Reflection [ l; c; r ] -> 
            Reflection.create [ push Up l; push Up c; push Up r; body (1, 0) ]
        | Reflection [ l; t; c; r ] ->
            Reflection.create [ l; t; c; r; body (3, 1) ]
        | r -> r
    
    let shrink ref =
        let set s pos =
            { s with pos = pos; }
        match ref with
        | Reflection [ s ]  ->  
            Reflection.create [ set s (0, 0) ]
        | Reflection [ l; r ] ->
            Reflection.create [ set l (0, 0); set r (1, 0) ]
        | Reflection [ l; c; r ] ->
            Reflection.create [ set l (0, 0); set c (1, 0); set r (2, 0) ]
        | Reflection [ l; t; c; r ] ->
            Reflection.create [ set l (0, 1); set t (1, 0); set c (1, 1); set r (2, 1) ]
        | r -> r 
    
module Crate =
    open SegmentKind
    
    let create p bonus =
        {
            id = nextId ()
            hitBox = HitBox.single p
            shape = Reflection.singleOfNothing (0, 0)
            bonus = bonus
        }
        
    let rec apply (rnd:Random) (s:Size) (dir:Direction) (c:Crate) (v:Vehicle) =
        match c.bonus with
        | HealthBonus health ->
            let vrx = Projection.project c.hitBox c.shape
                      |> Projection.reflect v.hitBox
                      |> Reflection.applyMatched (heal health) v.shape
            let pr = Projection.project v.hitBox vrx
            let hb = Projection.toHitBox pr
            Some { v with hitBox = hb; shape = vrx }
        | DamageBonus dmg ->
            let vrx = Projection.project c.hitBox c.shape
                      |> Projection.reflect v.hitBox
                      |> Reflection.applyMatched (damage dmg) v.shape
                      |> Reflection.filterAll dead
                      |> Shape.shrink
            let pr = Projection.project v.hitBox vrx
            let hb = Projection.toHitBox pr
            Some { v with hitBox = hb; shape = vrx }
        | ShapeBonus ->
            let sp = Shape.grow v.shape
            let pr = Projection.project v.hitBox sp
            let hb = Projection.toHitBox pr
            Some { v with hitBox = hb; shape = sp }
        | RandomBonus ->
            if rnd.NextDouble() > 0.5
            then apply rnd s dir { c with bonus = DamageBonus (rnd.Next(1, 5)) } v
            else apply rnd s dir { c with bonus = HealthBonus (rnd.Next(1, 5)) } v
          
    let disappearOverVehicles (vs:Vehicle list) (c:Crate) =
        match vs |> List.tryFind (fun v -> HitBox.intersect v.hitBox c.hitBox) with
        | Some v ->
            if Projection.project v.hitBox v.shape
               |> Projection.contains
               <| Projection.project c.hitBox c.shape
            then None
            else Some c
        | None -> Some c
  
    let spawn (rnd:Random) size cs =
        let getCratePosition () =
            Position.getRandom rnd size
        if cs |> List.length < 5 then
            let pr = rnd.Next(0, 100)
            if pr >= 0 && pr < 1
            then Some (create (getCratePosition()) (HealthBonus(rnd.Next(1, 5))))
            elif pr >= 1 && pr < 2
            then Some (create (getCratePosition()) (DamageBonus(1)))
            elif pr >= 2 && pr < 3
            then Some (create (getCratePosition()) ShapeBonus)
            elif pr >= 4 && pr < 5
            then Some (create (getCratePosition()) RandomBonus)
            else None
        else None
          
module Bullet =
    let create dir pos dmg =
        {
          Bullet.id = nextId ()
          hitBox = HitBox.single pos
          shape = Reflection.singleOfNothing (0, 0)
          dmg = dmg
          dir = dir
          spd = 2
        }
        
    let moveBullet (b:Bullet) =
        Some { b with hitBox = HitBox.move b.dir b.hitBox }

    let stopOverBoundaries (s:Size) (b:Bullet) =
        if HitBox.tryMove s b.dir b.hitBox
        then Some b
        else None

    let rec stopOverVehicles (vs: Vehicle list) (b:Bullet) =
        match vs |> List.tryFind (fun v -> HitBox.intersect v.hitBox b.hitBox) with
        | Some v ->
            if Projection.project v.hitBox v.shape
               |> Projection.contains
               <| Projection.project b.hitBox b.shape
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
        if HitBox.tryMove size dir m.hitBox
        then Movable m
        else Blocked m
    
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
            | Left | Down -> Position.move dir (HitBox.topLeft m.hitBox)
            | Right | Up -> Position.move dir (HitBox.bottomRight m.hitBox)
        Some [ Bullet.create dir pos m.dmg ]

    let fireById id (m:Vehicle) =
        if m.id = id
        then Some m
        else None

    let fireOverBoundaries size dir (m:Vehicle) =
        if HitBox.tryMove size dir m.hitBox
        then Some m 
        else None

    let hitByBullet (bs:Bullet list) (m:Vehicle) =
        match bs |> List.tryFind (fun b -> HitBox.intersect b.hitBox m.hitBox) with
        | Some b ->
            let vrx = Projection.project b.hitBox b.shape
                      |> Projection.reflect m.hitBox
                      |> Reflection.applyMatched (damage b.dmg) m.shape
                      |> Reflection.filterAll dead
                      |> Shape.shrink
            Some { m with shape = vrx } 
        | None -> Some m
        
    let takeCrate (rnd:Random) (s:Size) (dir:Direction) (cs:Crate list) (m:Vehicle) =
        match cs |> List.tryFind (fun c -> HitBox.intersect c.hitBox m.hitBox) with
        | Some c ->
            if Projection.project m.hitBox m.shape
               |> Projection.contains
               <| Projection.project c.hitBox c.shape
            then Crate.apply rnd s dir c m
            else Some m
        | None -> Some m
        
    let filterDead (m:Vehicle) =
        if (m.shape |> Reflection.filterAll dead |> Reflection.isEmpty)
        then None
        else Some m
        
    let health { Vehicle.shape = Reflection shape } =
        let getBodyHealth segment =
            match segment with
            | { kind = Body value } -> value
            | _ -> 0
        shape |> List.map getBodyHealth |> List.sum
        
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
    let rec tick (rnd:Random) (cs:GameCommand list) (game:Game)  =
        match cs with
        | Move(vid, dir)::rest ->
            let movePipe =
                Movable.ret
                >> Movable.bind (Vehicle.moveById vid)
                >> Movable.bind (Vehicle.stopOverBoundaries game.size dir)
                >> Movable.bind (Vehicle.stopOverVehicles game.vehicles dir)
                >> Movable.bind (Vehicle.moveVehicle dir)
            let moved =
                game.vehicles
                |> List.map movePipe
                |> List.map Movable.unwind
            let hitPipe =
                Option.ret
                >> Option.bind (Vehicle.takeCrate rnd game.size dir game.crates)
                >> Option.bind (Vehicle.filterDead)
            let survived =
                moved
                |> List.map hitPipe
                |> List.choose id
            tick rnd rest { game with vehicles = survived } 
        | Fire(vid, dir)::rest ->
            let firePipe =
                Vehicle.fireById vid
                >> Option.bind (Vehicle.fireOverBoundaries game.size dir)
                >> Option.bind (Vehicle.fire dir)
            let fired =
                game.vehicles
                |> List.map firePipe
                |> List.choose id
                |> List.concat
            tick rnd rest { game with bullets = game.bullets @ fired } 
        | Tick::rest ->
            let rec subTick subGame subEpoch =
                let epochBullets =
                    subGame.bullets
                    |> List.filter (fun b -> b.spd >= subEpoch)
                if epochBullets |> List.isEmpty
                then
                    { subGame with time = subGame.time + 1u }
                else
                    let movePipe =
                        Option.ret
                        >> Option.bind (Bullet.stopOverBoundaries game.size)
                        >> Option.bind (Bullet.stopOverVehicles game.vehicles)
                        >> Option.bind (Bullet.moveBullet)
                    let moved =
                        epochBullets
                        |> List.map movePipe
                        |> List.choose id
                    let hitPipe =
                        Option.ret
                        >> Option.bind (Vehicle.hitByBullet moved)
                        >> Option.bind (Vehicle.filterDead)
                    let survived =
                        subGame.vehicles
                        |> List.map hitPipe
                        |> List.choose id
                    subTick { subGame with bullets = moved; vehicles = survived; time = subGame.time + 1u } (subEpoch + 1)
            let cratePipe =
                Option.ret
                >> Option.bind (Crate.disappearOverVehicles game.vehicles)
            let spawned = Crate.spawn rnd game.size game.crates
            let crates =
                game.crates
                |> List.map cratePipe
                |> List.append [spawned]
                |> List.choose id
            tick rnd rest (subTick { game with crates = crates } 1) 
        | [] -> game
        
    let addVehicle rnd id game =
        let vx = {
            id = id
            dmg = 1
            hitBox = HitBox.single (Position.getRandom rnd game.size)
            shape = Reflection.createWith (Body 9) [(0, 0)]
            color = Vehicle.getColor rnd
        }
        { game with vehicles = vx::game.vehicles }
        
    let remVehicle id game =
        { game with vehicles = game.vehicles |> List.filter (fun v -> not (v.id = id)) }
        
    let empty size =
        { bullets = []; vehicles = []; crates = []; size = size; time = 0u }
       
module Area =
    let empty = { users = []; commands = []; connections = Map.empty }
    
    let addUser cnn area =
        let id =
            if not (area.users |> List.isEmpty)
            then area.users |> List.max |> (+) 1
            else 1
        id, { area with users = id::area.users; connections = area.connections |> Map.add cnn id }

    let remUser cnn id area =
        let idx =
            match area.connections |> Map.tryFind cnn  with
            | Some found -> found
            | None -> id
        idx,
        { area with users = area.users |> List.filter (fun ux -> not (ux = idx))
                    connections = area.connections |> Map.remove cnn }
        