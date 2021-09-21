namespace Metan.Core

open System

[<AutoOpen>]
module Core =
    type Size = int * int
    type Position = int * int
    type Direction = Up | Down | Left | Right
    type Shape = Shape of Position list
    type HitBox = HitBox of Position * Position
    type Damage = int
    type Bullet =
        {
            hitBox:HitBox
            dir:Direction
            dmg:Damage
        }
    type Health = int
    type VehicleId = int
    type Vehicle =
        {
            id:VehicleId
            hitBox:HitBox
            shape:Position list
            dmg:Damage
            health:Health
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
    type Crate = { hitBox:HitBox; bonus:Bonus }
        
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
          time: uint
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
        | UserCommand of ConnectionId * UserCommand
        | AreaCommand of GameCommand
    type Area =
        {
            users: UserId list
            commands: GameCommand list
        }

module Position =
    let move dir (x, y) =
        match dir with
        | Up -> (x, y + 1)
        | Down -> (x, y - 1)
        | Left -> (x - 1, y)
        | Right -> (x + 1, y)

    let getRandom (rnd:Random) ((w, h):Size) =
        (rnd.Next(0, w), rnd.Next(0, h))

module Option =
    let ret v = Some v

module List =
    let any f vs =
        vs |> List.tryFind f |> Option.isSome  

module HitBox =
    let single p =
        HitBox (p, p)
        
    let intersect (HitBox((l1, t1), (r1, b1))) (HitBox((l2, t2), (r2, b2))) =
        // ! ( b.left > a.right || b.right < a.left || b.top < a.bottom || b.bottom > a.top)
        not (l2 > r1 || r2 < l1 || t2 < b1 || b2 > t1)

    let move dir (HitBox((l, t), (r, b))) =
        match dir with
        | Up -> HitBox ((l, t + 1), (r, b + 1))
        | Down -> HitBox ((l, t - 1), (r, b - 1))
        | Left -> HitBox ((l - 1, t), (r - 1, b))
        | Right -> HitBox ((l + 1, t), (r + 1, b))
        
    let topLeft (HitBox(lt, _)) =
        lt
        
    let bottomRight (HitBox(_, rb)) =
        rb
        
    let outBounds (w, h) (HitBox((l, t), (r, b))) =
        t < 0 || b > h || l < 0 || r > w

module Shape =
    exception EmptyGlobalShape
    
    let lcl = [ (0, 0) ]
    
    let glb pos = Shape [ pos ]
    
    let toGlb ((gx, gy): Position) = function
        | ps ->
            Shape (ps |> List.map (fun (x, y) -> (x + gx, y + gy)))

    let toLcl ((gx, gy): Position) = function
        | Shape ps ->
            (ps |> List.map (fun (x, y) -> (x - gx, y - gy)))
    
    let join (Shape(ps1)) (Shape(ps2)) =
        Shape (ps1 @ ps2)

    let move (dir:Direction) (Shape(ps)) =
        Shape (ps |> List.map (Position.move dir))
        
    let toHitBox (Shape(ps)) =
        match ps with
        | [] ->
            raise EmptyGlobalShape
        | [ p ] ->
            HitBox.single p
        | _ ->
            let xs = ps |> List.map fst   
            let ys = ps |> List.map snd   
            HitBox ((List.min xs, List.min ys), (List.max xs, List.max ys))

module Crate =
    let rec apply (rnd:Random) (c:Crate) (v:Vehicle) =
        match c.bonus with
        | HealthBonus health ->
            Some { v with health = min 9 (health + v.health) }
        | DamageBonus damage ->
            if damage < v.health
            then Some { v with health = v.health - damage }
            else None
        | ShapeBonus ->
            let tl = HitBox.topLeft v.hitBox
            let gv = Shape.toGlb tl v.shape
            let gc = Shape.glb tl
            let gvx = Shape.move Right gv
            let gvy = Shape.join gc gvx
            let hb = Shape.toHitBox gvy
            Some { v with hitBox = hb; shape = Shape.toLcl tl gvy }
        | RandomBonus ->
            if rnd.NextDouble() > 0.5
            then apply rnd { c with bonus = DamageBonus (rnd.Next(1, 5)) } v
            else apply rnd { c with bonus = HealthBonus (rnd.Next(1, 5)) } v
            
    let disappearOverVehicles (vs:Vehicle list) (c:Crate) =
        match vs |> List.tryFind (fun v -> HitBox.intersect v.hitBox c.hitBox) with
        | Some _ -> None
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
        | Some _ -> None
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
            if b.dmg < m.health
            then Some { m with health = m.health - b.dmg } 
            else None
        | None -> Some m
        
    let takeCrate (rnd:Random) (cs:Crate list) (m:Vehicle) =
        match cs |> List.tryFind (fun c -> HitBox.intersect c.hitBox m.hitBox) with
        | Some c -> Crate.apply rnd c m
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
            let pipe = Movable.ret
                       >> Movable.bind (Vehicle.moveById vid)
                       >> Movable.bind (Vehicle.stopOverBoundaries game.size dir)
                       >> Movable.bind (Vehicle.stopOverVehicles game.vehicles dir)
                       >> Movable.bind (Vehicle.moveVehicle dir)
            let vs = game.vehicles
                     |> List.map pipe
                     |> List.map Movable.unwind
            tick rnd { game with vehicles = vs } rest
        | Fire(vid, dir)::rest ->
            let firePipe = Vehicle.fireById vid
                           >> Option.bind (Vehicle.fireOverBoundaries game.size dir)
                           >> Option.bind (Vehicle.fire dir)
            let bs = game.vehicles
                     |> List.map firePipe
                     |> List.choose id
                     |> List.concat
            tick rnd { game with bullets = game.bullets @ bs } rest
        | Tick::rest ->
            let movePipe = Option.ret
                           >> Option.bind (Bullet.stopOverBoundaries game.size)
                           >> Option.bind (Bullet.stopOverVehicles game.vehicles)
                           >> Option.bind (Bullet.moveBullet)
            let bs = game.bullets
                     |> List.map movePipe
                     |> List.choose id
            let hitPipe = Option.ret
                          >> Option.bind (Vehicle.hitByBullet bs)
                          >> Option.bind (Vehicle.takeCrate rnd game.crates)
            let vs = game.vehicles
                     |> List.map hitPipe
                     |> List.choose id
            let cratePipe = Option.ret
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
        let vx = { id = id; dmg = 1; health = 9; hitBox = hb; shape = Shape.lcl; color = cr }
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