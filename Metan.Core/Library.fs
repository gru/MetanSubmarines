namespace Metan.Core

open System

[<AutoOpen>]
module Core =
    type Position = int * int
    type Direction = Up | Down | Left | Right
    type Size = Size of int * int
    type Damage = int
    type Bullet =
        {
            pos:Position
            dir:Direction
            dmg:Damage
        }
    type Health = int
    type VehicleId = int
    type Vehicle =
        {
            id:VehicleId
            pos:Position
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
        | RandomBonus
    type Crate = { pos:Position; bonus:Bonus }
        
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
        
    let move dir (x, y) =
        match dir with
        | Up -> (x, y + 1)
        | Down -> (x, y - 1)
        | Left -> (x - 1, y)
        | Right -> (x + 1, y)

    let onBounds (Size(w, h)) (x, y) =
        x = 0 || y = 0 || x = w || y = h
        
    let inBounds (Size(w, h)) (x, y) =
        x > 0 || y > 0 || x < w || y < h

    let outBounds (Size(w, h)) (x, y) =
        x < 0 || y < 0 || x > w || y > h

    let getRandomPosition (rnd:Random) (Size(w, h)) =
        (rnd.Next(0, w), rnd.Next(0, h))
        
    let getRandomColor (rnd:Random) =
        let ns = Enum.GetNames(typeof<ConsoleColor>)
        Enum.Parse<ConsoleColor>(ns.[rnd.Next(0, ns.Length - 1)]) 

module Option =
    let ret v = Some v

module List =
    let any f vs =
        vs |> List.tryFind f |> Option.isSome  
        
module Crate =
    let rec apply (rnd:Random) (c:Crate) (v:Vehicle) =
        match c.bonus with
        | HealthBonus health ->
            Some { v with health = min 9 (health + v.health) }
        | DamageBonus damage ->
            if damage < v.health
            then Some { v with health = v.health - damage }
            else None
        | RandomBonus ->
            if rnd.NextDouble() > 0.5
            then apply rnd { c with bonus = DamageBonus (rnd.Next(1, 5)) } v
            else apply rnd { c with bonus = HealthBonus (rnd.Next(1, 5)) } v
            
    let disappearOverVehicles (vs:Vehicle list) (c:Crate) =
        match vs |> List.tryFind (fun v -> v.pos = c.pos) with
        | Some _ -> None
        | None -> Some c
    
    let spawn (rnd:Random) size cs =
        if cs |> List.length < 5 then
            let pr = rnd.Next(0, 100)
            if pr >= 0 && pr < 1
            then Some { pos = getRandomPosition rnd size; bonus = HealthBonus(rnd.Next(1, 5)) }
            elif pr >= 1 && pr < 2
            then Some { pos = getRandomPosition rnd size; bonus = DamageBonus(1) }
            elif pr >= 2 && pr < 3
            then Some { pos = getRandomPosition rnd size; bonus = RandomBonus }
            else None
        else None
            
module Bullet =
    let moveBullet (b:Bullet) =
        Some { b with pos = move b.dir b.pos }

    let stopOverBoundaries (s:Size) (b:Bullet) =
        let px = move b.dir b.pos
        if outBounds s px
        then None
        else Some b

    let rec stopOverVehicles (vs: Vehicle list) (b:Bullet) =
        match vs |> List.tryFind (fun v -> v.pos = b.pos) with
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
        Movable { m with pos = move dir m.pos }
        
    let stopOverBoundaries size dir (m:Vehicle) =
        if outBounds size (move dir m.pos)
        then Blocked m
        else Movable m
    
    let stopOverVehicles (vs:Vehicle list) dir (m:Vehicle) =
        let px = move dir m.pos
        if vs |> List.any (fun v -> v.pos = px) 
        then Blocked m
        else Movable m

    let moveById id (m:Vehicle) =
        if m.id = id
        then Movable m
        else Blocked m

    let fire dir (m:Vehicle) =
        Some [{ Bullet.pos = move dir m.pos; dmg = m.dmg; dir = dir }]

    let fireById id (m:Vehicle) =
        if m.id = id
        then Some m
        else None

    let fireOverBoundaries size dir (m:Vehicle) =
        let px = move dir m.pos
        if (outBounds size px)
        then None
        else Some m 

    let hitByBullet (bs:Bullet list) (m:Vehicle) =
        match bs |> List.tryFind (fun b -> b.pos = m.pos) with
        | Some b ->
            if b.dmg < m.health
            then Some { m with health = m.health - b.dmg } 
            else None
        | None -> Some m
        
    let takeCrate (rnd:Random) (cs:Crate list) (m:Vehicle) =
        match cs |> List.tryFind (fun c -> c.pos = m.pos) with
        | Some c -> Crate.apply rnd c m
        | None -> Some m

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
        
    let addVehicle id pos cr vs =
        let vx = { id = id; dmg = 1; health = 9; pos = pos; color = cr }
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