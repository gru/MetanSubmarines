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
    type GameCommand =
        | Move of VehicleId * Direction
        | Fire of VehicleId * Direction
        | Tick
    type Game =
        {
          bullets:Bullet list
          vehicles:Vehicle list
          size: Size
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


module Option =
    let ret v = Some v

module List =
    let any f vs =
        vs |> List.tryFind f |> Option.isSome  
        
module Bullet =
    let moveBullet (b:Bullet) =
        Some { b with pos = move b.dir b.pos }

    let stopOverBoundaries (s:Size) (b:Bullet) =
        let px = move b.dir b.pos
        if outBounds s px
        then None
        else Some b

    let rec stopOverVehicles vs (b:Bullet) =
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
    
    let stopOverVehicles vehicles dir (m:Vehicle) =
        let px = move dir m.pos
        if vehicles |> List.any (fun v -> v.pos = px) 
        then Blocked m
        else Movable m

    let moveOverId id (m:Vehicle) =
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
            if (b.dmg < m.health)
            then Some { m with health = m.health - b.dmg } 
            else None
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
    let rec tick (game:Game) (cs:GameCommand list) =
        match cs with
        | Move(vid, dir)::rest ->
            let pipe = Movable.ret
                       >> Movable.bind (Vehicle.moveOverId vid)
                       >> Movable.bind (Vehicle.stopOverBoundaries game.size dir)
                       >> Movable.bind (Vehicle.stopOverVehicles game.vehicles dir)
                       >> Movable.bind (Vehicle.moveVehicle dir)
            let vs = game.vehicles
                     |> List.map pipe
                     |> List.map Movable.unwind
            tick { game with vehicles = vs } rest
        | Fire(vid, dir)::rest ->
            let firePipe = Vehicle.fireById vid
                           >> Option.bind (Vehicle.fireOverBoundaries game.size dir)
                           >> Option.bind (Vehicle.fire dir)
            let bs = game.vehicles
                     |> List.map firePipe
                     |> List.choose id
                     |> List.concat
            tick { game with bullets = game.bullets @ bs } rest
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
            let vs = game.vehicles
                     |> List.map hitPipe
                     |> List.choose id
            tick { game with bullets = bs; vehicles = vs } rest
        | [] -> game
        
    let getRandomPosition (Size(w, h)) =
        let r = Random()
        (r.Next(0, w), r.Next(0, h))
        
    let getRandomColor () =
        let r = Random()
        let ns = Enum.GetNames(typeof<ConsoleColor>)
        Enum.Parse<ConsoleColor>(ns.[r.Next(0, ns.Length - 1)]) 
        
    let remVehicle id vs =
        vs |> List.filter (fun v -> not (v.id = id))
        
    let addVehicle id size vs =
        let cr = getRandomColor ()
        let px = getRandomPosition size
        let vx = { id = id; dmg = 1; health = 9; pos = px; color = cr }
        vx::vs
        
module Area =
    let addUser us =
        if not (us |> List.isEmpty)
        then us |> List.max |> (+) 1
        else 1

    let remUser (u:UserId) (us:UserId list) =
        us |> List.filter (fun ux -> not (ux = u))
        
    let anyUser (us:UserId list) =
        us |> List.isEmpty |> not