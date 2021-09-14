open System
open Akkling

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
type Movable<'T> =
    | Movable of 'T
    | Blocked of 'T
    
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
        List.fold (fun r v -> r || (f v)) false vs
        
    let all f vs  =
        List.fold (fun r v -> r && (f v)) true vs
        
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
    
let print (game:Game) (prev: Game) =
    for v in prev.vehicles do
        Console.SetCursorPosition v.pos
        Console.Write ' '
    for b in prev.bullets do
        Console.SetCursorPosition b.pos
        Console.Write ' '
    let color = Console.ForegroundColor     
    for v in game.vehicles do
        Console.SetCursorPosition v.pos
        Console.ForegroundColor <- v.color
        Console.Write v.health
    Console.ForegroundColor <- ConsoleColor.Yellow
    for b in game.bullets do
        Console.SetCursorPosition b.pos
        Console.Write '.'
    Console.ForegroundColor <- color
         
let rec area (m:Actor<GameCommand>) =
    m.ScheduleRepeatedly
        TimeSpan.Zero (TimeSpan.FromSeconds 0.1) m.Self Tick |> ignore
    
    let rec loop prev cs = actor {
        let! command = m.Receive ()
        match command with
        | Tick ->
            let cur = Game.tick prev (Tick :: cs)
            print cur prev
            return! loop(cur) []
        | c ->
            return! loop(prev) (c :: cs)
    }
    loop { bullets = []
           vehicles = [ { id = 1; pos = (0, 0); dmg = 1; health = 9; color = ConsoleColor.Red }
                        { id = 2; pos = (10, 10); dmg = 1; health = 9; color = ConsoleColor.Blue } ]
           size = Size(50, 25) }
         []
            
let clearInputChar () =
    Console.SetCursorPosition (Console.CursorLeft - 1, Console.CursorTop)
    Console.Write ' '
            
[<EntryPoint>] 
let main _ =
    let system = System.create "system" <| Configuration.defaultConfig()
    let area = spawn system "area" <| props(area)

    Console.Clear()
    Console.CursorVisible <- false

    let mutable info = Console.ReadKey();
    while (not <| info.Key.Equals(ConsoleKey.E)) do
        match info.Key, info.KeyChar with
        | ConsoleKey.LeftArrow, _ ->
            area <! Move(1, Left)
        | ConsoleKey.RightArrow, _ ->
            area <! Move(1, Right)
        | ConsoleKey.UpArrow, _ ->
            area <! Move(1, Down)
        | ConsoleKey.DownArrow, _ ->
            area <! Move(1, Up)
        | ConsoleKey.A, _ ->
            area <! Fire(1, Left)
            clearInputChar ()
        | ConsoleKey.D, _ ->
            area <! Fire(1, Right)
            clearInputChar ()
        | ConsoleKey.W, _ ->
            area <! Fire(1, Down)
            clearInputChar ()
        | ConsoleKey.S, _ ->
            area <! Fire(1, Up)
            clearInputChar ()
        | _ -> ()
        info <- Console.ReadKey()

    system.Terminate()
    |> Async.AwaitTask
    |> ignore
    0
