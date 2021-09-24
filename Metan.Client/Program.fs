open System
open MBrace.FsPickler
open Metan.Core
open Microsoft.AspNetCore.SignalR.Client
  
let decode<'T> (bs:BinarySerializer) =
    bs.UnPickle<'T>
  
let encode (bs:BinarySerializer) =
    bs.Pickle

type UserState = { mutable id: UserId option; mutable hitBox: bool }

let printGT (game:Game) =
    match game.size with
    | w, _ ->
        Console.ForegroundColor <- ConsoleColor.White
        Console.SetCursorPosition (w + 2, 0)
        Console.Write $"Game time: {game.time}"

let printHB (prev: Game) (game:Game) =
    let printHitBox (HitBox ((x1, y1), (x2, y2))) (c:char) (cc:ConsoleColor) =
        Console.ForegroundColor <- cc
        for x = x1 to x2 do
            for y = y1 to y2 do
                Console.SetCursorPosition (x, y)
                Console.Write c
    for v in prev.vehicles do
        printHitBox v.hitBox ' ' ConsoleColor.Black
    for b in prev.bullets do
        printHitBox b.hitBox ' ' ConsoleColor.Black
    for c in prev.crates do
        printHitBox c.hitBox ' ' ConsoleColor.Black
    for v in game.vehicles do
        printHitBox v.hitBox 'V' ConsoleColor.Red
    for c in game.crates do
        printHitBox c.hitBox 'X' ConsoleColor.Green
    for c in game.bullets do
        printHitBox c.hitBox 'B' ConsoleColor.Yellow

let print (prev: Game) (game:Game) =
    for v in prev.vehicles do
        match Projection.project v.hitBox v.shape with
        | Projection ps ->
            for p in ps do
                Console.SetCursorPosition p.pos
                Console.Write ' '
    for b in prev.bullets do
        let tl = HitBox.topLeft b.hitBox
        Console.SetCursorPosition tl
        Console.Write ' '
    for c in prev.crates do
        let tl = HitBox.topLeft c.hitBox
        Console.SetCursorPosition tl
        Console.Write ' '
    let color = Console.ForegroundColor     
    for v in game.vehicles do
        Console.ForegroundColor <- v.color
        match Projection.project v.hitBox v.shape with
        | Projection sl ->
            for s in sl do
                Console.SetCursorPosition s.pos
                match s.kind with
                | Body health ->
                    Console.Write health
                | Dmg dmg ->
                    Console.Write dmg
    Console.ForegroundColor <- ConsoleColor.Yellow
    for b in game.bullets do
        let tl = HitBox.topLeft b.hitBox
        Console.SetCursorPosition tl
        Console.Write '.'
    for c in game.crates do
        let tl = HitBox.topLeft c.hitBox
        Console.SetCursorPosition tl
        match c.bonus with
        | HealthBonus _ ->
            Console.ForegroundColor <- ConsoleColor.Red
            Console.Write 'H'
        | DamageBonus _ ->
            Console.ForegroundColor <- ConsoleColor.Yellow
            Console.Write 'D'
        | ShapeBonus ->
            Console.ForegroundColor <- ConsoleColor.Yellow
            Console.Write 'S'
        | RandomBonus _ ->
            Console.ForegroundColor <- ConsoleColor.Gray
            Console.Write '?'
    Console.ForegroundColor <- color

let onAreaEvent (us:UserState) (decode:byte[] -> AreaEvent) (data:byte[]) =
    match decode data with
    | State (prev, game) ->
        printGT game
        if us.hitBox
        then printHB prev game
        else print prev game
    | UserEvent (_, UserJoined id) ->
        Console.Clear()
        Console.CursorVisible <- false
        us.id <- Some id
    | UserEvent (_, UserLeft) ->
        Console.Clear()
        printfn "User left"
        Environment.Exit 0
        us.id <- None

let call (c:HubConnection) (encode:AreaCommand -> byte[]) (cmd:AreaCommand) =
    c.SendAsync("CallAreaCommand", encode cmd)
    |> Async.AwaitTask
    |> Async.RunSynchronously

[<EntryPoint>]
let main _ =
    let connection =
        (HubConnectionBuilder())
          .WithAutomaticReconnect()
          .WithUrl("http://localhost:5000/server")
          .Build()
    
    let us = { id = None; hitBox = false }
    let bs = BinarySerializer()
    let enc = encode bs
    let dec = decode bs
    
    use sub = connection.On("AreaEventSent", onAreaEvent us dec)
    
    let tryConnect () = 
        try
            connection.StartAsync()
            |> Async.AwaitTask
            |> Async.RunSynchronously
            printfn "Connected successfully"
            true
        with
            | e -> printfn $"{e.Message}"; false
    
    let mutable connected = tryConnect
    while not (connected()) do
        let info = Console.ReadKey()
        match info.Key with
        | ConsoleKey.Escape -> Environment.Exit 0
        | _ -> connected <- tryConnect
    
    let callAreaCommand cmd = call connection enc cmd
    let callUserCommand cmd = callAreaCommand (UserCommand ("", cmd))
    let callGameCommand cmd = callAreaCommand (AreaCommand cmd) 
    
    callUserCommand Join
    
    let mutable info = Console.ReadKey();
    while (not <| info.Key.Equals(ConsoleKey.E)) do
        let clearInputChar () =
            Console.SetCursorPosition (Console.CursorLeft - 1, Console.CursorTop)
            Console.Write ' '
            
        match us.id with
        | Some id ->
            match info.Key with
            | ConsoleKey.LeftArrow ->
                callGameCommand (Move (id, Left))
            | ConsoleKey.RightArrow ->
                callGameCommand (Move (id, Right))
            | ConsoleKey.UpArrow ->
                callGameCommand (Move (id, Down))
            | ConsoleKey.DownArrow ->
                callGameCommand (Move (id, Up))
            | ConsoleKey.A ->
                callGameCommand (Fire (id, Left))
                clearInputChar ()
            | ConsoleKey.D ->
                callGameCommand (Fire (id, Right))
                clearInputChar ()
            | ConsoleKey.W ->
                callGameCommand (Fire (id, Down))
                clearInputChar ()
            | ConsoleKey.S ->
                callGameCommand (Fire (id, Up))
                clearInputChar ()
            | ConsoleKey.H ->
                us.hitBox <- not us.hitBox
                Console.Clear()
            | ConsoleKey.B ->
                callAreaCommand JoinBot
                clearInputChar ()
            | ConsoleKey.Escape ->
                callUserCommand (Leave id)
            | _ -> ()
        | None ->
            printfn "User not connected"
            
        info <- Console.ReadKey()
    
    Console.ReadKey()
    |> ignore
    0