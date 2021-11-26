open System
open MBrace.FsPickler
open Metan
open Metan.Core
open Microsoft.AspNetCore.SignalR.Client
  
let decode<'T> (bs:BinarySerializer) =
    bs.UnPickle<'T>
  
let encode (bs:BinarySerializer) =
    bs.Pickle

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
                | Nothing -> ()
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

let onAreaEvent (client:Submarines) (decode:byte[] -> AreaEvent) (data:byte[]) =
    let toClientHitBox (HitBox((tlx, tly), (brx, bry))) =
        ClientHitBox(tlx, tly, brx, bry)
    match decode data with
    | State (_, game) ->
        client.Clear()
        for v in game.vehicles do
            let hb = toClientHitBox(v.hitBox)
            if client.ContainsVehicle(v.id)
            then client.MoveVehicle(v.id, hb)
            else client.AddVehicle(v.id, hb, v.dmg)
    | UserEvent (_, UserJoined id) ->
        client.Id <- id
    | UserEvent (_, UserLeft) ->
        client.Exit();

let call (c:HubConnection) (encode:AreaCommand -> byte[]) (cmd:AreaCommand) =
    c.SendAsync("CallAreaCommand", encode cmd)
    |> Async.AwaitTask
    |> Async.RunSynchronously

[<STAThread>]
[<EntryPoint>]
let main argv =
    let url =
        if argv |> Array.contains "-mondjo"
        then "https://submarines.mondjo.ru"
        elif argv |> Array.contains "-docker"
        then "http://localhost:8081" 
        else "http://localhost:5000"
        
    let connection =
        (HubConnectionBuilder())
          .WithAutomaticReconnect()
          .WithUrl($"{url}/server")
          .Build()
    
    let bs = BinarySerializer()
    let enc = encode bs
    let dec = decode bs
    
    use client = new Submarines()
    use serverSub = connection.On("AreaEventSent", onAreaEvent client dec)
    
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
    
    use clientSub =
        client.KeyboardEvent
            .Subscribe(fun e ->
                match e.Event with
                | UserEvent.MoveUp ->
                    callGameCommand (Move (client.Id, Up))
                | UserEvent.MoveDown ->
                    callGameCommand (Move (client.Id, Down))
                | UserEvent.MoveLeft ->
                    callGameCommand (Move (client.Id, Left))
                | UserEvent.MoveRight ->
                    callGameCommand (Move (client.Id, Right))
                | UserEvent.FireUp ->
                    callGameCommand (Fire (client.Id, Up))
                | UserEvent.FireDown ->
                    callGameCommand (Fire (client.Id, Down))
                | UserEvent.FireLeft ->
                    callGameCommand (Fire (client.Id, Left))
                | UserEvent.FireRight ->
                    callGameCommand (Fire (client.Id, Right))
                | UserEvent.ShowHitBox ->
                    client.ShowHitBox <- true
                | UserEvent.HideHitBox ->
                    client.ShowHitBox <- false
                | UserEvent.Leave ->
                     callUserCommand (Leave client.Id)
                | _ ->
                    ()
            )
    client.Run()
    0