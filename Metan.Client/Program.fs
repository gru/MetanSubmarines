open System
open MBrace.FsPickler
open Metan
open Metan.Core
open Microsoft.AspNetCore.SignalR.Client
open Metan.Core.ChangeDetection

let decode<'T> (bs:BinarySerializer) =
    bs.UnPickle<'T>
  
let encode (bs:BinarySerializer) =
    bs.Pickle

let onAreaEvent (client:Submarines) (decode:byte[] -> AreaEvent) (data:byte[]) =
    let toClientHitBox (HitBox((tlx, tly), (brx, bry))) =
        ClientHitBox(tlx, tly, brx, bry)
    let apply change =
        match change with
        | VehicleAdded({ id = id; hitBox = hitBox; dmg = dmg }) ->
            client.State.VehicleAdded(id, toClientHitBox hitBox , dmg)
        | VehicleRemoved(id) ->
            client.State.VehicleRemoved(id)
        | VehicleMoved(id, _, hitBox) ->
            client.State.VehicleMoved(id, toClientHitBox hitBox)
        | CrateAdded({ id = id; hitBox = hitBox }) ->
            client.State.CrateAdded(id, toClientHitBox hitBox)
        | CrateRemoved(id) ->
            client.State.CrateRemoved(id)
        | BulletAdded({ id = id; hitBox = hitBox }) ->
            client.State.BulletAdded(id, toClientHitBox hitBox)
        | BulletRemoved(id) ->
            client.State.BulletRemoved(id)
        | BulletMoved(id, _, hitBox) ->
            client.State.BulletMoved(id, toClientHitBox hitBox)
        | _ -> ()
    match decode data with
    | State (previous, current) ->
        if client.Ready
        then
            if client.State.Initialized
            then
                for change in delta previous current do
                    apply change
            else
                for change in init current do
                    apply change
                    
                client.State.Initialized <- true
        else ()
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
                    client.Options.ShowHitBox <- true
                | UserEvent.HideHitBox ->
                    client.Options.ShowHitBox <- false
                | UserEvent.Leave ->
                     callUserCommand (Leave client.Id)
                | _ ->
                    ()
            )
    client.Run()
    0