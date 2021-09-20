open System
open MBrace.FsPickler
open Metan.Core
open Microsoft.AspNetCore.SignalR.Client
  
let decode<'T> (bs:BinarySerializer) =
    bs.UnPickle<'T>
  
let encode (bs:BinarySerializer) =
    bs.Pickle

let print (prev: Game) (game:Game) =
    match game.size with
    | Size (w, _) ->
        Console.SetCursorPosition (w + 2, 0)
        Console.Write $"Game time: {game.time}" 
    for v in prev.vehicles do
        Console.SetCursorPosition v.pos
        Console.Write ' '
    for b in prev.bullets do
        Console.SetCursorPosition b.pos
        Console.Write ' '
    for c in prev.crates do
        Console.SetCursorPosition c.pos
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
    for c in game.crates do
        Console.SetCursorPosition c.pos
        match c.bonus with
        | HealthBonus _ ->
            Console.ForegroundColor <- ConsoleColor.Red
            Console.Write 'H'
        | DamageBonus _ ->
            Console.ForegroundColor <- ConsoleColor.Yellow
            Console.Write 'D'
        | RandomBonus _ ->
            Console.ForegroundColor <- ConsoleColor.Gray
            Console.Write '?'
    Console.ForegroundColor <- color
    
type UserState = { mutable id: UserId option }

let onAreaEvent (us:UserState) (decode:byte[] -> AreaEvent) (data:byte[]) =
    match decode data with
    | State (prev, game) ->
        print prev game
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
    
    let us = { id = None }
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
    
    let callUserCommand cmd = call connection enc (UserCommand ("", cmd))
    let callGameCommand cmd = call connection enc (AreaCommand cmd) 
    
    callUserCommand Join
    
    let mutable info = Console.ReadKey();
    while (not <| info.Key.Equals(ConsoleKey.E)) do
        let clearInputChar () =
            Console.SetCursorPosition (Console.CursorLeft - 1, Console.CursorTop)
            Console.Write ' '
            
        match us.id with
        | Some id ->
            match info.Key, info.KeyChar with
            | ConsoleKey.LeftArrow, _ ->
                callGameCommand (Move (id, Left))
            | ConsoleKey.RightArrow, _ ->
                callGameCommand (Move (id, Right))
            | ConsoleKey.UpArrow, _ ->
                callGameCommand (Move (id, Down))
            | ConsoleKey.DownArrow, _ ->
                callGameCommand (Move (id, Up))
            | ConsoleKey.A, _ ->
                callGameCommand (Fire (id, Left))
                clearInputChar ()
            | ConsoleKey.D, _ ->
                callGameCommand (Fire (id, Right))
                clearInputChar ()
            | ConsoleKey.W, _ ->
                callGameCommand (Fire (id, Down))
                clearInputChar ()
            | ConsoleKey.S, _ ->
                callGameCommand (Fire (id, Up))
                clearInputChar ()
            | ConsoleKey.Escape, _ ->
                callUserCommand (Leave id)
            | _ -> ()
        | None ->
            printfn "User not connected"
            
        info <- Console.ReadKey()
    
    Console.ReadKey()
    |> ignore
    0