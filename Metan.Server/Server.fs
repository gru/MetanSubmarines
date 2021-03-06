namespace Metan.Server

open System
open System.Threading.Tasks
open Microsoft.AspNetCore.SignalR
open Microsoft.Extensions.Hosting
open Akka.Actor
open Akkling
open MBrace.FsPickler
open Metan.Core

module Serialization =
    let decode<'T> (bs:BinarySerializer) =
        bs.UnPickle<'T>
    
    let encode (bs:BinarySerializer) =
        bs.Pickle

type SignalRHub () =
     inherit Hub ()
     
     let bs = BinarySerializer()
     let dec = Serialization.decode bs
    
     member this.ActorSystem: ActorSystem =
         let actorSystem =
             this.Context
                 .GetHttpContext()
                 .RequestServices
                 .GetService(typeof<ActorSystem>)
         actorSystem :?> ActorSystem

     member this.AreaActor: TypedActorSelection<AreaCommand> =
         select this.ActorSystem "user/area"

     member this.CallAreaCommand(msg: byte[]) =
        let cmd =
            match dec msg with
            | UserCommand (_, uc) ->
                UserCommand (this.Context.ConnectionId, uc)
            | cmd -> cmd
        this.AreaActor <! cmd 
        Task.CompletedTask
        
     override this.OnDisconnectedAsync(_:Exception) =
         this.AreaActor <! UserCommand(this.Context.ConnectionId, Leave(-1))
         Task.CompletedTask
         
type EventPublisher (hub:IHubContext<SignalRHub>) =
     let bs = BinarySerializer()
     let enc = Serialization.encode bs
     
     member this.SendAreaEvent(evt: AreaEvent) =
         hub.Clients.All.SendAsync("AreaEventSent", enc evt)
         |> Async.AwaitTask
         |> Async.RunSynchronously
         
     member this.SendAreaEvent(cnn:string, evt: AreaEvent) =
         hub.Clients.Client(cnn).SendAsync("AreaEventSent", enc evt)
         |> Async.AwaitTask
         |> Async.RunSynchronously
         
[<AutoOpen>]
module Actors =
    let rec bot (me:Actor<AreaEvent>) =
        let rnd = Random()
        let tryFindMe id game =
            game.vehicles |> List.tryFind (fun v -> v.id = id)
        let replyAndSwitch f1 f2 (p1, p2) =
            f1 p2; f2 p1
        let rec connecting () =
            actor {
                let! msg = me.Receive()
                match msg with
                | UserEvent (_, UserJoined id) ->
                    become (searching(id))
                | _ -> ignored()
            }
        and searching id = function
            | State (_, game) ->
                match game |> tryFindMe id with
                | Some v ->
                    AI.searchCrate rnd game v
                    |> replyAndSwitch (reply (me.Sender())) (switch id)
                | None -> stop()
            | _ -> ignored() 
        and moving id state = function
            | State (_, game) ->
                match game |> tryFindMe id with
                | Some v ->
                    AI.move rnd game v state
                    |> replyAndSwitch (reply (me.Sender())) (switch id)
                | None -> stop()
            | _ -> ignored() 
        and idle id state = function
            | State (_, game) ->
                match game |> tryFindMe id with
                | Some _ ->
                    AI.idle game state
                    |> replyAndSwitch (reply (me.Sender())) (switch id)
                | None -> stop() 
            | _ -> ignored() 
        and reply areaRef commands =
            for cmd in commands do
                areaRef <! cmd
        and switch id state =
            printfn $"%A{state}"
            become (match state with
                    | SearchCrate -> searching id 
                    | MoveTo _ -> moving id state
                    | Idle _ -> idle id state)
        connecting()
    
    let rec client (ep:EventPublisher) (me:Actor<AreaEvent>) =
        let rec connecting() = actor {
            let! message = me.Receive()
            match message with
                | UserEvent (cnn, UserJoined _) as evt ->
                    ep.SendAreaEvent (cnn, evt)
                    return! connected
                | _ -> ignored()
            }
        and connected = 
            actor {
                let! message = me.Receive()
                match message with
                | State _ as evt ->
                    ep.SendAreaEvent evt 
                | UserEvent (cnn, UserLeft) as evt ->
                    ep.SendAreaEvent (cnn, evt)
                    return! stop()
                | _ -> ignored()
            }
        connecting()

    let rec area (ep:EventPublisher) (me:Actor<AreaCommand>)  =
        me.ScheduleRepeatedly
            TimeSpan.Zero (TimeSpan.FromSeconds 0.1) me.Self (AreaCommand Tick) |> ignore

        let rnd = Random()
        let rec awaiting area game = actor {
            let! command = me.Receive()
            match command with
            | UserCommand (_, Join) as cmd ->
                me.Self <! cmd
                return! loop(area) game
            | _ ->
                return! awaiting(area) game
            }
        and loop area game = actor {
            let! command = me.Receive ()
            match command with
            | AreaCommand Tick ->
                let gx = Game.tick rnd (Tick :: area.commands) game
                let all = select me "client_*"
                all <! State (game, gx)
                return! loop({ area with commands = [] }) gx
            | AreaCommand cmd ->
                return! loop({ area with commands = cmd::area.commands }) game
            | UserCommand (cnn, Join) ->
                let id, ax = area |> Area.addUser cnn
                let gx = game |> Game.addVehicle rnd id
                let usr = props (client ep)
                          |> spawn me $"client_{id}"
                usr <! UserEvent (cnn, UserJoined id)
                let all = select me "client_*"
                all <! State (game, gx)
                return! loop(ax) gx
            | UserCommand (cnn, Leave id) ->
                let idx, ax = area |> Area.remUser cnn id
                let gx = game |> Game.remVehicle idx
                let usr = select me $"client_{idx}"
                usr <! UserEvent (cnn, UserLeft)
                let all = select me "client_*"
                all <! State (game, gx)
                return! if gx.vehicles |> List.isEmpty
                    then awaiting(ax) gx
                    else loop(ax) gx
            | JoinBot ->
                let id, ax = area |> Area.addUser ""
                let gx = game |> Game.addVehicle rnd id
                let bot = spawn me $"client_{id}" (props bot)
                bot <! UserEvent ("", UserJoined id)
                let all = select me "client_*"
                all <! State (game, gx)
                return! loop(ax) gx
        }
        awaiting Area.empty (Game.empty (Size(79, 47)))

type ActorService (system:ActorSystem, ep:EventPublisher, appLifetime:IHostApplicationLifetime) =
    interface IHostedService with 
        member this.StartAsync _ =
            props(area ep)
                |> spawn system "area"
                |> ignore
    
            system.WhenTerminated
                .ContinueWith(fun x -> appLifetime.StopApplication())
                |> ignore
                
            Task.CompletedTask
            
        member this.StopAsync _ =
            CoordinatedShutdown
                .Get(system)
                .Run(CoordinatedShutdown.ClrExitReason.Instance) :> Task