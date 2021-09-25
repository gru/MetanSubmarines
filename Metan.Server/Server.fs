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
                AI.searchCrate rnd game id
                |> replyAndSwitch (reply (me.Sender())) (switch id)
            | _ -> ignored() 
        and moving id pos = function
            | State (_, game) ->
                AI.move game id pos
                |> replyAndSwitch (reply (me.Sender())) (switch id) 
            | _ -> ignored() 
        and idle id till = function
            | State (_, game) ->
                AI.idle game till
                |> replyAndSwitch (reply (me.Sender())) (switch id)
            | _ -> ignored() 
        and reply areaRef commands =
            for cmd in commands do
                areaRef <! cmd
        and switch id state =
            printfn $"%A{state}"
            match state with
            | SearchCrate -> become (searching id) 
            | MoveTo pos -> become (moving id pos)
            | Idle till -> become (idle id till)
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
                let gx = Game.tick rnd game (Tick :: area.commands)
                let ref = select me "client_*"
                ref <! State (game, gx)
                return! loop({ area with commands = [] }) gx
            | AreaCommand cmd ->
                return! loop({ area with commands = cmd::area.commands }) game
            | UserCommand (cnn, Join) ->
                let id = area.users |> Area.addUser
                let vc = Vehicle.getColor rnd
                let vp = Position.getRandom rnd game.size
                let vx = game.vehicles |> Game.addVehicle id (HitBox.single vp) vc
                let ref = props (client ep)
                          |> spawn me $"client_{id}"
                ref <! UserEvent (cnn, UserJoined id)
                return! loop({area with users = id::area.users }) { game with vehicles = vx }
            | UserCommand (cnn, Leave id) ->
                let ux = area.users |> Area.remUser id
                let vx = game.vehicles |> Game.remVehicle id
                let ax = { area with users = ux }
                let gx = { game with vehicles = vx }
                let ref = select me $"client_{id}"
                ref <! UserEvent (cnn, UserLeft)
                return! if ux |> List.isEmpty
                    then awaiting(ax) gx
                    else loop(ax) gx
            | JoinBot ->
                let id = area.users |> Area.addUser
                let vc = Vehicle.getColor rnd
                let vp = Position.getRandom rnd game.size
                let vx = game.vehicles |> Game.addVehicle id (HitBox.single vp) vc
                let ref = props bot
                          |> spawn me $"client_{id}"
                ref <! UserEvent ("", UserJoined id)
                return! loop({area with users = id::area.users }) { game with vehicles = vx }
        }
        awaiting Area.empty (Game.empty (Size(50, 25)))

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