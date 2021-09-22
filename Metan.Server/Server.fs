namespace Metan.Server

open System
open System.Diagnostics
open System.Threading.Tasks
open MBrace.FsPickler
open Metan.Core
open Microsoft.AspNetCore.SignalR
open Microsoft.Extensions.Hosting
open Akka.Actor
open Akkling

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
            | AreaCommand _ as ac ->
                ac
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
                let childrenRef = select me "client_*"
                childrenRef <! State (game, gx)
                return! loop({ area with commands = [] }) gx
            | AreaCommand cmd ->
                return! loop({ area with commands = cmd::area.commands }) game
            | UserCommand (cnn, Join) ->
                let id = area.users |> Area.addUser
                let vc = Vehicle.getColor rnd
                let vp = Position.getRandom rnd game.size
                let vx = game.vehicles |> Game.addVehicle id (HitBox.single vp) vc
                let childRef = spawn me $"client_{id}" (props (client ep))
                childRef <! UserEvent (cnn, UserJoined id)
                return! loop({area with users = id::area.users }) { game with vehicles = vx }
            | UserCommand (cnn, Leave id) ->
                let ux = area.users |> Area.remUser id
                let vx = game.vehicles |> Game.remVehicle id
                let ax = { area with users = ux }
                let gx = { game with vehicles = vx }
                let childRef = select me $"client_{id}"
                childRef <! UserEvent (cnn, UserLeft)
                return! if ux |> List.isEmpty
                    then awaiting(ax) gx
                    else loop(ax) gx 
        }
        awaiting Area.empty  { bullets = []; vehicles = [
                { id = 2; hitBox = HitBox ((5, 5), (7, 6)); dmg = 1; color = ConsoleColor.Cyan; shape = Reflection [
                    { pos = (0, 0); kind = Body 9 }
                    { pos = (1, 0); kind = Body 9 }
                    { pos = (2, 0); kind = Body 9 }
                    { pos = (2, 1); kind = Body 9 } ]
                }
            ]; crates = [
                        { hitBox = HitBox ((10, 10), (10, 10)); bonus = ShapeBonus }
                        { hitBox = HitBox ((20, 20), (20, 20)); bonus = ShapeBonus }
                        { hitBox = HitBox ((30, 20), (30, 20)); bonus = ShapeBonus }
            ]; size = (Size(50, 25)); time = 0u }

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