namespace Metan.Server

open System
open System.Threading.Tasks
open MBrace.FsPickler
open Microsoft.AspNetCore.SignalR
open Microsoft.Extensions.Hosting
open Akka.Actor
open Akkling
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
        this.AreaActor <! (dec msg) 
        Task.CompletedTask
         
type EventPublisher (hub:IHubContext<SignalRHub>) =
     
     let bs = BinarySerializer()
     let enc = Serialization.encode bs
     
     member this.AreaEventSent(evt: AreaEvent) =
         hub.Clients.All.SendAsync("AreaEventSent", enc evt)
         |> Async.AwaitTask
         |> Async.RunSynchronously
         
[<AutoOpen>]
module Actors =

    let rec client (ep:EventPublisher) (me:Actor<AreaEvent>) =
        let rec connecting() = actor {
            let! message = me.Receive()
            match message with
                | UserJoined(id) as evt ->
                    ep.AreaEventSent evt 
                    return! connected(id)
                | _ -> ignored()
            }
        and connected id = 
            actor {
                let! message = me.Receive()
                match message with
                | State _ as evt ->
                    ep.AreaEventSent evt 
                | UserLeft as evt ->
                    ep.AreaEventSent evt
                    return! stop()
                | _ -> ignored()
            }
        connecting()

    let rec area (ep:EventPublisher) (me:Actor<AreaCommand>)  =
        me.ScheduleRepeatedly
            TimeSpan.Zero (TimeSpan.FromSeconds 0.1) me.Self (AreaCommand Tick) |> ignore

        let rec awaiting area game = actor {
            let! command = me.Receive()
            match command with
            | Join as cmd ->
                me.Self <! cmd
                return! loop(area) game
            | _ ->
                return! awaiting(area) game
            }
        and loop area game = actor {
            let! command = me.Receive ()
            match command with
            | AreaCommand Tick ->
                let gx = Game.tick game (Tick :: area.commands)
                let childrenRef = select me "client_*"
                childrenRef <! State (game, gx)
                return! loop({ area with commands = [] }) gx
            | AreaCommand cmd ->
                return! loop({ area with commands = cmd::area.commands }) game
            | Join ->
                let id = area.users |> Area.addUser
                let vx = game.vehicles |> Game.addVehicle id game.size
                let childRef = spawn me $"client_{id}" (props (client ep))
                childRef <! UserJoined id
                return! loop({area with users = id::area.users }) { game with vehicles = vx }
            | Leave id ->
                let ux = area.users |> Area.remUser id
                let vx = game.vehicles |> Game.remVehicle id
                let ax = { area with users = ux }
                let gx = { game with vehicles = vx }
                let childRef = select me $"client_{id}"
                childRef <! UserLeft
                return! if ux |> Area.anyUser
                    then loop(ax) gx
                    else awaiting(ax) gx 
        }
        awaiting { users = []
                   commands = [] }
                 { bullets = []
                   vehicles = []
                   size = Size(50, 25) }

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