namespace Metan.Server

open Akka.Actor
open Akkling
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection

type Startup() =

    member _.ConfigureServices(services: IServiceCollection) =
        let system = System.create "system" <| Configuration.defaultConfig()
        services
            .AddSingleton<ActorSystem>(system)
            .AddHostedService<ActorService>()
            .AddSingleton<EventPublisher>()
            .AddSignalR()
            |> ignore
        ()

    member _.Configure(app: IApplicationBuilder, env: IWebHostEnvironment) =
        app.UseRouting()
           .UseEndpoints(fun endpoints ->
                endpoints.MapGet("/", fun context ->
                    context.Response.WriteAsync("Metan Server is running"))
                |> ignore
                endpoints.MapHub<SignalRHub>("/server")
                |> ignore
            ) |> ignore