FROM mcr.microsoft.com/dotnet/aspnet:5.0 AS base
WORKDIR /app
EXPOSE 80

FROM mcr.microsoft.com/dotnet/sdk:5.0 AS build
WORKDIR /src
COPY ["Metan.Server/Metan.Server.fsproj", "Metan.Server/"]
RUN dotnet restore "Metan.Server/Metan.Server.fsproj"
COPY . .
WORKDIR "/src/Metan.Server"
RUN dotnet build "Metan.Server.fsproj" -c Release -o /app/build

FROM build AS publish
RUN dotnet publish "Metan.Server.fsproj" -c Release -o /app/publish

FROM base AS final
WORKDIR /app
COPY --from=publish /app/publish .
ENTRYPOINT ["dotnet", "Metan.Server.dll"]
