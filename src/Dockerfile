FROM microsoft/dotnet:2.1-sdk AS build
WORKDIR /app

# copy csproj and restore as distinct layers
COPY LibTakedown/LibTakedown.fsproj LibTakedown/
COPY Takedown.Web/Takedown.Web.fsproj Takedown.Web/
WORKDIR /app/Takedown.Web
RUN dotnet restore

# copy everything else and build app
WORKDIR /app
COPY . ./
WORKDIR /app/Takedown.Web
RUN dotnet publish -c Release -o out

FROM microsoft/dotnet:2.1-aspnetcore-runtime AS runtime
WORKDIR /app
COPY --from=build /app/Takedown.Web/out .
ENTRYPOINT ["dotnet", "Takedown.Web.dll"]