FROM mcr.microsoft.com/dotnet/sdk:6.0
WORKDIR ./app
COPY Program.cs .
COPY dotnet.csproj .
ENTRYPOINT ["dotnet", "run", "--configuration", "Release"]
