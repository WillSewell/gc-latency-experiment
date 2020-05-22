FROM mcr.microsoft.com/dotnet/core/sdk:3.1
WORKDIR ./app
COPY Program.cs .
COPY dotnet.csproj .
ENTRYPOINT ["dotnet", "run", "--configuration", "Release"]
