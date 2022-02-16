﻿namespace GWallet.Backend.UtxoCoin

open System
open System.Net

open NOnion
open NOnion.Directory
open NOnion.Services

open GWallet.Backend


module internal TorOperations =
    let GetRandomTorFallbackDirectoryEndpoint() =
        match Caching.Instance.GetServers
            (ServerType.ProtocolServer ServerProtocol.Tor)
            |> Shuffler.Unsort
            |> Seq.tryHead with
        | Some server ->
            match server.ServerInfo.ConnectionType.Protocol with
            | Protocol.Tcp port ->
                IPEndPoint(IPAddress.Parse server.ServerInfo.NetworkPath, int32 port)
            | _ -> failwith "Invalid Tor directory. Tor directories must have an IP and port."
        | None ->
            failwith "Couldn't find any Tor server"

    let internal GetTorDirectory(): Async<TorDirectory> =
        async {
            return! FSharpUtil.Retry<TorDirectory, NOnionException>
                (fun _ -> TorDirectory.Bootstrap (GetRandomTorFallbackDirectoryEndpoint()))
                Config.TOR_CONNECTION_RETRY_COUNT
        }

    let internal StartTorServiceHost directory =
        async {
            return! FSharpUtil.Retry<TorServiceHost, NOnionException>
                (fun _ -> async { return TorServiceHost(directory, Config.TOR_CONNECTION_RETRY_COUNT) })
                Config.TOR_CONNECTION_RETRY_COUNT
        }

    let internal TorConnect directory introductionPointPublicInfo =
        async {
            return! FSharpUtil.Retry<TorServiceClient, NOnionException>
                (fun _ -> TorServiceClient.Connect directory introductionPointPublicInfo)
                Config.TOR_CONNECTION_RETRY_COUNT
        }
