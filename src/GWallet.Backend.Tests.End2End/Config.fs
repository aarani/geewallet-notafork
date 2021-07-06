﻿namespace GWallet.Backend.Tests.End2End

open System
open System.Diagnostics
open System.IO
open System.Net
open System.Security

open NBitcoin

open GWallet.Backend
open GWallet.Backend.UtxoCoin.Lightning
open GWallet.Backend.FSharpUtil.UwpHacks

open FSharpUtil

[<RequireQualifiedAccess>]
module Config =

    let CredentialsFilePath = "../../../WslCredentials.dat"

    let CredentialsUnsecuredOpt =
        if Environment.OSVersion.Platform <> PlatformID.Unix then
            if File.Exists CredentialsFilePath then
                let credentialsText = File.ReadAllText CredentialsFilePath
                match credentialsText.Split ([|Environment.NewLine|], StringSplitOptions.None) with
                | [|userName; password|] -> Some (userName, password)
                | _ -> None
            else None
        else None

    let CredentialsSecuredOpt =
        match CredentialsUnsecuredOpt with
        | Some (userName, password) ->
            let passwordSecure = new SecureString ()
            for c in password do passwordSecure.AppendChar c
            Some (userName, passwordSecure)
        | None -> None

    let WebPort = "80"
    let HttpPort = "8080"
    let GlobalIP = "0.0.0.0"
    let LocalHostIP = "127.0.0.1"
    let LocalHost2IP = "127.0.0.2" // TODO: see if we can give this a better name.

    let ServerHostIP =
        if Environment.OSVersion.Platform <> PlatformID.Unix then
            GlobalIP
        else 
            LocalHostIP

    let ServerHost2IP = // TODO: see if we can give this a better name.
        if Environment.OSVersion.Platform <> PlatformID.Unix then
            let (userName, password) = UnwrapOption CredentialsSecuredOpt "Missing WSL credentials."
            let procStart =
                ProcessStartInfo (
                    UseShellExecute = false,
                    FileName = "wsl.exe",
                    Arguments = "hostname -I",
                    RedirectStandardOutput = true,
                    UserName = userName,
                    Password = password)
            use proc = new Process (StartInfo = procStart)
            if proc.Start () then
                let mutable ipOpt = None
                while not proc.StandardOutput.EndOfStream do
                    let line = proc.StandardOutput.ReadLine().Trim()
                    ipOpt <- Some line
                Option.defaultValue LocalHost2IP ipOpt
            else LocalHost2IP
        else LocalHost2IP

    let BitcoindRpcIP = LocalHostIP
    let BitcoindRpcPort = "18554"
    let BitcoindRpcAddress = BitcoindRpcIP + ":" + BitcoindRpcPort
    let BitcoindRpcAllowIP = LocalHostIP
    let BitcoindZeromqPublishRawBlockAddress = "127.0.0.1:28332"
    let BitcoindZeromqPublishRawTxAddress = "127.0.0.1:28333"

    let ElectrumIP = if Environment.OSVersion.Platform <> PlatformID.Unix then "0.0.0.0" else "[::1]"
    let ElectrumPort = "50001"
    let ElectrumRpcAddress = ElectrumIP + ":" + ElectrumPort

    let LndListenIP = ServerHost2IP
    let LndListenPort = "9735"
    let LndListenAddress = LndListenIP + ":" + LndListenPort
    let LndRestListenIP = ServerHost2IP
    let LndRestListenPort = HttpPort
    let LndRestListenAddress = LndRestListenIP + ":" + LndRestListenPort

    let LightningIP = ServerHostIP
    let LightningPort = "9735"
    let LightningAddress = ServerHostIP + ":" + LightningPort

    // NOTE: this key needs to be hard-coded, as opposed to randomly
    // generated, since it is used in two separate processes and must be
    // the same in each process.
    let FundeeAccountsPrivateKey =
        new Key (uint256.Parse("9d1ee30acb68716ed5f4e25b3c052c6078f1813f45d33a47e46615bfd05fa6fe").ToBytes())

    let FundeeLightningIPEndpoint =
        IPEndPoint (IPAddress.Parse LightningIP, Int32.Parse "9734")

    let FundeeNodeEndpoint =
        let extKey = FundeeAccountsPrivateKey.ToBytes() |> ExtKey
        let fundeeNodePubKey = extKey.PrivateKey.PubKey
        NodeEndPoint.Parse
            Currency.BTC
            (SPrintF3
                "%s@%s:%d"
                (fundeeNodePubKey.ToHex())
                (FundeeLightningIPEndpoint.Address.ToString())
                FundeeLightningIPEndpoint.Port
            )
        