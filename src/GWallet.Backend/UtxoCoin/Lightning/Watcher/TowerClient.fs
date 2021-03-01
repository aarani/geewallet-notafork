﻿namespace GWallet.Backend.UtxoCoin.Lightning.Watcher

open System.Net.Sockets
open StreamJsonRpc
open GWallet.Backend.UtxoCoin
open DotNetLightning.Utils
open DotNetLightning.Channel
open NBitcoin
open GWallet.Backend.UtxoCoin.Lightning
open GWallet.Backend
open GeeTower.API

type internal TowerClient =
    { TowerHost: string
      TowerPort: int }

    static member Default =
        { 
            TowerHost = Config.DEFAULT_WATCHTOWER_HOST
            TowerPort = Config.DEFAULT_WATCHTOWER_PORT
        }
               
    member internal self.CreateAndSendPunishmentTx
        (perCommitmentSecret: PerCommitmentSecret)
        (commitments: Commitments)
        (localChannelPrivKeys: ChannelPrivKeys)
        (network: Network)
        (account: NormalUtxoAccount)
        (quietMode: bool)
        : Async<unit> =
        async {
            try
                let! rewardAdderss = self.GetRewardAddress (account :> IAccount).Currency
            
                let! punishmentTx =
                    ForceCloseTransaction.CreatePunishmentTx
                        perCommitmentSecret
                        commitments
                        localChannelPrivKeys
                        network
                        account
                        (rewardAdderss |> Some)

                let towerRequest =
                    {
                        AddPunishmentTxRequest.TransactionHex = punishmentTx.ToHex()
                        CommitmentTxHash = commitments.RemoteCommit.TxId.Value.ToString()
                    }

                do! self.AddPunishmentTx towerRequest
            with 
            | ex -> 
                if not quietMode then
                    raise <| FSharpUtil.ReRaise ex
        }


    member private self.AddPunishmentTx(request: AddPunishmentTxRequest): Async<unit> =
        async {
            use client = new TcpClient()

            do!
                client.ConnectAsync(self.TowerHost, self.TowerPort)
                |> Async.AwaitTask
            
            let mutable jsonRpc: JsonRpc = new JsonRpc(client.GetStream())
            jsonRpc.StartListening()

            return!
                jsonRpc.NotifyAsync("add_punishment_tx", request)
                |> Async.AwaitTask
        }

    member private self.GetRewardAddress(currency: Currency): Async<string> =
        async {
            use client = new TcpClient()

            do!
                client.ConnectAsync(self.TowerHost, self.TowerPort)
                |> Async.AwaitTask

            let mutable jsonRpc: JsonRpc = new JsonRpc(client.GetStream())
            jsonRpc.StartListening()

            let toTowerCurrency (currency: Currency) = 
                match currency with 
                | Currency.BTC ->TowerCurrency.Bitcoin
                | Currency.LTC -> TowerCurrency.Litecoin
                | _ -> failwith ("only btc and ltc are supported on tower")

            let! response = 
                jsonRpc.InvokeAsync<TowerAPIResponseOrError<GetRewardAddressResponse>>("get_reward_address", toTowerCurrency currency)
                |> Async.AwaitTask
            
            return  
                match response with
                | TowerAPIResponse (reward) -> reward.RewardAddress
                | TowerAPIError (e) -> failwith (sprintf "Tower returned an error: %s" (e.ToString()))
        }
