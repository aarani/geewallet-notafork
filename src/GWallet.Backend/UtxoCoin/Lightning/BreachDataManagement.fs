namespace GWallet.Backend.UtxoCoin.Lightning

open System.IO
open System

open NBitcoin
open DotNetLightning.Utils
open DotNetLightning.Channel
open Newtonsoft.Json

open GWallet.Backend
open GWallet.Backend.UtxoCoin
open GWallet.Backend.FSharpUtil
open GWallet.Backend.FSharpUtil.UwpHacks


type ChannelBreachData = 
    {
        ChannelId: ChannelIdentifier
        PenaltyTxs: List<string>
    }
    
    static member LightningSerializerSettings =
        let settings = JsonMarshalling.SerializerSettings
        let signatureConverter = NBitcoin.JsonConverters.SignatureJsonConverter ()

        settings.Converters.Add signatureConverter
        settings

    member internal self.PenaltyTxExists(commitmentNumber: CommitmentNumber) : bool =
        (self.PenaltyTxs |> List.tryItem(int32((UInt48.MaxValue - commitmentNumber.Index()).UInt64))).IsSome

    member internal self.GetPenaltyTx(commitmentNumber: CommitmentNumber) : Option<string> =
        self.PenaltyTxs |> List.tryItem(int32((UInt48.MaxValue - commitmentNumber.Index()).UInt64)) //overflow?

    member internal self.InsertRevokedCommitment 
                                        (perCommitmentSecret: PerCommitmentSecret)
                                        (commitments: Commitments)
                                        (localChannelPrivKeys: ChannelPrivKeys)
                                        (network: Network)
                                        (account: NormalUtxoAccount) 
                                            : Async<ChannelBreachData> = async {
        if self.PenaltyTxs.Length <> int32((UInt48.MaxValue - commitments.RemoteCommit.Index.Index()).UInt64) then
            failwith "Unexpected penalty trsansaction list length"

        let! punishmentTx = 
            ForceCloseTransaction.CreatePunishmentTx perCommitmentSecret
                                                     commitments
                                                     localChannelPrivKeys
                                                     network
                                                     account


        return { self with PenaltyTxs = self.PenaltyTxs @ [ punishmentTx.ToHex() ] }
    }

type internal BreachDataStore(account: NormalUtxoAccount) =
    static member BreachDataFilePrefix = "breach-"
    static member BreachDataFileEnding = ".json"

    member val Account = account
    member val Currency = (account :> IAccount).Currency

    member self.AccountDir: DirectoryInfo =
        Config.GetConfigDir self.Currency AccountKind.Normal

    member self.ChannelDir: DirectoryInfo =
        let subdirectory = SPrintF2 "%s-%s" (self.Account :> BaseAccount).AccountFile.Name Settings.ConfigDirName
        Path.Combine (self.AccountDir.FullName, subdirectory) |> DirectoryInfo

    member self.BreachDataFileName (channelId: ChannelIdentifier): string =
        Path.Combine(
            self.ChannelDir.FullName,
            SPrintF3
                "%s%s%s"
                BreachDataStore.BreachDataFilePrefix
                (channelId.ToString())
                BreachDataStore.BreachDataFileEnding
        )
    
    member internal self.LoadBreachData(channelId: ChannelIdentifier): ChannelBreachData =
        try 
            let fileName = self.BreachDataFileName channelId
            let json = File.ReadAllText fileName
            Marshalling.DeserializeCustom<ChannelBreachData> (
                json,
                ChannelBreachData.LightningSerializerSettings
            )
        with
        | :? FileNotFoundException | :? DirectoryNotFoundException ->
            {
                ChannelBreachData.ChannelId = channelId
                ChannelBreachData.PenaltyTxs = []
            }

    // For now all lightning incoming messages are handled within a single thread, we don't need a lock here.
    member internal self.SaveBreachData (serializedBreachData: ChannelBreachData) =
        let fileName = self.BreachDataFileName serializedBreachData.ChannelId
        let json =
            Marshalling.SerializeCustom(
                serializedBreachData,
                ChannelBreachData.LightningSerializerSettings
            )
        if not self.ChannelDir.Exists then
            self.ChannelDir.Create()
        File.WriteAllText(fileName, json)
