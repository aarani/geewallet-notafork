﻿namespace GWallet.Backend.UtxoCoin.Lightning

open System.IO
open System.Net.Http
open System.Linq

open NBitcoin
open DotNetLightning.Serialization
open DotNetLightning.Serialization.Msgs
open DotNetLightning.Utils
open ResultUtils.Portability


module RapidGossipSyncer =
    
    let private RGSPrefix = [| 76uy; 68uy; 75uy; 1uy |]

    type internal CompactAnnouncment =
        {
            ChannelFeatures: Result<FeatureBits, FeatureError>
            ShortChannelId: ShortChannelId
            NodeId1: NodeId
            NodeId2: NodeId
        }

    type internal CompactChannelUpdate =
        {
            ShortChannelId: ShortChannelId
            CLTVExpiryDelta: uint16
            HtlcMinimumMSat: uint64
            FeeBaseMSat: uint32
            FeeProportionalMillionths: uint32
            HtlcMaximumMSat: uint64
        }

    type internal ChannelUpdates =
        {
            Forward: UnsignedChannelUpdateMsg option
            Backward: UnsignedChannelUpdateMsg option
        }
        with
            static member Empty = { Forward = None; Backward = None }
    
            member self.With(update: UnsignedChannelUpdateMsg) =
                let isForward = (update.ChannelFlags &&& 1uy) = 0uy
                if isForward then
                    match self.Forward with
                    | Some(prevUpd) when update.Timestamp < prevUpd.Timestamp -> self
                    | _ -> { self with Forward = Some(update) }
                else
                    match self.Backward with
                    | Some(prevUpd) when update.Timestamp < prevUpd.Timestamp -> self
                    | _ -> { self with Backward = Some(update) }

    /// see https://github.com/lightningdevkit/rust-lightning/tree/main/lightning-rapid-gossip-sync/#custom-channel-update
    module internal CustomChannelUpdateFlags =
        let Direction = 1uy
        let DisableCHannel = 2uy
        let HtlcMaximumMsat = 4uy
        let FeeProportionalMillionths = 8uy
        let FeeBaseMsat = 16uy
        let HtlcMinimumMsat = 32uy
        let CltvExpiryDelta = 64uy
        let IncrementalUpdate = 128uy

    let Sync () =
        async {
            use httpClient = new HttpClient()
            // Always do a full sync
            let! gossipData =
                httpClient.GetByteArrayAsync "https://rapidsync.lightningdevkit.org/snapshot/0"
                |> Async.AwaitTask

            use memStream = new MemoryStream(gossipData)
            use lightningReader = new LightningReaderStream(memStream)

            let prefix = Array.zeroCreate RGSPrefix.Length
            
            do! lightningReader.ReadAsync(prefix, 0, prefix.Length)
                |> Async.AwaitTask
                |> Async.Ignore

            if not (Enumerable.SequenceEqual (prefix, RGSPrefix)) then
                failwith "Invalid version prefix"

            let chainHash = lightningReader.ReadUInt256 true
            if chainHash <> Network.Main.GenesisHash then
                failwith "Invalid chain hash"

            let lastSeenTimestamp = lightningReader.ReadUInt32 false
            let backdatedTimestamp = lastSeenTimestamp - uint (24 * 3600 * 7)

            let nodeIdsCount = lightningReader.ReadUInt32 false
            let nodeIds = 
                Array.init
                    (int nodeIdsCount)
                    (fun _ -> lightningReader.ReadPubKey() |> NodeId)

            let announcementsCount = lightningReader.ReadUInt32 false

            let rec readAnnouncements (remainingCount: uint) (previousShortChannelId: uint64) (announcements: List<CompactAnnouncment>) =
                if remainingCount = 0u then
                    announcements
                else
                    let features = lightningReader.ReadWithLen () |> FeatureBits.TryCreate
                    let shortChannelId = previousShortChannelId + lightningReader.ReadBigSize ()
                    let nodeId1 = nodeIds.[lightningReader.ReadBigSize () |> int]
                    let nodeId2 = nodeIds.[lightningReader.ReadBigSize () |> int]

                    let compactAnn =
                        {
                            ChannelFeatures = features
                            ShortChannelId = shortChannelId |> ShortChannelId.FromUInt64
                            NodeId1 = nodeId1
                            NodeId2 = nodeId2
                        }

                    readAnnouncements (remainingCount - 1u) shortChannelId (compactAnn::announcements)

            let announcements = readAnnouncements announcementsCount 0UL List.Empty

            let updatesCount = lightningReader.ReadUInt32 false

            let defaultCltvExpiryDelta: uint16 = lightningReader.ReadUInt16 false
            let defaultHtlcMinimumMSat: uint64 = lightningReader.ReadUInt64 false
            let defaultFeeBaseMSat: uint32 = lightningReader.ReadUInt32 false
            let defaultFeeProportionalMillionths: uint32 = lightningReader.ReadUInt32 false
            let defaultHtlcMaximumMSat: uint64 = lightningReader.ReadUInt64 false

            let rec readUpdates (remainingCount: uint) (previousShortChannelId: uint64) (updates: Map<ShortChannelId, ChannelUpdates>) =
                if remainingCount = 0u then
                    updates
                else
                    let shortChannelId = previousShortChannelId + lightningReader.ReadBigSize ()
                    let customChannelFlag = lightningReader.ReadByte()
                    let standardChannelFlagMask = 0b11uy
                    let standardChannelFlag = customChannelFlag &&& standardChannelFlagMask

                    if customChannelFlag &&& CustomChannelUpdateFlags.IncrementalUpdate > 0uy then
                        failwith "We don't support increamental updates yet!"        

                    let cltvExpiryDelta =
                        if customChannelFlag &&& CustomChannelUpdateFlags.CltvExpiryDelta > 0uy then
                            lightningReader.ReadUInt16 false
                        else
                            defaultCltvExpiryDelta

                    let htlcMinimumMSat =
                        if customChannelFlag &&& CustomChannelUpdateFlags.HtlcMinimumMsat > 0uy then
                            lightningReader.ReadUInt64 false
                        else
                            defaultHtlcMinimumMSat

                    let feeBaseMSat =
                        if customChannelFlag &&& CustomChannelUpdateFlags.FeeBaseMsat > 0uy then
                            lightningReader.ReadUInt32 false
                        else
                            defaultFeeBaseMSat

                    let feeProportionalMillionths =
                        if customChannelFlag &&& CustomChannelUpdateFlags.FeeProportionalMillionths > 0uy then
                            lightningReader.ReadUInt32 false
                        else
                            defaultFeeProportionalMillionths
                    
                    let htlcMaximumMSat =
                        if customChannelFlag &&& CustomChannelUpdateFlags.HtlcMaximumMsat > 0uy then
                            lightningReader.ReadUInt64 false
                        else
                            defaultHtlcMaximumMSat

                    let structuredShortChannelId = shortChannelId |> ShortChannelId.FromUInt64

                    let channelUpdate =
                        {
                            UnsignedChannelUpdateMsg.ShortChannelId = structuredShortChannelId
                            Timestamp = backdatedTimestamp
                            ChainHash = Network.Main.GenesisHash
                            ChannelFlags = standardChannelFlag
                            MessageFlags = 1uy
                            CLTVExpiryDelta = cltvExpiryDelta |> BlockHeightOffset16
                            HTLCMinimumMSat = htlcMinimumMSat |> LNMoney.MilliSatoshis
                            FeeBaseMSat = feeBaseMSat |> LNMoney.MilliSatoshis
                            FeeProportionalMillionths = feeProportionalMillionths 
                            HTLCMaximumMSat = htlcMaximumMSat |> LNMoney.MilliSatoshis |> Some
                        }

                    let newUpdates =
                        let oldValue =
                            match updates |> Map.tryFind structuredShortChannelId with
                            | Some(updates) -> updates
                            | None -> ChannelUpdates.Empty
                        updates |> Map.add structuredShortChannelId (oldValue.With channelUpdate)

                    readUpdates (remainingCount - 1u) shortChannelId newUpdates

            let updates = readUpdates updatesCount 0UL Map.empty

            let _ = updates, announcements

            return ()
        }

