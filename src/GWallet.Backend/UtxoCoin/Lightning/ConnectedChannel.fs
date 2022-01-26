namespace GWallet.Backend.UtxoCoin.Lightning

open System
open System.Net

open NBitcoin
open DotNetLightning.Channel
open DotNetLightning.Utils
open DotNetLightning.Serialization.Msgs
open ResultUtils.Portability

open GWallet.Backend
open GWallet.Backend.FSharpUtil
open GWallet.Backend.FSharpUtil.UwpHacks
open GWallet.Backend.UtxoCoin


type internal ReestablishError =
    | RecvReestablish of RecvMsgError
    | PeerErrorResponse of PeerNode * PeerErrorMessage
    | ExpectedReestablishMsg of ILightningMsg
    | ExpectedReestablishOrFundingLockedMsg of ILightningMsg
    | LocalOutOfSync
    | RemoteOutOfSync
    | InvalidChannelId
    interface IErrorMsg with
        member self.Message =
            match self with
            | RecvReestablish err ->
                SPrintF1 "Error receiving channel_reestablish: %s" (err :> IErrorMsg).Message
            | PeerErrorResponse (_, err) ->
                SPrintF1 "Peer responded to our channel_reestablish with an error: %s" (err :> IErrorMsg).Message
            | ExpectedReestablishMsg msg ->
                SPrintF1 "Expected channel_reestablish, got %A" (msg.GetType())
            | ExpectedReestablishOrFundingLockedMsg msg ->
                SPrintF1 "Expected channel_reestablish or funding_locked, got %A" (msg.GetType())
            | LocalOutOfSync ->
                "We are out of sync"
            | RemoteOutOfSync ->
                "Remote party is out of sync"
            | InvalidChannelId ->
                "Peer was tried to reestablish the wrong channel"
        member self.ChannelBreakdown: bool =
            match self with
            | RecvReestablish recvMsgError ->
                (recvMsgError :> IErrorMsg).ChannelBreakdown
            | PeerErrorResponse _ -> true
            | ExpectedReestablishMsg _ -> false
            | ExpectedReestablishOrFundingLockedMsg _ -> false
            // If local is out of sync we can't force close or we might lose money
            | LocalOutOfSync -> false
            // If remote is out of sync we should help them with force closing the channel
            | RemoteOutOfSync -> true
            | InvalidChannelId -> false

    member internal self.PossibleBug =
        match self with
        | RecvReestablish err -> err.PossibleBug
        | PeerErrorResponse _
        | ExpectedReestablishMsg _
        | ExpectedReestablishOrFundingLockedMsg _ -> false
        | LocalOutOfSync -> false
        | RemoteOutOfSync -> false
        | InvalidChannelId -> false

type internal ReconnectError =
    | Connect of ConnectError
    | Reestablish of ReestablishError
    interface IErrorMsg with
        member self.Message =
            match self with
            | Connect err ->
                SPrintF1 "Error reconnecting to peer: %s" (err :> IErrorMsg).Message
            | Reestablish err ->
                SPrintF1 "Error reestablishing channel with connected peer: %s" (err :> IErrorMsg).Message
        member self.ChannelBreakdown: bool =
            match self with
            | Connect connectError ->
                (connectError :> IErrorMsg).ChannelBreakdown
            | Reestablish reestablishError ->
                (reestablishError :> IErrorMsg).ChannelBreakdown

    member internal self.PossibleBug =
        match self with
        | Connect err -> err.PossibleBug
        | Reestablish err -> err.PossibleBug

type internal ConnectedChannel =
    {
        PeerNode: PeerNode
        Channel: MonoHopUnidirectionalChannel
        Account: NormalUtxoAccount
        MinimumDepth: BlockHeightOffset32
        ChannelIndex: int
    }
    interface IDisposable with
        member self.Dispose() =
            (self.PeerNode :> IDisposable).Dispose()

    static member private LoadChannel (channelStore: ChannelStore)
                                      (nodeMasterPrivKey: NodeMasterPrivKey)
                                      (channelId: ChannelIdentifier)
                                          : Async<SerializedChannel * MonoHopUnidirectionalChannel> = async {
        let serializedChannel = channelStore.LoadChannel channelId
        Infrastructure.LogDebug <| SPrintF1 "loading channel for %s" (channelId.ToString())
        let! channel =
            MonoHopUnidirectionalChannel.Create
                channelStore.Account
                nodeMasterPrivKey
                serializedChannel.ChannelIndex
                serializedChannel.SavedChannelState
                serializedChannel.RemoteNextCommitInfo
                serializedChannel.NegotiatingState
                serializedChannel.Commitments
        return serializedChannel, channel
    }

    static member private Reestablish (peerNode: PeerNode)
                                      (channel: MonoHopUnidirectionalChannel)
                                          : Async<Result<PeerNode * MonoHopUnidirectionalChannel, ReestablishError>> = async {
        let channelId = channel.ChannelId
        let ourReestablishMsg = channel.Channel.CreateChannelReestablish()
        Infrastructure.LogDebug <| SPrintF1 "sending reestablish for %s" (channelId.ToString())
        let! peerNodeAfterReestablishSent = peerNode.SendMsg ourReestablishMsg
        Infrastructure.LogDebug <| SPrintF1 "receiving reestablish for %s" (channelId.ToString())
        let! reestablishRes = async {
            let! recvMsgRes = peerNodeAfterReestablishSent.RecvChannelMsg()
            match recvMsgRes with
            | Error (RecvMsg recvMsgError) -> return Error <| RecvReestablish recvMsgError
            | Error (ReceivedPeerErrorMessage (peerNodeAfterNextMsgReceived, errorMessage)) ->
                return Error <| PeerErrorResponse (peerNodeAfterNextMsgReceived, errorMessage)
            | Ok (peerNodeAfterNextMsgReceived, channelMsg) ->
                match channelMsg with
                | :? ChannelReestablishMsg as reestablishMsg ->
                    return Ok (peerNodeAfterNextMsgReceived, reestablishMsg)
                | :? FundingLockedMsg ->
                    let! recvMsgRes = peerNodeAfterNextMsgReceived.RecvChannelMsg()
                    match recvMsgRes with
                    | Error (RecvMsg recvMsgError) -> return Error <| RecvReestablish recvMsgError
                    | Error (ReceivedPeerErrorMessage (peerNodeAfterReestablishReceived, errorMessage)) ->
                        return Error <| PeerErrorResponse
                            (peerNodeAfterReestablishReceived, errorMessage)
                    | Ok (peerNodeAfterReestablishReceived, channelMsg) ->
                        match channelMsg with
                        | :? ChannelReestablishMsg as reestablishMsg ->
                            return Ok (peerNodeAfterReestablishReceived, reestablishMsg)
                        | msg ->
                            return Error <| ExpectedReestablishMsg msg
                | msg ->
                    return Error <| ExpectedReestablishOrFundingLockedMsg msg
        }
        match reestablishRes with
        | Error err -> return Error err
        | Ok (peerNodeAfterReestablishReceived, theirReestablishMsg) ->
            if theirReestablishMsg.ChannelId = channelId.DnlChannelId then
                let result = ChannelSyncing.checkSync channel.ChannelPrivKeys channel.Channel.SavedChannelState channel.Channel.RemoteNextCommitInfo theirReestablishMsg
                Console.WriteLine(sprintf "Sync result: %A" result)
                match result with
                | ChannelSyncing.SyncResult.Success retransmitList ->
                    let rec retransmit (retransmitList: list<ILightningMsg>) (peerNode: PeerNode) =
                        async {
                            match List.tryHead retransmitList with
                            | Some msg ->
                                let! newPeerNode = peerNode.SendMsg msg
                                Console.WriteLine(sprintf "retransmited msg %A" msg)
                                return! retransmit (retransmitList.Tail) newPeerNode
                            | None ->
                                return peerNode
                        }

                    let! peerNodeAfterRetransmit =  retransmit retransmitList peerNodeAfterReestablishReceived
                    //FIXME: fix localNextHtlcId and remoteNextHtlcId
                    //remove unsigned changes
                    let newChannel =
                        {
                             channel.Channel with
                                Commitments = 
                                {
                                    channel.Channel.Commitments with
                                        ProposedLocalChanges = List.empty
                                        ProposedRemoteChanges = List.empty
                                }
                        }
                    let! newNewChannel, peerNodeAfterIfRevokeReceived =
                        async {
                            match channel.Channel.RemoteNextCommitInfo with
                            | Some (Waiting _) ->
                                let! recvMsgRes = peerNodeAfterRetransmit.RecvChannelMsg()
                                match recvMsgRes with
                                | Error (RecvMsg e) -> return failwith (sprintf "%A" e)
                                | Error (ReceivedPeerErrorMessage (_, e)) ->
                                    return failwith (sprintf "%A" e)
                                | Ok (peerNodeAfterRevokeReceived, channelMsg) ->
                                    match channelMsg with
                                    | :? RevokeAndACKMsg as theirRevokeAndAckMsg ->
                                        let channelAferRevokeAndAckRes =
                                            newChannel.ApplyRevokeAndACK theirRevokeAndAckMsg
                                        match channelAferRevokeAndAckRes with
                                        | Ok channelAfterRevokeAndAck ->
                                            Console.WriteLine("Received revoke msg")
                                            return (channelAfterRevokeAndAck, peerNodeAfterRevokeReceived)
                                        | e -> return failwith (sprintf "%A" e)
                                    | _ -> return failwith ""
                            | _ ->
                                Console.WriteLine("Didn't need revoke msg")
                                return (newChannel, peerNodeAfterRetransmit)
                        }

                    // return LocalOutOfSync just so we don't breakdown anything
                    let newMonoHopChannel =
                        {
                            channel with
                                Channel = newNewChannel
                        }

                    return Ok (peerNodeAfterIfRevokeReceived, newMonoHopChannel)
                | ChannelSyncing.SyncResult.RemoteLate ->
                    return Error <| ReestablishError.RemoteOutOfSync
                | _ ->
                    return Error <| ReestablishError.LocalOutOfSync
            else
                return Error <| ReestablishError.InvalidChannelId
    }

    static member internal ConnectFromWallet (channelStore: ChannelStore)
                                             (nodeMasterPrivKey: NodeMasterPrivKey)
                                             (channelId: ChannelIdentifier)
                                                 : Async<Result<ConnectedChannel, ReconnectError>> = async {
        let! serializedChannel, channel =
            ConnectedChannel.LoadChannel channelStore nodeMasterPrivKey channelId
        let! connectRes =
            let nodeId = channel.RemoteNodeId
            let peerId = PeerId (serializedChannel.CounterpartyIP :> EndPoint)
            PeerNode.Connect nodeMasterPrivKey nodeId peerId
        match connectRes with
        | Error connectError -> return Error <| Connect connectError
        | Ok peerNode ->
            let! reestablishRes =
                ConnectedChannel.Reestablish peerNode channel
            match reestablishRes with
            | Error reestablishError -> return Error <| Reestablish reestablishError
            | Ok (peerNodeAfterReestablish, channelAfterReestablish) ->
                let minimumDepth = serializedChannel.MinDepth()
                let channelIndex = serializedChannel.ChannelIndex
                let connectedChannel = {
                    Account = channelStore.Account
                    Channel = channelAfterReestablish
                    PeerNode = peerNodeAfterReestablish
                    MinimumDepth = minimumDepth
                    ChannelIndex = channelIndex
                }
                return Ok connectedChannel
    }

    static member internal AcceptFromWallet (channelStore: ChannelStore)
                                            (transportListener: TransportListener)
                                            (channelId: ChannelIdentifier)
                                                : Async<Result<ConnectedChannel, ReconnectError>> = async {
        let! serializedChannel, channel =
            ConnectedChannel.LoadChannel channelStore transportListener.NodeMasterPrivKey channelId
        let! connectRes =
            let nodeId = channel.RemoteNodeId
            PeerNode.AcceptFromTransportListener
                transportListener
                nodeId
        match connectRes with
        | Error connectError -> return Error <| Connect connectError
        | Ok peerNode ->
            let! reestablishRes =
                ConnectedChannel.Reestablish peerNode channel
            match reestablishRes with
            | Error reestablishError -> return Error <| Reestablish reestablishError
            | Ok (peerNodeAfterReestablish, channelAfterReestablish) ->
                let minimumDepth = serializedChannel.MinDepth()
                let channelIndex = serializedChannel.ChannelIndex
                let connectedChannel = {
                    Account = channelStore.Account
                    Channel = channelAfterReestablish
                    PeerNode = peerNodeAfterReestablish
                    MinimumDepth = minimumDepth
                    ChannelIndex = channelIndex
                }
                return Ok connectedChannel
    }

    member self.SaveToWallet() =
        let channelStore = ChannelStore self.Account
        let serializedChannel : SerializedChannel = {
            ChannelIndex = self.ChannelIndex
            RemoteNextCommitInfo = self.Channel.Channel.RemoteNextCommitInfo
            SavedChannelState = self.Channel.Channel.SavedChannelState
            NegotiatingState = self.Channel.Channel.NegotiatingState
            Commitments = self.Channel.Channel.Commitments
            AccountFileName = self.Account.AccountFile.Name
            CounterpartyIP = self.PeerNode.PeerId.Value :?> IPEndPoint
            InitialRecoveryTransactionOpt = None
            LocalChannelPubKeys = self.Channel.ChannelPrivKeys.ToChannelPubKeys()
            RecoveryTxIdOpt = None
        }
        channelStore.SaveChannel serializedChannel

    member internal self.RemoteNodeId
        with get(): NodeId = self.Channel.RemoteNodeId

    member internal self.Network
        with get(): Network = self.Channel.Network

    member self.ChannelId
        with get(): ChannelIdentifier =
            self.Channel.ChannelId

    member self.FundingTxId
        with get(): TransactionIdentifier =
            self.Channel.FundingTxId

    member internal self.FundingScriptCoin
        with get(): ScriptCoin =
            self.Channel.FundingScriptCoin

    member self.SendError (err: string): Async<ConnectedChannel> = async {
        let! peerNode = self.PeerNode.SendError err (self.Channel.Channel.SavedChannelState.StaticChannelConfig.ChannelId() |> Some)
        return {
            self with
                PeerNode = peerNode
        }
    }

