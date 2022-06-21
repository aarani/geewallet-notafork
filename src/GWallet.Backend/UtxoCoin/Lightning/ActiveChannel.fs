namespace GWallet.Backend.UtxoCoin.Lightning

open System

open NBitcoin
open DotNetLightning.Crypto
open DotNetLightning.Channel
open DotNetLightning.Utils
open DotNetLightning.Serialization
open DotNetLightning.Serialization.Msgs
open DotNetLightning.Payment
open ResultUtils.Portability

open GWallet.Backend
open GWallet.Backend.FSharpUtil
open GWallet.Backend.FSharpUtil.UwpHacks
open GWallet.Backend.UtxoCoin
open GWallet.Backend.UtxoCoin.Lightning.Watcher

type HtlcSettleFailReason =
    | UnknownPaymentHash
    | IncorrectPaymentAmount
    | RequiredChannelFeatureMissing
    | BadFinalPayload

type HTLCSettleStatus =
    | Success
    | Fail
    | NotSettled

type internal LockFundingError =
    | FundingNotConfirmed of FundedChannel * BlockHeightOffset32
    | FundingOnChainLocationUnknown of FundedChannel
    | RecvFundingLocked of RecvMsgError
    | FundingLockedPeerErrorResponse of BrokenChannel * PeerErrorMessage
    | ExpectedFundingLocked of ILightningMsg
    | InvalidFundingLocked of BrokenChannel * ChannelError
    interface IErrorMsg with
        member self.Message =
            match self with
            | FundingNotConfirmed(_, remainingConfirmations) ->
                SPrintF1
                    "Funding not yet confirmed on-chain. %i more confirmations required"
                    remainingConfirmations.Value
            | FundingOnChainLocationUnknown _ ->
                "Funding appears to be confirmed but its on-chain location has not been indexed yet"
            | RecvFundingLocked err ->
                SPrintF1 "Error receiving funding locked: %s" (err :> IErrorMsg).Message
            | FundingLockedPeerErrorResponse (_, err) ->
                SPrintF1 "Peer responded to our funding_locked with an error: %s" (err :> IErrorMsg).Message
            | ExpectedFundingLocked msg ->
                SPrintF1 "Expected funding_locked message, got %A" (msg.GetType())
            | InvalidFundingLocked (_, err) ->
                SPrintF1 "Invalid funding_locked message: %s" err.Message
        member self.ChannelBreakdown: bool =
            match self with
            | FundingNotConfirmed _ -> false
            | FundingOnChainLocationUnknown _ -> false
            | RecvFundingLocked recvMsgError -> (recvMsgError :> IErrorMsg).ChannelBreakdown
            | FundingLockedPeerErrorResponse _ -> true
            | ExpectedFundingLocked _ -> false
            | InvalidFundingLocked _ -> true

    member internal self.PossibleBug =
        match self with
        | RecvFundingLocked err -> err.PossibleBug
        | FundingNotConfirmed _
        | FundingOnChainLocationUnknown _
        | FundingLockedPeerErrorResponse _
        | ExpectedFundingLocked _
        | InvalidFundingLocked _ -> false

type internal ReconnectActiveChannelError =
    | Reconnect of ReconnectError
    | LockFunding of LockFundingError
    interface IErrorMsg with
        member self.Message =
            match self with
            | Reconnect err ->
                SPrintF1 "Error reconnecting: %s" (err :> IErrorMsg).Message
            | LockFunding err ->
                SPrintF1 "Error locking funding: %s" (err :> IErrorMsg).Message
        member self.ChannelBreakdown: bool =
            match self with
            | Reconnect reconnectError -> (reconnectError :> IErrorMsg).ChannelBreakdown
            | LockFunding lockFundingError -> (lockFundingError :> IErrorMsg).ChannelBreakdown

    member internal self.PossibleBug =
        match self with
        | Reconnect err -> err.PossibleBug
        | LockFunding err -> err.PossibleBug

type internal SendCommitError =
    | RecvRevokeAndAck of RecvMsgError
    | CommitmentSignedPeerErrorResponse of BrokenChannel * PeerErrorMessage
    | ExpectedRevokeAndAck of ILightningMsg
    | InvalidRevokeAndAck of BrokenChannel * ChannelError
    interface IErrorMsg with
        member self.Message =
            match self with
            | RecvRevokeAndAck err ->
                SPrintF1 "Error receiving revoke_and_ack: %s" (err :> IErrorMsg).Message
            | CommitmentSignedPeerErrorResponse (_, err) ->
                SPrintF1 "Peer responded to our commitment_signed with an error message: %s" (err :> IErrorMsg).Message
            | ExpectedRevokeAndAck msg ->
                SPrintF1 "Expected revoke_and_ack, got %A" (msg.GetType())
            | InvalidRevokeAndAck (_, err) ->
                SPrintF1 "Invalid revoke_and_ack: %s" err.Message
        member self.ChannelBreakdown: bool =
            match self with
            | RecvRevokeAndAck recvMsgError -> (recvMsgError :> IErrorMsg).ChannelBreakdown
            | CommitmentSignedPeerErrorResponse _ -> true
            | ExpectedRevokeAndAck _ -> false
            | InvalidRevokeAndAck _ -> true

    member internal self.PossibleBug =
        match self with
        | RecvRevokeAndAck err -> err.PossibleBug
        | CommitmentSignedPeerErrorResponse _
        | ExpectedRevokeAndAck _
        | InvalidRevokeAndAck _ -> false

type internal RecvCommitError =
    | RecvCommitmentSigned of RecvMsgError
    | PeerErrorMessageInsteadOfCommitmentSigned of BrokenChannel * PeerErrorMessage
    | ExpectedCommitmentSigned of ILightningMsg
    | InvalidCommitmentSigned of BrokenChannel * ChannelError
    interface IErrorMsg with
        member self.Message =
            match self with
            | RecvCommitmentSigned err ->
                SPrintF1 "Error receiving commitment_signed: %s" (err :> IErrorMsg).Message
            | PeerErrorMessageInsteadOfCommitmentSigned (_, err) ->
                SPrintF1 "Peer sent us an error message instead of commitment_signed: %s" (err :> IErrorMsg).Message
            | ExpectedCommitmentSigned msg ->
                SPrintF1 "Expected commitment_signed, got %A" (msg.GetType())
            | InvalidCommitmentSigned (_, err) ->
                SPrintF1 "Invalid commitment signed: %s" err.Message
        member self.ChannelBreakdown: bool =
            match self with
            | RecvCommitmentSigned recvMsgError -> (recvMsgError :> IErrorMsg).ChannelBreakdown
            | PeerErrorMessageInsteadOfCommitmentSigned _
            | InvalidCommitmentSigned _ -> true
            | ExpectedCommitmentSigned _ -> false

    member internal self.PossibleBug =
        match self with
        | RecvCommitmentSigned err -> err.PossibleBug
        | PeerErrorMessageInsteadOfCommitmentSigned _
        | ExpectedCommitmentSigned _
        | InvalidCommitmentSigned _ -> false

type internal RecvFulfillOrFailError =
    | RecvFulfill of RecvMsgError
    | PeerErrorMessageInsteadOfHtlcFulfill of BrokenChannel * PeerErrorMessage
    | ExpectedHtlcFulfillOrFail of ILightningMsg
    | InvalidCHtlcFulfillOrFail of BrokenChannel * ChannelError
    | SendCommit of SendCommitError
    | RecvCommit of RecvCommitError
    interface IErrorMsg with
        member self.Message =
            match self with
            | RecvFulfill err ->
                SPrintF1 "Error receiving htlc fulfill: %s" (err :> IErrorMsg).Message
            | PeerErrorMessageInsteadOfHtlcFulfill (_, err) ->
                SPrintF1 "Peer sent us an error message instead of htlc fulfill: %s" (err :> IErrorMsg).Message
            | ExpectedHtlcFulfillOrFail msg ->
                SPrintF1 "Expected htlc fulfill, got %A" (msg.GetType())
            | InvalidCHtlcFulfillOrFail (_, err) ->
                SPrintF1 "Invalid htlc fulfill: %s" err.Message
            | SendCommit err ->
                SPrintF1 "Error sending commitment: %s" (err :> IErrorMsg).Message
            | RecvCommit err ->
                SPrintF1 "Error receiving commitment: %s" (err :> IErrorMsg).Message
        member self.ChannelBreakdown: bool =
            match self with
            | RecvFulfill recvMsgError -> (recvMsgError :> IErrorMsg).ChannelBreakdown
            | PeerErrorMessageInsteadOfHtlcFulfill _
            | ExpectedHtlcFulfillOrFail _ -> true
            | InvalidCHtlcFulfillOrFail _ -> false
            | SendCommit sendCommitError ->
                (sendCommitError :> IErrorMsg).ChannelBreakdown
            | RecvCommit recvCommitError ->
                (recvCommitError :> IErrorMsg).ChannelBreakdown
    
    member internal self.PossibleBug =
        match self with
        | RecvFulfill err -> err.PossibleBug
        | PeerErrorMessageInsteadOfHtlcFulfill _
        | ExpectedHtlcFulfillOrFail _
        | InvalidCHtlcFulfillOrFail _ -> false
        | SendCommit err -> err.PossibleBug
        | RecvCommit err -> err.PossibleBug

type internal SendMonoHopPaymentError =
    | InvalidMonoHopPayment of ActiveChannel * InvalidMonoHopUnidirectionalPaymentError
    | SendCommit of SendCommitError
    | RecvCommit of RecvCommitError
    interface IErrorMsg with
        member self.Message =
            match self with
            | InvalidMonoHopPayment (_, err) ->
                SPrintF1 "Invalid monohop payment: %s" err.Message
            | SendCommit err ->
                SPrintF1 "Error sending commitment: %s" (err :> IErrorMsg).Message
            | RecvCommit err ->
                SPrintF1 "Error receiving commitment: %s" (err :> IErrorMsg).Message
        member self.ChannelBreakdown: bool =
            match self with
            | InvalidMonoHopPayment _ -> false
            | SendCommit sendCommitError ->
                (sendCommitError :> IErrorMsg).ChannelBreakdown
            | RecvCommit recvCommitError ->
                (recvCommitError :> IErrorMsg).ChannelBreakdown

    member internal self.PossibleBug =
        match self with
        | InvalidMonoHopPayment _ -> false
        | SendCommit err -> err.PossibleBug
        | RecvCommit err -> err.PossibleBug

and internal SendHtlcPaymentError =
    | NoMultiHopHtlcSupport
    | InvalidHtlcPayment of ActiveChannel * InvalidUpdateAddHTLCError
    | SendCommit of SendCommitError
    | RecvCommit of RecvCommitError
    | RecvFulfillOrFail of RecvFulfillOrFailError
    | ReceivedHtlcFail of ActiveChannel
    interface IErrorMsg with
        member self.Message =
            match self with
            | NoMultiHopHtlcSupport -> "Multihop htlc payments are unsupported"
            | InvalidHtlcPayment (_, err) ->
                SPrintF1 "Invalid htlc payment: %s" err.Message
            | SendCommit err ->
                SPrintF1 "Error sending commitment: %s" (err :> IErrorMsg).Message
            | RecvCommit err ->
                SPrintF1 "Error receiving commitment: %s" (err :> IErrorMsg).Message
            | RecvFulfillOrFail err ->
                SPrintF1 "Error receiving fulfill or fail: %s" (err :> IErrorMsg).Message
            | ReceivedHtlcFail _ -> "Error received htlc fail"
        member self.ChannelBreakdown: bool =
            match self with
            | NoMultiHopHtlcSupport _ -> false
            | InvalidHtlcPayment _ -> false
            | SendCommit sendCommitError ->
                (sendCommitError :> IErrorMsg).ChannelBreakdown
            | RecvCommit recvCommitError ->
                (recvCommitError :> IErrorMsg).ChannelBreakdown
            | RecvFulfillOrFail recvFulfillOrFailError ->
                (recvFulfillOrFailError :> IErrorMsg).ChannelBreakdown
            | ReceivedHtlcFail _ -> false

    member internal self.PossibleBug =
        match self with
        | NoMultiHopHtlcSupport _ -> false
        | InvalidHtlcPayment _ -> false
        | SendCommit err -> err.PossibleBug
        | RecvCommit err -> err.PossibleBug
        | RecvFulfillOrFail err -> err.PossibleBug
        | ReceivedHtlcFail _ -> false

and internal RecvMonoHopPaymentError =
    | RecvMonoHopPayment of RecvMsgError
    | PeerErrorMessageInsteadOfMonoHopPayment of BrokenChannel * PeerErrorMessage
    | InvalidMonoHopPayment of BrokenChannel * ChannelError
    | ExpectedMonoHopPayment of ILightningMsg
    | RecvCommit of RecvCommitError
    | SendCommit of SendCommitError
    interface IErrorMsg with
        member self.Message =
            match self with
            | RecvMonoHopPayment err ->
                SPrintF1 "Error receiving monohop payment message: %s" (err :> IErrorMsg).Message
            | PeerErrorMessageInsteadOfMonoHopPayment (_, err) ->
                SPrintF1 "Peer sent us an error message instead of a monohop payment: %s" (err :> IErrorMsg).Message
            | InvalidMonoHopPayment (_, err) ->
                SPrintF1 "Invalid monohop payment message: %s" err.Message
            | ExpectedMonoHopPayment msg ->
                SPrintF1 "Expected monohop payment msg, got %A" (msg.GetType())
            | RecvCommit err ->
                SPrintF1 "Error receiving commitment: %s" (err :> IErrorMsg).Message
            | SendCommit err ->
                SPrintF1 "Error sending commitment: %s" (err :> IErrorMsg).Message
        member self.ChannelBreakdown: bool =
            match self with
            | RecvMonoHopPayment recvMsgError ->
                (recvMsgError :> IErrorMsg).ChannelBreakdown
            | PeerErrorMessageInsteadOfMonoHopPayment _ -> true
            | InvalidMonoHopPayment _ -> true
            | ExpectedMonoHopPayment _ -> false
            | RecvCommit recvCommitError ->
                (recvCommitError :> IErrorMsg).ChannelBreakdown
            | SendCommit sendCommitError ->
                (sendCommitError :> IErrorMsg).ChannelBreakdown

    member internal self.PossibleBug =
        match self with
        | RecvMonoHopPayment err -> err.PossibleBug
        | RecvCommit err -> err.PossibleBug
        | SendCommit err -> err.PossibleBug
        | PeerErrorMessageInsteadOfMonoHopPayment _
        | InvalidMonoHopPayment _
        | ExpectedMonoHopPayment _ -> false

and internal RecvHtlcPaymentError =
    | RecvHtlcPayment of RecvMsgError
    | PeerErrorMessageInsteadOfHtlcPayment of BrokenChannel * PeerErrorMessage
    | InvalidHtlcPayment of BrokenChannel * ChannelError
    | ExpectedHtlcPayment of ILightningMsg
    | RecvCommit of RecvCommitError
    | SendCommit of SendCommitError
    | SettleFailed of BrokenChannel * ChannelError
    interface IErrorMsg with
        member self.Message =
            match self with
            | RecvHtlcPayment err ->
                SPrintF1 "Error receiving htlc payment message: %s" (err :> IErrorMsg).Message
            | PeerErrorMessageInsteadOfHtlcPayment (_, err) ->
                SPrintF1 "Peer sent us an error message instead of a htlc payment: %s" (err :> IErrorMsg).Message
            | InvalidHtlcPayment (_, err) ->
                SPrintF1 "Invalid htlc payment msg: %s" err.Message
            | SettleFailed (_, err) ->
                SPrintF1 "Could not settle htlc payment: %s" err.Message
            | ExpectedHtlcPayment msg ->
                SPrintF1 "Expected htlc payment msg, got %A" (msg.GetType())
            | RecvCommit err ->
                SPrintF1 "Error receiving commitment: %s" (err :> IErrorMsg).Message
            | SendCommit err ->
                SPrintF1 "Error sending commitment: %s" (err :> IErrorMsg).Message
        member self.ChannelBreakdown: bool =
            match self with
            | RecvHtlcPayment recvMsgError ->
                (recvMsgError :> IErrorMsg).ChannelBreakdown
            | PeerErrorMessageInsteadOfHtlcPayment _ -> true
            | InvalidHtlcPayment _ -> true
            | SettleFailed _ -> true
            | ExpectedHtlcPayment _ -> false
            | RecvCommit recvCommitError ->
                (recvCommitError :> IErrorMsg).ChannelBreakdown
            | SendCommit sendCommitError ->
                (sendCommitError :> IErrorMsg).ChannelBreakdown

    member internal self.PossibleBug =
        match self with
        | RecvHtlcPayment err -> err.PossibleBug
        | RecvCommit err -> err.PossibleBug
        | SendCommit err -> err.PossibleBug
        | PeerErrorMessageInsteadOfHtlcPayment _
        | SettleFailed _ -> true
        | InvalidHtlcPayment _
        | ExpectedHtlcPayment _ -> false

and internal AcceptUpdateFeeError =
    | RecvUpdateFee of RecvMsgError
    | PeerErrorMessageInsteadOfUpdateFee of BrokenChannel * PeerErrorMessage
    | ExpectedUpdateFee of ILightningMsg
    | InvalidUpdateFee of BrokenChannel * ChannelError
    | RecvCommit of RecvCommitError
    | SendCommit of SendCommitError
    interface IErrorMsg with
        member self.ChannelBreakdown: bool =
            (self :> IErrorMsg).ChannelBreakdown
        member self.Message =
            match self with
            | RecvUpdateFee err ->
                SPrintF1 "Error receiving update fee message: %s" (err :> IErrorMsg).Message
            | PeerErrorMessageInsteadOfUpdateFee (_, err) ->
                SPrintF1 "Peer sent us an error message instead of an update fee message: %s" (err :> IErrorMsg).Message
            | ExpectedUpdateFee msg ->
                SPrintF1 "Expected update fee msg, got %A" (msg.GetType())
            | InvalidUpdateFee (_, err) ->
                SPrintF1 "Invalid update fee message: %s" err.Message
            | RecvCommit err ->
                SPrintF1 "Error receiving commitment: %s" (err :> IErrorMsg).Message
            | SendCommit err ->
                SPrintF1 "Error sending commitment: %s" (err :> IErrorMsg).Message

and internal UpdateFeeError =
    | SendCommit of SendCommitError
    | RecvCommit of RecvCommitError
    interface IErrorMsg with
        member self.ChannelBreakdown: bool =
            (self :> IErrorMsg).ChannelBreakdown
        member self.Message =
            match self with
            | SendCommit err ->
                SPrintF1 "Error sending commitment: %s" (err :> IErrorMsg).Message
            | RecvCommit err ->
                SPrintF1 "Error receiving commitment: %s" (err :> IErrorMsg).Message

and internal ActiveChannel =
    {
        ConnectedChannel: ConnectedChannel
    }
    interface IDisposable with
        member self.Dispose() =
            (self.ConnectedChannel :> IDisposable).Dispose()

    static member private LockFunding (fundedChannel: FundedChannel)
                                      (confirmationCount: BlockHeightOffset32)
                                      (absoluteBlockHeight: BlockHeight)
                                      (txIndexInBlock: TxIndexInBlock)
                                          : Async<Result<ActiveChannel, LockFundingError>> = async {
        let theirFundingLockedMsgOpt = fundedChannel.TheirFundingLockedMsgOpt
        if confirmationCount < fundedChannel.ConnectedChannel.MinimumDepth then
            failwith
                "LockFunding called when required confirmation depth has not been reached"
        let connectedChannel = fundedChannel.ConnectedChannel
        let peerNode = connectedChannel.PeerNode
        let channel = connectedChannel.Channel
        let ourFundingLockedMsgAndChannelRes =
            channel.Channel.ApplyFundingConfirmedOnBC
                absoluteBlockHeight
                txIndexInBlock
                confirmationCount
        let channelAfterFundingConfirmed, ourFundingLockedMsg = UnwrapResult ourFundingLockedMsgAndChannelRes "DNL error creating funding_locked msg"

        let! peerNodeAfterFundingLockedSent = peerNode.SendMsg ourFundingLockedMsg
        let! theirFundingLockedMsgRes = async {
            match theirFundingLockedMsgOpt with
            | Some theirFundingLockedMsg -> return Ok (peerNodeAfterFundingLockedSent, theirFundingLockedMsg)
            | None ->
                let! recvChannelMsgRes = peerNodeAfterFundingLockedSent.RecvChannelMsg()
                match recvChannelMsgRes with
                | Error (RecvMsg recvMsgError) -> return Error <| RecvFundingLocked recvMsgError
                | Error (ReceivedPeerErrorMessage (peerNodeAfterFundingLockedReceived, errorMessage)) ->
                    let connectedChannelAfterError =
                        {
                            connectedChannel with
                                PeerNode = peerNodeAfterFundingLockedReceived
                                Channel =
                                    {
                                        Channel = channelAfterFundingConfirmed
                                    }
                        }
                    let brokenChannel = { BrokenChannel.ConnectedChannel = connectedChannelAfterError }
                    return Error <| FundingLockedPeerErrorResponse
                        (brokenChannel, errorMessage)
                | Ok (peerNodeAfterFundingLockedReceived, channelMsg) ->
                    match channelMsg with
                    | :? FundingLockedMsg as fundingLockedMsg ->
                        return Ok (peerNodeAfterFundingLockedReceived, fundingLockedMsg)
                    | _ -> return Error <| ExpectedFundingLocked channelMsg
        }

        match theirFundingLockedMsgRes with
        | Error err -> return Error err
        | Ok (peerNodeAfterFundingLockedReceived, theirFundingLockedMsg) ->
            let channelAfterFundingLockedRes =
                channelAfterFundingConfirmed.ApplyFundingLocked
                    theirFundingLockedMsg

            match channelAfterFundingLockedRes with
            | Error err ->
                let connectedChannelAfterError =
                    {
                        connectedChannel with
                            PeerNode = peerNodeAfterFundingLockedReceived
                            Channel =
                                {
                                    Channel = channelAfterFundingConfirmed
                                }
                    }

                let! connectedChannelAfterErrorSent = connectedChannelAfterError.SendError err.Message
                let brokenChannel = { BrokenChannel.ConnectedChannel = connectedChannelAfterErrorSent }
                return Error <| InvalidFundingLocked
                    (brokenChannel, err)
            | Ok channelAfterFundingLocked ->
                let connectedChannelAfterFundingLocked =
                    {
                        connectedChannel with
                            PeerNode = peerNodeAfterFundingLockedReceived
                            Channel =
                                {
                                    Channel = channelAfterFundingLocked
                                }
                    }
                connectedChannelAfterFundingLocked.SaveToWallet()
                let activeChannel: ActiveChannel = {
                    ConnectedChannel = connectedChannelAfterFundingLocked
                }
                return Ok activeChannel
    }

    static member private CheckFundingConfirmed (fundedChannel: FundedChannel)
                                                   : Async<Result<ActiveChannel, LockFundingError>> = async {
        let! confirmationCount = fundedChannel.GetConfirmations()
        if confirmationCount < fundedChannel.MinimumDepth then
            let remainingConfirmations = fundedChannel.MinimumDepth - confirmationCount
            return Error <| FundingNotConfirmed(fundedChannel, remainingConfirmations)
        else
            let! locationOnChainOpt = fundedChannel.GetLocationOnChain()
            match locationOnChainOpt with
            | None ->
                return Error <| FundingOnChainLocationUnknown fundedChannel
            | Some (absoluteBlockHeight, txIndexInBlock) ->
                return!
                    ActiveChannel.LockFunding
                        fundedChannel
                        confirmationCount
                        absoluteBlockHeight
                        txIndexInBlock
    }

    static member private ConfirmFundingLocked (connectedChannel: ConnectedChannel)
                                                   : Async<Result<ActiveChannel, LockFundingError>> = async {
        let channel = connectedChannel.Channel
        let nextLocalCommitment = channel.Channel.SavedChannelState.LocalCommit.Index
        let nextRemoteCommitment = channel.Channel.SavedChannelState.RemoteCommit.Index
        if nextLocalCommitment = CommitmentNumber.FirstCommitment &&
           nextRemoteCommitment = CommitmentNumber.FirstCommitment then
            let fundedChannel = {
                FundedChannel.ConnectedChannel = connectedChannel
                TheirFundingLockedMsgOpt =
                    match channel.Channel.RemoteNextCommitInfo with
                    | None -> None
                    | Some remoteNextCommitInfo ->
                        let msg : FundingLockedMsg =
                            {
                                ChannelId = channel.ChannelId.DnlChannelId
                                NextPerCommitmentPoint = remoteNextCommitInfo.PerCommitmentPoint()
                            }
                        Some msg
            }
            return! ActiveChannel.CheckFundingConfirmed fundedChannel
        else
            let activeChannel = {
                ActiveChannel.ConnectedChannel = connectedChannel
            }
            return Ok activeChannel
    }

    static member private HandleRebroadcastUpdates (activeChannel: ActiveChannel)
                                                   : Async<ActiveChannel> = async {
        let rec receiveEvent (activeChannel: ActiveChannel): Async<ActiveChannel> = async {
            let connectedChannel = activeChannel.ConnectedChannel
            try
                let! recvChannelMsgRes = AsyncExtensions.WithTimeout (TimeSpan.FromSeconds 30.) (connectedChannel.PeerNode.RecvChannelMsg())
                match recvChannelMsgRes with
                | Ok (peerNodeAfterMsgReceived, channelMsg) ->
                    match channelMsg with
                    | :? UpdateAddHTLCMsg ->
                        return failwith "Uni-directional only!"
                    | :? UpdateFulfillHTLCMsg
                    | :? UpdateFailHTLCMsg
                    | :? UpdateFailMalformedHTLCMsg ->
                        let! handleHtlcSettleRes = activeChannel.HandleHtlcFulfillOrFail peerNodeAfterMsgReceived channelMsg false
                        match handleHtlcSettleRes with
                        | Ok (newActiveChannel, _wasSettleOk) ->
                            return! receiveEvent newActiveChannel
                        | Error err ->
                            return failwith (err :> IErrorMsg).Message
                    | :? CommitmentSignedMsg as theirCommitmentSignedMsg ->
                        let ourRevokeAndAckMsgAndChannelRes =
                            connectedChannel.Channel.Channel.ApplyCommitmentSigned theirCommitmentSignedMsg
                        match ourRevokeAndAckMsgAndChannelRes with
                        | Error err ->
                            return failwith err.Message
                        | Ok (channelAfterCommitmentSignedApply, ourRevokeAndAckMsg) ->
                            let! peerNodeAfterRevokeAndAckSent = peerNodeAfterMsgReceived.SendMsg ourRevokeAndAckMsg

                            let connectedChannelAfterRevokeAndAck =
                                {
                                    connectedChannel with
                                        PeerNode = peerNodeAfterRevokeAndAckSent
                                        Channel =
                                            {
                                                Channel = channelAfterCommitmentSignedApply
                                            }
                                }
                            connectedChannelAfterRevokeAndAck.SaveToWallet()
                            let ourCommitmentSignedMsgAndChannelRes = connectedChannelAfterRevokeAndAck.Channel.Channel.SignCommitment ()
                            let channelAfterCommitmentSigned, ourCommitmentSignedMsg = UnwrapResult ourCommitmentSignedMsgAndChannelRes "error executing sign commit command"
                            let! peerNodeAfterCommitmentSignedSent = peerNodeAfterRevokeAndAckSent.SendMsg ourCommitmentSignedMsg
                            let connectedChannelAfterCommitmentSignedSent =
                                {
                                    connectedChannel with
                                        PeerNode = peerNodeAfterCommitmentSignedSent
                                        Channel =
                                            {
                                                Channel = channelAfterCommitmentSigned
                                            }
                                }
                            connectedChannelAfterCommitmentSignedSent.SaveToWallet()
                            let activeChannel = { ConnectedChannel = connectedChannelAfterCommitmentSignedSent }
                            return! receiveEvent activeChannel
                    | :? RevokeAndACKMsg as theirRevokeAndAckMsg ->
                        let channelAferRevokeAndAckRes =
                            activeChannel.ConnectedChannel.Channel.Channel.ApplyRevokeAndACK theirRevokeAndAckMsg

                        match channelAferRevokeAndAckRes with
                        | Error err ->
                            return failwith err.Message
                        | Ok channelAferRevokeAndAck ->
                            let connectedChannelAfterRevokeAndAck =
                                {
                                    connectedChannel with
                                        PeerNode = peerNodeAfterMsgReceived
                                        Channel =
                                            {
                                                Channel = channelAferRevokeAndAck
                                            }
                                }
                            connectedChannelAfterRevokeAndAck.SaveToWallet()

                            let channelPrivKeys = connectedChannelAfterRevokeAndAck.Channel.ChannelPrivKeys
                            let savedChannelState = connectedChannel.Channel.Channel.SavedChannelState
                            let network = connectedChannelAfterRevokeAndAck.Network
                            let account = connectedChannelAfterRevokeAndAck.Account

                            let perCommitmentSecret = theirRevokeAndAckMsg.PerCommitmentSecret

                            let breachStore = BreachDataStore account
                            let! breachData =
                                    breachStore
                                        .LoadBreachData(connectedChannelAfterRevokeAndAck.ChannelId)
                                        .InsertRevokedCommitment perCommitmentSecret
                                                                    savedChannelState
                                                                    channelPrivKeys
                                                                    network
                                                                    account
                            breachStore.SaveBreachData breachData

                            do! TowerClient.Default.CreateAndSendPunishmentTx perCommitmentSecret
                                        savedChannelState
                                        channelPrivKeys
                                        network
                                        account
                                        true
                            let activeChannel = { ConnectedChannel = connectedChannelAfterRevokeAndAck }
                            return activeChannel
                    | _ ->
                        let connectedChannelAfterMsgReceived = {
                            connectedChannel with
                                PeerNode = peerNodeAfterMsgReceived
                        }
                        let activeChannelAfterMsgReceived = {
                            activeChannel with
                                ConnectedChannel = connectedChannelAfterMsgReceived
                        }
                        return! receiveEvent activeChannelAfterMsgReceived
                | Error err ->
                    return failwith (err :> IErrorMsg).Message
            with
            | :? TimeoutException ->
                return activeChannel
        }

        return! receiveEvent activeChannel
    }


    static member internal ConnectReestablish (channelStore: ChannelStore)
                                              (nodeMasterPrivKey: NodeMasterPrivKey)
                                              (channelId: ChannelIdentifier)
                                              (nonionEndPoint: Option<NOnionEndPoint>)
                                                  : Async<Result<ActiveChannel, ReconnectActiveChannelError>> = async {
        let! connectRes =
            ConnectedChannel.ConnectFromWallet channelStore nodeMasterPrivKey channelId nonionEndPoint
        match connectRes with
        | Error reconnectError -> return Error <| Reconnect reconnectError
        | Ok connectedChannel ->
            let! activeChannelRes = ActiveChannel.ConfirmFundingLocked connectedChannel
            match activeChannelRes with
            | Error lockFundingError -> return Error <| LockFunding lockFundingError
            | Ok activeChannel ->
                let! newActiveChannel = ActiveChannel.HandleRebroadcastUpdates activeChannel
                return Ok newActiveChannel
    }

    static member internal AcceptReestablish (channelStore: ChannelStore)
                                    (transportListener: TransportListener)
                                    (channelId: ChannelIdentifier)
                                        : Async<Result<ActiveChannel, ReconnectActiveChannelError >> = async {
        let! connectRes =
            ConnectedChannel.AcceptFromWallet channelStore transportListener channelId
        match connectRes with
        | Error reconnectError -> return Error <| Reconnect reconnectError
        | Ok connectedChannel ->
            let! activeChannelRes = ActiveChannel.ConfirmFundingLocked connectedChannel
            match activeChannelRes with
            | Error lockFundingError -> return Error <| LockFunding lockFundingError
            | Ok activeChannel -> return Ok activeChannel
    }

    member internal self.Balance
        with get(): LNMoney =
            self.ConnectedChannel.Channel.Balance()

    member internal self.SpendableBalance
        with get(): LNMoney =
            self.ConnectedChannel.Channel.SpendableBalance()

    member self.ChannelId
        with get(): ChannelIdentifier = self.ConnectedChannel.ChannelId

    member private self.SendCommit(): Async<Result<ActiveChannel, SendCommitError>> = async {
        let connectedChannel = self.ConnectedChannel
        let peerNode = connectedChannel.PeerNode
        let channel = connectedChannel.Channel

        let ourCommitmentSignedMsgAndChannelRes = channel.Channel.SignCommitment ()
        let channelAfterCommitmentSigned, ourCommitmentSignedMsg = UnwrapResult ourCommitmentSignedMsgAndChannelRes "error executing sign commit command"

        let! peerNodeAfterCommitmentSignedSent = peerNode.SendMsg ourCommitmentSignedMsg

        let rec recv (peerNode: PeerNode) =
            async {
                let! recvChannelMsgRes = peerNode.RecvChannelMsg()
                match recvChannelMsgRes with
                | Error (RecvMsg recvMsgError) -> return Error <| RecvRevokeAndAck recvMsgError
                | Error (ReceivedPeerErrorMessage (peerNodeAfterRevokeAndAckReceived, errorMessage)) ->
                    let connectedChannelAfterError =
                        {
                            connectedChannel with
                                PeerNode = peerNodeAfterRevokeAndAckReceived
                                Channel =
                                    {
                                        Channel = channelAfterCommitmentSigned
                                    }
                        }
                    let brokenChannel = { BrokenChannel.ConnectedChannel = connectedChannelAfterError }
                    return Error <| CommitmentSignedPeerErrorResponse
                        (brokenChannel, errorMessage)
                | Ok (peerNodeAfterRevokeAndAckReceived, channelMsg) ->
                    match channelMsg with
                    | :? RevokeAndACKMsg as theirRevokeAndAckMsg ->
                        let channelAferRevokeAndAckRes =
                            channelAfterCommitmentSigned.ApplyRevokeAndACK theirRevokeAndAckMsg

                        match channelAferRevokeAndAckRes with
                        | Error err ->
                            let connectedChannelAfterError =
                                {
                                    connectedChannel with
                                        PeerNode = peerNodeAfterRevokeAndAckReceived
                                        Channel =
                                            {
                                                Channel = channelAfterCommitmentSigned
                                            }
                                }
                            let! connectedChannelAfterErrorSent = connectedChannelAfterError.SendError err.Message
                            let brokenChannel = { BrokenChannel.ConnectedChannel = connectedChannelAfterErrorSent }
                            return Error <| InvalidRevokeAndAck
                                (brokenChannel, err)
                        | Ok channelAferRevokeAndAck ->
                            let connectedChannelAfterRevokeAndAck =
                                {
                                    connectedChannel with
                                        PeerNode = peerNodeAfterRevokeAndAckReceived
                                        Channel =
                                            {
                                                Channel = channelAferRevokeAndAck
                                            }
                                }
                            connectedChannelAfterRevokeAndAck.SaveToWallet()

                            let channelPrivKeys = connectedChannelAfterRevokeAndAck.Channel.ChannelPrivKeys
                            let savedChannelState = self.ConnectedChannel.Channel.Channel.SavedChannelState
                            let network = connectedChannelAfterRevokeAndAck.Network
                            let account = connectedChannelAfterRevokeAndAck.Account

                            let perCommitmentSecret = theirRevokeAndAckMsg.PerCommitmentSecret

                            let breachStore = BreachDataStore account
                            let! breachData =
                                    breachStore
                                        .LoadBreachData(connectedChannelAfterRevokeAndAck.ChannelId)
                                        .InsertRevokedCommitment perCommitmentSecret
                                                                    savedChannelState
                                                                    channelPrivKeys
                                                                    network
                                                                    account
                            breachStore.SaveBreachData breachData

                            do! TowerClient.Default.CreateAndSendPunishmentTx perCommitmentSecret
                                        savedChannelState
                                        channelPrivKeys
                                        network
                                        account
                                        true

                            let activeChannel = { ConnectedChannel = connectedChannelAfterRevokeAndAck }
                            return Ok activeChannel
                    | :? FundingLockedMsg as _fundingLockedMsg ->
                        return! recv(peerNodeAfterRevokeAndAckReceived);
                    | _ -> return Error <| ExpectedRevokeAndAck channelMsg
            }

        return! recv(peerNodeAfterCommitmentSignedSent);
    }

    member private self.RecvCommit(): Async<Result<ActiveChannel, RecvCommitError>> = async {
        let connectedChannel = self.ConnectedChannel
        let peerNode = connectedChannel.PeerNode
        let channel = connectedChannel.Channel

        let! recvChannelMsgRes = peerNode.RecvChannelMsg()
        match recvChannelMsgRes with
        | Error (RecvMsg recvMsgError) -> return Error <| RecvCommitmentSigned recvMsgError
        | Error (ReceivedPeerErrorMessage (peerNodeAfterCommitmentSignedReceived, errorMessage)) ->
            let connectedChannelAfterError = {
                connectedChannel with
                    PeerNode = peerNodeAfterCommitmentSignedReceived
                    Channel = channel
            }
            let brokenChannel = { BrokenChannel.ConnectedChannel = connectedChannelAfterError }
            return Error <| PeerErrorMessageInsteadOfCommitmentSigned
                (brokenChannel, errorMessage)
        | Ok (peerNodeAfterCommitmentSignedReceived, channelMsg) ->
            match channelMsg with
            | :? CommitmentSignedMsg as theirCommitmentSignedMsg ->
                let ourRevokeAndAckMsgAndChannelRes=
                    channel.Channel.ApplyCommitmentSigned theirCommitmentSignedMsg
                match ourRevokeAndAckMsgAndChannelRes with
                | Error err ->
                    let connectedChannelAfterError = {
                        connectedChannel with
                            PeerNode = peerNodeAfterCommitmentSignedReceived
                            Channel = channel
                    }
                    let! connectedChannelAfterErrorSent = connectedChannelAfterError.SendError err.Message
                    let brokenChannel = { BrokenChannel.ConnectedChannel = connectedChannelAfterErrorSent }
                    return Error <| InvalidCommitmentSigned
                        (brokenChannel, err)
                | Ok (channelAfterCommitmentSigned, ourRevokeAndAckMsg) ->
                    let! peerNodeAfterRevokeAndAckSent = peerNodeAfterCommitmentSignedReceived.SendMsg ourRevokeAndAckMsg

                    let connectedChannelAfterRevokeAndAck =
                        {
                            connectedChannel with
                                PeerNode = peerNodeAfterRevokeAndAckSent
                                Channel =
                                    {
                                        Channel = channelAfterCommitmentSigned
                                    }
                        }
                    connectedChannelAfterRevokeAndAck.SaveToWallet()
                    let activeChannel = { ConnectedChannel = connectedChannelAfterRevokeAndAck }
                    return Ok activeChannel
            | _ -> return Error <| ExpectedCommitmentSigned channelMsg
    }

    member private self.RecvHtlcFulfillOrFail(): Async<Result<ActiveChannel * bool, RecvFulfillOrFailError>> = async {
        let connectedChannel = self.ConnectedChannel
        let peerNode = connectedChannel.PeerNode
        let channel = connectedChannel.Channel

        let! recvChannelMsgRes = peerNode.RecvChannelMsg()
        match recvChannelMsgRes with
        | Error (RecvMsg recvMsgError) -> return Error <| RecvFulfillOrFailError.RecvFulfill recvMsgError
        | Error (ReceivedPeerErrorMessage (peerNodeAfterHtlcFulfillReceived, errorMessage)) ->
            let connectedChannelAfterError = {
                connectedChannel with
                    PeerNode = peerNodeAfterHtlcFulfillReceived
                    Channel = channel
            }
            let brokenChannel = { BrokenChannel.ConnectedChannel = connectedChannelAfterError }
            return Error <| PeerErrorMessageInsteadOfHtlcFulfill (brokenChannel, errorMessage)
        | Ok (peerNodeAfterHtlcResultReceived, channelMsg) ->
            return! self.HandleHtlcFulfillOrFail peerNodeAfterHtlcResultReceived channelMsg true
    }

    member internal self.HandleHtlcFulfillOrFail (nextPeerNode: PeerNode) (channelMsg: IChannelMsg) (commit: bool) : Async<Result<ActiveChannel * bool, RecvFulfillOrFailError>> =
        async {
            let connectedChannel = self.ConnectedChannel
            let channel = connectedChannel.Channel

            match channelMsg with
            | :? UpdateFulfillHTLCMsg as theirFulfillMsg ->
                let channelAfterFulfillMsgRes =
                    channel.Channel.ApplyUpdateFulfillHTLC theirFulfillMsg
                match channelAfterFulfillMsgRes with
                | Error err ->
                    let connectedChannelAfterError = {
                        connectedChannel with
                            PeerNode = nextPeerNode
                            Channel = channel
                    }
                    let brokenChannel = { BrokenChannel.ConnectedChannel = connectedChannelAfterError }
                    return Error <| InvalidCHtlcFulfillOrFail (brokenChannel, err)
                | Ok channelAfterFulfillMsg ->
                    let connectedChannelAfterFulfillMsg = {
                        connectedChannel with
                            PeerNode = nextPeerNode
                            Channel = 
                                {
                                    Channel = channelAfterFulfillMsg
                                }
                    }
                    connectedChannelAfterFulfillMsg.SaveToWallet()

                    let activeChannel = { ConnectedChannel = connectedChannelAfterFulfillMsg }
                    if commit then
                        let! activeChannelAfterCommitReceivedRes = activeChannel.RecvCommit()
                        match activeChannelAfterCommitReceivedRes with
                        | Error err -> return Error <| RecvFulfillOrFailError.RecvCommit err
                        | Ok activeChannelAfterCommitReceived ->
                            let! activeChannelAfterCommitSentRes = activeChannelAfterCommitReceived.SendCommit()
                            match activeChannelAfterCommitSentRes with
                            | Error err -> return Error <| RecvFulfillOrFailError.SendCommit err
                            | Ok activeChannelAfterCommitSent -> return Ok (activeChannelAfterCommitSent, true)
                    else
                        return Ok (activeChannel, true)
            | :? UpdateFailHTLCMsg as theirFailMsg ->
                let channelAfterFailMsgRes =
                    channel.Channel.ApplyUpdateFailHTLC theirFailMsg
                match channelAfterFailMsgRes with
                | Error err ->
                    let connectedChannelAfterError = {
                        connectedChannel with
                            PeerNode = nextPeerNode
                            Channel = channel
                    }
                    let brokenChannel = { BrokenChannel.ConnectedChannel = connectedChannelAfterError }
                    return Error <| InvalidCHtlcFulfillOrFail (brokenChannel, err)
                | Ok channelAfterFailMsg ->
                    let connectedChannelAfterFailMsg = {
                        connectedChannel with
                            PeerNode = nextPeerNode
                            Channel = 
                                {
                                    Channel = channelAfterFailMsg
                                }
                    }
                    connectedChannelAfterFailMsg.SaveToWallet()
                    let activeChannel = { ConnectedChannel = connectedChannelAfterFailMsg }
                    if commit then
                        let! activeChannelAfterCommitReceivedRes = activeChannel.RecvCommit()
                        match activeChannelAfterCommitReceivedRes with
                        | Error err -> return Error <| RecvFulfillOrFailError.RecvCommit err
                        | Ok activeChannelAfterCommitReceived ->
                            let! activeChannelAfterCommitSentRes = activeChannelAfterCommitReceived.SendCommit()
                            match activeChannelAfterCommitSentRes with
                            | Error err -> return Error <| RecvFulfillOrFailError.SendCommit err
                            | Ok activeChannelAfterCommitSent -> return Ok (activeChannelAfterCommitSent, false)
                    else
                        return Ok (activeChannel, false)
            | :? UpdateFailMalformedHTLCMsg as theirFailMsg ->
                let channelAfterFailMsgRes =
                    channel.Channel.ApplyUpdateFailMalformedHTLC theirFailMsg
                match channelAfterFailMsgRes with
                | Error err ->
                    let connectedChannelAfterError = {
                        connectedChannel with
                            PeerNode = nextPeerNode
                            Channel = channel
                    }
                    let brokenChannel = { BrokenChannel.ConnectedChannel = connectedChannelAfterError }
                    return Error <| InvalidCHtlcFulfillOrFail (brokenChannel, err)
                | Ok channelAfterFailMsg ->
                    let connectedChannelAfterFailMsg = {
                        connectedChannel with
                            PeerNode = nextPeerNode
                            Channel = 
                                {
                                    Channel = channelAfterFailMsg
                                }
                    }
                    connectedChannelAfterFailMsg.SaveToWallet()
                    let activeChannel = { ConnectedChannel = connectedChannelAfterFailMsg }
                    if commit then
                        let! activeChannelAfterCommitReceivedRes = activeChannel.RecvCommit()
                        match activeChannelAfterCommitReceivedRes with
                        | Error err -> return Error <| RecvFulfillOrFailError.RecvCommit err
                        | Ok activeChannelAfterCommitReceived ->
                            let! activeChannelAfterCommitSentRes = activeChannelAfterCommitReceived.SendCommit()
                            match activeChannelAfterCommitSentRes with
                            | Error err -> return Error <| RecvFulfillOrFailError.SendCommit err
                            | Ok activeChannelAfterCommitSent -> return Ok (activeChannelAfterCommitSent, false)
                    else
                        return Ok (activeChannel, false)
            | _ -> return Error <| ExpectedHtlcFulfillOrFail channelMsg
        }

    member internal self.SendMonoHopUnidirectionalPayment (amount: LNMoney)
                                                     : Async<Result<ActiveChannel, SendMonoHopPaymentError>> = async {
        let connectedChannel = self.ConnectedChannel
        let peerNode = connectedChannel.PeerNode
        let channel = connectedChannel.Channel

        let monoHopUnidirectionalPaymentMsgAndChannelRes =
            channel.Channel.MonoHopUnidirectionalPayment amount
        match monoHopUnidirectionalPaymentMsgAndChannelRes with
        | Error (InvalidMonoHopUnidirectionalPayment err) ->
            return Error <| SendMonoHopPaymentError.InvalidMonoHopPayment (self, err)
        | Error err -> return failwith <| SPrintF1 "error executing mono hop payment command: %s" err.Message
        | Ok (channelAfterMonoHopPayment, monoHopUnidirectionalPaymentMsg) ->
            let! peerNodeAfterMonoHopPaymentSent = peerNode.SendMsg monoHopUnidirectionalPaymentMsg
            let connectedChannelAfterMonoHopPaymentSent =
                {
                    connectedChannel with
                        PeerNode = peerNodeAfterMonoHopPaymentSent
                        Channel =
                            {
                                Channel = channelAfterMonoHopPayment
                            }
                }
            connectedChannelAfterMonoHopPaymentSent.SaveToWallet()
            let activeChannel = { ConnectedChannel = connectedChannelAfterMonoHopPaymentSent }
            let! activeChannelAfterCommitSentRes = activeChannel.SendCommit()
            match activeChannelAfterCommitSentRes with
            | Error err -> return Error <| SendMonoHopPaymentError.SendCommit err
            | Ok activeChannelAfterCommitSent ->
                let! activeChannelAfterCommitReceivedRes = activeChannelAfterCommitSent.RecvCommit()
                match activeChannelAfterCommitReceivedRes with
                | Error err -> return Error <| SendMonoHopPaymentError.RecvCommit err
                | Ok activeChannelAfterCommitReceived -> return Ok activeChannelAfterCommitReceived
    }

    member internal self.SendHtlcPayment (invoice: PaymentInvoice) (waitForResult: bool): Async<Result<ActiveChannel, SendHtlcPaymentError>> = async {
        let connectedChannel = self.ConnectedChannel
        let peerNode = connectedChannel.PeerNode
        let channel = connectedChannel.Channel
        let currency = (connectedChannel.Account :> IAccount).Currency

        let sessionKey = new NBitcoin.Key()

        let paymentRequest = invoice.PaymentRequest
        let amount =
            match paymentRequest.AmountValue with
            | Some invoiceAmount ->
                invoiceAmount
            | None ->
                failwith "Geewallet doesn't support invoices without amount field"

        let associatedData = paymentRequest.PaymentHash.ToBytes()

        if paymentRequest.NodeIdValue.Value <> channel.RemoteNodeId.Value then
            return Error <| SendHtlcPaymentError.NoMultiHopHtlcSupport
        else
            let! blockHeight = async {
                let! blockHeightResponse =
                    Server.Query currency
                        (QuerySettings.Default ServerSelectionMode.Fast)
                        (ElectrumClient.SubscribeHeaders ())
                        None
                return
                    (uint32 blockHeightResponse.Height) + paymentRequest.MinFinalCLTVExpiryDelta.Value
            }

            let tlvs =
                match paymentRequest.PaymentSecret with
                | Some paymentSecret ->
                    [|
                        HopPayloadTLV.AmountToForward amount
                        HopPayloadTLV.OutgoingCLTV blockHeight
                        HopPayloadTLV.PaymentData
                            (
                                PaymentSecret.Create paymentSecret,
                                amount
                            )
                    |]
                | None ->
                    [|
                        HopPayloadTLV.AmountToForward amount
                        HopPayloadTLV.OutgoingCLTV blockHeight
                    |]

            let realm0Data = TLVPayload(tlvs).ToBytes()
            let onionPacket = Sphinx.PacketAndSecrets.Create (sessionKey, (List.singleton channel.RemoteNodeId.Value), (List.singleton realm0Data), associatedData, Sphinx.PacketFiller.DeterministicPacketFiller)

            let channelAndAddHtlcMsgRes =
                channel.Channel.AddHTLC
                    {
                        OperationAddHTLC.Amount = amount
                        PaymentHash = paymentRequest.PaymentHash
                        Expiry = BlockHeight blockHeight
                        Onion = onionPacket.Packet
                        Upstream = None
                        Origin = Origin.Local
                        CurrentHeight = BlockHeight.Zero
                    }

            match channelAndAddHtlcMsgRes with
            | Error (InvalidUpdateAddHTLC err) ->
                return Error <| SendHtlcPaymentError.InvalidHtlcPayment (self, err)
            | Error err -> return failwith <| SPrintF1 "error executing htlc payment command: %s" err.Message
            | Ok (channelAfterAddHtlcPayment, addHtlcMsg) ->
                let! peerNodeAfterAddHtlcSent = peerNode.SendMsg addHtlcMsg
                let connectedChannelAfterAddHtlcSent =
                    {
                        connectedChannel with
                            PeerNode = peerNodeAfterAddHtlcSent
                            Channel =
                                {
                                    Channel = channelAfterAddHtlcPayment
                                }
                    }
                connectedChannelAfterAddHtlcSent.SaveToWallet()
                let activeChannel = { ConnectedChannel = connectedChannelAfterAddHtlcSent }
                let! activeChannelAfterCommitSentRes = activeChannel.SendCommit()
                match activeChannelAfterCommitSentRes with
                | Error err -> return Error <| SendHtlcPaymentError.SendCommit err
                | Ok activeChannelAfterCommitSent ->
                    let! activeChannelAfterCommitReceivedRes = activeChannelAfterCommitSent.RecvCommit()
                    match activeChannelAfterCommitReceivedRes with
                    | Error err -> return Error <| SendHtlcPaymentError.RecvCommit err
                    | Ok activeChannelAfterCommitReceived when waitForResult ->
                        let! activeChannelAfterNewCommitRes = activeChannelAfterCommitReceived.RecvHtlcFulfillOrFail()
                        match (activeChannelAfterNewCommitRes) with
                        | Error err ->
                            return Error <| SendHtlcPaymentError.RecvFulfillOrFail err
                        | Ok (activeChannelAfterNewCommit, true) ->
                            return Ok (activeChannelAfterNewCommit)
                        | Ok (activeChannelAfterNewCommit, false) ->
                            return Error <| SendHtlcPaymentError.ReceivedHtlcFail activeChannelAfterNewCommit
                    | Ok activeChannelAfterCommitReceived ->
                        return Ok (activeChannelAfterCommitReceived)
    }

    member internal self.RecvMonoHopUnidirectionalPayment (monoHopUnidirectionalPaymentMsg: MonoHopUnidirectionalPaymentMsg)
                                                              : Async<Result<ActiveChannel, RecvMonoHopPaymentError>> = async {
        let connectedChannel = self.ConnectedChannel
        let channelWrapper = connectedChannel.Channel

        let channelAfterMonoHopPaymentReceivedRes =
            channelWrapper.Channel.ApplyMonoHopUnidirectionalPayment
                monoHopUnidirectionalPaymentMsg

        match channelAfterMonoHopPaymentReceivedRes with
        | Error err ->
            let connectedChannelAfterErrorReceived =
                { connectedChannel with
                    Channel = channelWrapper
                }
            let! connectedChannelAfterError = connectedChannelAfterErrorReceived.SendError err.Message
            let brokenChannel = { BrokenChannel.ConnectedChannel = connectedChannelAfterError }
            return Error <| InvalidMonoHopPayment (brokenChannel, err)
        | Ok channelAfterMonoHopPaymentReceived ->
            let connectedChannelAfterMonoHopPaymentReceived =
                {
                    connectedChannel with
                        Channel =
                            {
                                Channel = channelAfterMonoHopPaymentReceived
                            }
                }

            connectedChannelAfterMonoHopPaymentReceived.SaveToWallet()
            let activeChannel = { ConnectedChannel = connectedChannelAfterMonoHopPaymentReceived }
            let! activeChannelAfterCommitReceivedRes = activeChannel.RecvCommit()
            match activeChannelAfterCommitReceivedRes with
            | Error err -> return Error <| RecvMonoHopPaymentError.RecvCommit err
            | Ok activeChannelAfterCommitReceived ->
            let! activeChannelAfterCommitSentRes = activeChannelAfterCommitReceived.SendCommit()
            match activeChannelAfterCommitSentRes with
            | Error err -> return Error <| RecvMonoHopPaymentError.SendCommit err
            | Ok activeChannelAfterCommitSent -> return Ok activeChannelAfterCommitSent
    }

    member internal self.FailHtlc (id: HTLCId) (reason: HtlcSettleFailReason) : Async<Result<ActiveChannel * HTLCSettleStatus, RecvHtlcPaymentError>> =
        async {
            let connectedChannel = self.ConnectedChannel
            let channelWrapper = connectedChannel.Channel
            let peerNode = connectedChannel.PeerNode

            let opFail = {
                OperationFailHTLC.Id = id
                Reason = 
                    Choice2Of2 
                        {
                            FailureMsg.Code = 
                                match reason with
                                | UnknownPaymentHash ->
                                    OnionError.UNKNOWN_PAYMENT_HASH
                                | IncorrectPaymentAmount ->
                                    OnionError.INCORRECT_PAYMENT_AMOUNT
                                | RequiredChannelFeatureMissing ->
                                    OnionError.REQUIRED_CHANNEL_FEATURE_MISSING
                                | BadFinalPayload ->
                                    //FIXME: this is not the greatest error to represent what happened
                                    OnionError.TEMPORARY_NODE_FAILURE
                                |> OnionError.FailureCode
                            Data =
                                match reason with
                                | UnknownPaymentHash ->
                                    FailureMsgData.UnknownPaymentHash
                                | IncorrectPaymentAmount ->
                                    FailureMsgData.IncorrectPaymentAmount
                                | RequiredChannelFeatureMissing ->
                                    FailureMsgData.RequiredChannelFeatureMissing
                                | BadFinalPayload ->
                                    //FIXME: this is not the greatest error to represent what happened 
                                    FailureMsgData.TemporaryNodeFailure
                        }
            }

            let channelAfterFailRes = channelWrapper.Channel.FailHTLC opFail
            match channelAfterFailRes with
            | Error err ->
                let connectedChannelAfterErrorReceived =
                    { connectedChannel with
                        Channel = channelWrapper
                    }
                let! connectedChannelAfterError = connectedChannelAfterErrorReceived.SendError err.Message
                let brokenChannel = { BrokenChannel.ConnectedChannel = connectedChannelAfterError }
                return Error <| SettleFailed (brokenChannel, err)
            | Ok (channelAfterFail, failMsg) ->
                let! peerNodeAfterFailSent = peerNode.SendMsg failMsg
                let connectedChannelAfterFailSent =
                    {
                        connectedChannel with
                            PeerNode = peerNodeAfterFailSent
                            Channel =
                                {
                                    Channel = channelAfterFail
                                }
                    }

                connectedChannelAfterFailSent.SaveToWallet()
                let activeChannel = { ConnectedChannel = connectedChannelAfterFailSent }
                let! activeChannelAfterCommitSentRes = activeChannel.SendCommit()
                match activeChannelAfterCommitSentRes with
                | Error err -> return Error <| RecvHtlcPaymentError.SendCommit err
                | Ok activeChannelAfterCommitSent ->
                    let! activeChannelAfterCommitReceivedRes = activeChannelAfterCommitSent.RecvCommit()
                    match activeChannelAfterCommitReceivedRes with
                    | Error err -> return Error <| RecvHtlcPaymentError.RecvCommit err
                    | Ok activeChannelAfterCommitReceived -> return Ok (activeChannelAfterCommitReceived, HTLCSettleStatus.Fail)
        }

    member internal self.FailMalformedHtlc (htlc: UpdateAddHTLCMsg) (cryptoError: CryptoError) : Async<Result<ActiveChannel * HTLCSettleStatus, RecvHtlcPaymentError>> =
        async {
            let connectedChannel = self.ConnectedChannel
            let channelWrapper = connectedChannel.Channel
            let peerNode = connectedChannel.PeerNode

            let opFail = {
                OperationFailMalformedHTLC.Id = htlc.HTLCId
                Sha256OfOnion = htlc.OnionRoutingPacket.ToBytes() |> Crypto.Hashes.SHA256 |> uint256
                FailureCode =
                    match cryptoError with
                    | BadMac ->
                        OnionError.INVALID_ONION_HMAC
                    | InvalidPublicKey _ ->
                        OnionError.INVALID_ONION_KEY
                    | _ ->
                        OnionError.INVALID_ONION_VERSION
                    |> OnionError.FailureCode
            }

            let channelAfterFailRes = channelWrapper.Channel.FailMalformedHTLC opFail
            match channelAfterFailRes with
            | Error err ->
                let connectedChannelAfterErrorReceived =
                    { connectedChannel with
                        Channel = channelWrapper
                    }
                let! connectedChannelAfterError = connectedChannelAfterErrorReceived.SendError err.Message
                let brokenChannel = { BrokenChannel.ConnectedChannel = connectedChannelAfterError }
                return Error <| SettleFailed (brokenChannel, err)
            | Ok (channelAfterFail, failMsg) ->
                let! peerNodeAfterFailSent = peerNode.SendMsg failMsg
                let connectedChannelAfterFailSent =
                    {
                        connectedChannel with
                            PeerNode = peerNodeAfterFailSent
                            Channel =
                                {
                                    Channel = channelAfterFail
                                }
                    }

                connectedChannelAfterFailSent.SaveToWallet()
                let activeChannel = { ConnectedChannel = connectedChannelAfterFailSent }
                let! activeChannelAfterCommitSentRes = activeChannel.SendCommit()
                match activeChannelAfterCommitSentRes with
                | Error err -> return Error <| RecvHtlcPaymentError.SendCommit err
                | Ok activeChannelAfterCommitSent ->
                    let! activeChannelAfterCommitReceivedRes = activeChannelAfterCommitSent.RecvCommit()
                    match activeChannelAfterCommitReceivedRes with
                    | Error err -> return Error <| RecvHtlcPaymentError.RecvCommit err
                    | Ok activeChannelAfterCommitReceived -> return Ok (activeChannelAfterCommitReceived, HTLCSettleStatus.Fail)
        }

    member internal self.SettleIncomingHtlc (updateAddHTLCMsg: UpdateAddHTLCMsg)
                                                              : Async<Result<ActiveChannel * HTLCSettleStatus, RecvHtlcPaymentError>> = async {
        let connectedChannel = self.ConnectedChannel
        let channelWrapper = connectedChannel.Channel
        let peerNode = connectedChannel.PeerNode
        let nodePrivateKey = channelWrapper.Channel.NodeSecret.RawKey()

        let parsedPacketRes =
            updateAddHTLCMsg.OnionRoutingPacket.ToBytes()
            |> Sphinx.parsePacket nodePrivateKey (updateAddHTLCMsg.PaymentHash.ToBytes())

        match parsedPacketRes with
        | Error err ->
            return! self.FailMalformedHtlc updateAddHTLCMsg err
        | Ok parsedPacket ->
            let onionPayload =
                OnionPayload.FromBytes parsedPacket.Payload
            match onionPayload with
            | Ok (TLVPayload tlvs) ->
                //Geewallet cannot handle routing payments
                let shortChannelIdOpt = tlvs |> Array.tryFind ( function | ShortChannelId _ -> true | _ -> false)
                //According to spec these values MUST exist in the payload
                let amountToForwardOpt = tlvs |> Array.tryPick ( function | AmountToForward amountToForward -> Some amountToForward | _ -> None) 
                let outgoingCLTVOpt = tlvs |> Array.tryPick ( function | OutgoingCLTV cltv -> Some cltv | _ -> None)

                match shortChannelIdOpt, amountToForwardOpt, outgoingCLTVOpt with
                | None, Some amountToForward, Some outgoingCLTV ->
                    let invoiceOpt =
                        (InvoiceDataStore connectedChannel.Account)
                            .LoadInvoiceData()
                            .TryGetInvoice updateAddHTLCMsg.PaymentHash

                    match invoiceOpt with
                    | None ->
                        return! self.FailHtlc updateAddHTLCMsg.HTLCId HtlcSettleFailReason.UnknownPaymentHash
                    | Some invoice ->
                        assert (invoice.MinFinalCltvExpiry = outgoingCLTV)
                        if invoice.Amount = amountToForward.ToMoney() && invoice.Amount = updateAddHTLCMsg.Amount.ToMoney() then
                            // should we do anything wrt CLTV?
                            let opFullfil = 
                                {
                                    OperationFulfillHTLC.Id = updateAddHTLCMsg.HTLCId
                                    PaymentPreimage = invoice.PaymentPreimage
                                    // This field is unused, probably because it's ported from eclair
                                    Commit = true
                                }
 
                            let channelAfterFulFillRes =
                                 channelWrapper.Channel.FulFillHTLC opFullfil
                            match channelAfterFulFillRes with
                            | Error err ->
                                let connectedChannelAfterErrorReceived =
                                    { connectedChannel with
                                        Channel = channelWrapper
                                    }
                                let! connectedChannelAfterError = connectedChannelAfterErrorReceived.SendError err.Message
                                let brokenChannel = { BrokenChannel.ConnectedChannel = connectedChannelAfterError }
                                return Error <| SettleFailed (brokenChannel, err)
                            | Ok (channelAfterFulFill, fulfillMsg) ->
                                let! peerNodeAfterFulFillSent = peerNode.SendMsg fulfillMsg
                                let connectedChannelAfterFulFillSent =
                                    {
                                        connectedChannel with
                                            PeerNode = peerNodeAfterFulFillSent
                                            Channel =
                                                {
                                                    Channel = channelAfterFulFill
                                                }
                                    }

                                connectedChannelAfterFulFillSent.SaveToWallet()
                                let activeChannel = { ConnectedChannel = connectedChannelAfterFulFillSent }
                                let! activeChannelAfterCommitSentRes = activeChannel.SendCommit()
                                match activeChannelAfterCommitSentRes with
                                | Error err -> return Error <| RecvHtlcPaymentError.SendCommit err
                                | Ok activeChannelAfterCommitSent ->
                                    let! activeChannelAfterCommitReceivedRes = activeChannelAfterCommitSent.RecvCommit()
                                    match activeChannelAfterCommitReceivedRes with
                                    | Error err -> return Error <| RecvHtlcPaymentError.RecvCommit err
                                    | Ok activeChannelAfterCommitReceived -> return Ok (activeChannelAfterCommitReceived, HTLCSettleStatus.Success)
                        else
                            return! self.FailHtlc updateAddHTLCMsg.HTLCId HtlcSettleFailReason.IncorrectPaymentAmount
                | _ ->
                    return! self.FailHtlc updateAddHTLCMsg.HTLCId BadFinalPayload
            | _ ->
                return! self.FailHtlc updateAddHTLCMsg.HTLCId HtlcSettleFailReason.RequiredChannelFeatureMissing
    }

    member internal self.RecvHtlcPayment (updateAddHTLCMsg: UpdateAddHTLCMsg) (settleImmediately: bool)
                                                              : Async<Result<ActiveChannel * HTLCSettleStatus, RecvHtlcPaymentError>> = async {
        let connectedChannel = self.ConnectedChannel
        let channelWrapper = connectedChannel.Channel
        let currency = (connectedChannel.Account :> IAccount).Currency
        
        let! blockHeight = async {
            let! blockHeightResponse =
                Server.Query currency
                    (QuerySettings.Default ServerSelectionMode.Fast)
                    (ElectrumClient.SubscribeHeaders ())
                    None
            return
                (blockHeightResponse.Height |> uint32) |> BlockHeight
        }
        
        let channelAfterMonoHopPaymentReceivedRes =
            channelWrapper.Channel.ApplyUpdateAddHTLC
                updateAddHTLCMsg
                blockHeight

        match channelAfterMonoHopPaymentReceivedRes with
        | Error err ->
            let connectedChannelAfterErrorReceived =
                { connectedChannel with
                    Channel = channelWrapper
                }
            let! connectedChannelAfterError = connectedChannelAfterErrorReceived.SendError err.Message
            let brokenChannel = { BrokenChannel.ConnectedChannel = connectedChannelAfterError }
            return Error <| InvalidHtlcPayment (brokenChannel, err)
        | Ok channelAfterMonoHopPaymentReceived ->
            let connectedChannelAfterMonoHopPaymentReceived =
                {
                    connectedChannel with
                        Channel =
                            {
                                Channel = channelAfterMonoHopPaymentReceived
                            }
                }

            connectedChannelAfterMonoHopPaymentReceived.SaveToWallet()
            let activeChannel = { ConnectedChannel = connectedChannelAfterMonoHopPaymentReceived }
            let! activeChannelAfterCommitReceivedRes = activeChannel.RecvCommit()
            match activeChannelAfterCommitReceivedRes with
            | Error err -> return Error <| RecvHtlcPaymentError.RecvCommit err
            | Ok activeChannelAfterCommitReceived ->
            let! activeChannelAfterCommitSentRes = activeChannelAfterCommitReceived.SendCommit()
            match activeChannelAfterCommitSentRes with
            | Error err -> return Error <| RecvHtlcPaymentError.SendCommit err
            | Ok activeChannelAfterCommitSent when settleImmediately ->
                return! activeChannelAfterCommitSent.SettleIncomingHtlc updateAddHTLCMsg
            | Ok activeChannelAfterCommitSent ->
                return Ok (activeChannelAfterCommitSent, HTLCSettleStatus.NotSettled)
    }

    member internal self.AcceptUpdateFee (): Async<Result<ActiveChannel, AcceptUpdateFeeError>> = async {
        let connectedChannel = self.ConnectedChannel
        let channelWrapper = connectedChannel.Channel
        let peerNode = connectedChannel.PeerNode

        let! recvChannelMsgRes = peerNode.RecvChannelMsg()
        match recvChannelMsgRes with
        | Error (RecvMsg recvMsgError) ->
            return Error <| AcceptUpdateFeeError.RecvUpdateFee recvMsgError
        | Error (ReceivedPeerErrorMessage (peerNodeAfterUpdateFeeReceived, errorMessage)) ->
            let connectedChannelAfterError = {
                connectedChannel with
                    PeerNode = peerNodeAfterUpdateFeeReceived
                    Channel = channelWrapper
            }
            let brokenChannel = { BrokenChannel.ConnectedChannel = connectedChannelAfterError }
            return Error <| AcceptUpdateFeeError.PeerErrorMessageInsteadOfUpdateFee
                (brokenChannel, errorMessage)
        | Ok (peerNodeAfterAcceptUpdateFeeReceived, channelMsg) ->
            match channelMsg with
            | :? UpdateFeeMsg as updateFeeMsg ->
                let channelWrapperAfterUpdateFeeReceivedRes =
                    channelWrapper.Channel.ApplyUpdateFee updateFeeMsg

                match channelWrapperAfterUpdateFeeReceivedRes with
                | Error err ->
                    let connectedChannelAfterUpdateFeeReceived = {
                        connectedChannel with
                            Channel = channelWrapper
                            PeerNode = peerNodeAfterAcceptUpdateFeeReceived
                    }
                    let! connectedChannelAfterError = connectedChannelAfterUpdateFeeReceived.SendError err.Message
                    let brokenChannel = { BrokenChannel.ConnectedChannel = connectedChannelAfterError }
                    return Error <| AcceptUpdateFeeError.InvalidUpdateFee
                        (brokenChannel, err)
                | Ok channelWrapperAfterUpdateFeeReceived ->
                    let connectedChannelAfterUpdateFeeReceived =
                        {
                            connectedChannel with
                                Channel =
                                    {
                                        Channel = channelWrapperAfterUpdateFeeReceived
                                    }
                                PeerNode = peerNodeAfterAcceptUpdateFeeReceived
                        }
                    connectedChannelAfterUpdateFeeReceived.SaveToWallet()
                    let activeChannel = { ConnectedChannel = connectedChannelAfterUpdateFeeReceived }
                    let! activeChannelAfterCommitReceivedRes = activeChannel.RecvCommit()
                    match activeChannelAfterCommitReceivedRes with
                    | Error err -> return Error <| AcceptUpdateFeeError.RecvCommit err
                    | Ok activeChannelAfterCommitReceived ->
                        let! activeChannelAfterCommitSentRes = activeChannelAfterCommitReceived.SendCommit()
                        match activeChannelAfterCommitSentRes with
                        | Error err -> return Error <| AcceptUpdateFeeError.SendCommit err
                        | Ok activeChannelAfterCommitSent -> return Ok activeChannelAfterCommitSent
            | _ -> return Error <| AcceptUpdateFeeError.ExpectedUpdateFee channelMsg
    }

    member internal self.UpdateFee (newFeeRate: FeeRatePerKw)
                                       : Async<Result<ActiveChannel, UpdateFeeError>> = async {
        let connectedChannel = self.ConnectedChannel
        let channelWrapper = connectedChannel.Channel
        let channelWrapperAndMsgAfterUpdateFeeRes =
            {
                OperationUpdateFee.FeeRatePerKw = newFeeRate
            }
            |> channelWrapper.Channel.UpdateFee

        match channelWrapperAndMsgAfterUpdateFeeRes with
        | Error (WeCannotAffordFee(localChannelReserve, requiredFee, missingAmount)) ->
            Infrastructure.LogDebug
            <| SPrintF4
                "cannot afford to update channel fee to %s: \
                    local channel reserve == %s, \
                    required fee == %s, \
                    missing amount == %s"
                (newFeeRate.ToString())
                (localChannelReserve.ToString())
                (requiredFee.ToString())
                (missingAmount.ToString())
            let activeChannelAfterUpdateFeeAttempt = {
                self with
                    ConnectedChannel = connectedChannel
            }
            return Ok activeChannelAfterUpdateFeeAttempt
        | Error err ->
            return
                SPrintF1 "error executing update fee command: %s" err.Message
                |> failwith
        | Ok (channelWrapperAfterUpdateFee, updateFeeMsg) ->
            let connectedChannelAfterUpdateFee = {
                connectedChannel with
                    Channel =
                        {
                            Channel = channelWrapperAfterUpdateFee
                        }
            }
            let peerNode = connectedChannelAfterUpdateFee.PeerNode
            let! peerNodeAfterUpdateFeeMsgSent =
                peerNode.SendMsg updateFeeMsg
            let connectedChannelAfterUpdateFeeMsgSent = {
                connectedChannelAfterUpdateFee with
                    PeerNode = peerNodeAfterUpdateFeeMsgSent
            }
            connectedChannelAfterUpdateFeeMsgSent.SaveToWallet()
            let activeChannelAfterUpdateFeeMsgSent = {
                self with
                    ConnectedChannel = connectedChannelAfterUpdateFeeMsgSent
            }
            let! activeChannelAfterCommitSentRes = activeChannelAfterUpdateFeeMsgSent.SendCommit()
            match activeChannelAfterCommitSentRes with
            | Error err -> return Error <| UpdateFeeError.SendCommit err
            | Ok activeChannelAfterCommitSent ->
                let! activeChannelAfterCommitReceivedRes = activeChannelAfterCommitSent.RecvCommit()
                match activeChannelAfterCommitReceivedRes with
                | Error err -> return Error <| UpdateFeeError.RecvCommit err
                | Ok activeChannelAfterCommitReceived -> return Ok activeChannelAfterCommitReceived
    }

    member internal self.Commitments(): Commitments =
        self.ConnectedChannel.Channel.Channel.Commitments

