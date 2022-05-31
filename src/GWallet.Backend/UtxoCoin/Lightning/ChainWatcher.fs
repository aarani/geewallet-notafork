﻿namespace GWallet.Backend.UtxoCoin.Lightning

open System.Linq

open NBitcoin
open DotNetLightning.Channel
open DotNetLightning.Channel.ClosingHelpers
open DotNetLightning.Utils
open DotNetLightning.Crypto
open ResultUtils.Portability

open GWallet.Backend
open GWallet.Backend.UtxoCoin
open GWallet.Backend.FSharpUtil
open GWallet.Backend.FSharpUtil.UwpHacks


module public ChainWatcher =

    let internal CheckForChannelFraudAndSendRevocationTx (channelId: ChannelIdentifier)
                                                         (channelStore: ChannelStore)
                                                             : Async<Option<string>> = async {
        let serializedChannel = channelStore.LoadChannel channelId
        let currency = (channelStore.Account :> IAccount).Currency
        let fundingScriptCoin = serializedChannel.FundingScriptCoin()
        let fundingDestination = fundingScriptCoin.ScriptPubKey.GetDestination()
        let network = UtxoCoin.Account.GetNetwork currency
        let fundingAddress: BitcoinAddress = fundingDestination.GetAddress network
        let fundingAddressString: string = fundingAddress.ToString()

        let scriptHash =
            Account.GetElectrumScriptHashFromPublicAddress
                currency
                fundingAddressString

        let! historyList =
            Server.Query currency
                         (QuerySettings.Default ServerSelectionMode.Fast)
                         (ElectrumClient.GetBlockchainScriptHashHistory scriptHash)
                         None

        let checkIfRevokedCommitment (spendingTxInfo: BlockchainScriptHashHistoryInnerResult) : Async<Option<string>> =
            async {
                let spendingTxId = spendingTxInfo.TxHash
        
                let! spendingTxString =
                    Server.Query
                        currency
                        (QuerySettings.Default ServerSelectionMode.Fast)
                        (ElectrumClient.GetBlockchainTransaction spendingTxId)
                        None
        
                let spendingTx =
                    Transaction.Parse(spendingTxString, network)
        
        
                let obscuredCommitmentNumberOpt =
                    tryGetObscuredCommitmentNumber fundingScriptCoin.Outpoint spendingTx
        
                match obscuredCommitmentNumberOpt with
                | Ok obscuredCommitmentNumber ->
                    let localChannelPubKeys = serializedChannel.LocalChannelPubKeys
                    let remoteChannelPubKeys = serializedChannel.SavedChannelState.StaticChannelConfig.RemoteChannelPubKeys
        
                    let commitmentNumber =
                        obscuredCommitmentNumber.Unobscure
                            serializedChannel.SavedChannelState.StaticChannelConfig.IsFunder
                            localChannelPubKeys.PaymentBasepoint
                            remoteChannelPubKeys.PaymentBasepoint
        
                    //TODO: or we could just search based on CommitmentTxHash
                    let breachDataOpt =
                        (BreachDataStore channelStore.Account)
                            .LoadBreachData(channelId)
                            .GetBreachData(commitmentNumber)
        
                    match breachDataOpt with
                    | None -> return None
                    | Some breachData ->
                        let! txId = UtxoCoin.Account.BroadcastRawTransaction currency breachData.PenaltyTx
                        return Some <| txId
                | Error _ -> return None
            }
        
        
        return!
            ListAsyncTryPick
                historyList
                (fun txInfo ->
                    // Only check spending txs with at least 1 conf, cause we need at least 1 conf to broadcast our
                    // penalty tx (because it tries to spend to_remote output as well, which is time-locked for one block)
                    if txInfo.Height > 0u then
                        checkIfRevokedCommitment txInfo
                    else
                        async { return None })

    }

    let CheckForChannelFraudsAndSendRevocationTx (accounts: seq<UtxoCoin.NormalUtxoAccount>)
                                                     : seq<Async<Option<string>>> =
        seq {
            for account in accounts do
                let channelStore = ChannelStore account
                let channelIds = channelStore.ListChannelIds()

                for channelId in channelIds do
                    yield
                        CheckForChannelFraudAndSendRevocationTx channelId channelStore
        }

    let CheckForChannelForceCloseAndSaveUnresolvedHtlcs (channelId: ChannelIdentifier)
                                                        (channelStore: ChannelStore)
                                                            : Async<bool> = async {
        let serializedChannel = channelStore.LoadChannel channelId
        let currency = (channelStore.Account :> IAccount).Currency
        let fundingScriptCoin = serializedChannel.FundingScriptCoin()
        let fundingDestination = fundingScriptCoin.ScriptPubKey.GetDestination()
        let network = UtxoCoin.Account.GetNetwork currency
        let fundingAddress: BitcoinAddress = fundingDestination.GetAddress network
        let fundingAddressString: string = fundingAddress.ToString()

        let scriptHash =
            Account.GetElectrumScriptHashFromPublicAddress
                currency
                fundingAddressString

        let! historyList =
            Server.Query currency
                         (QuerySettings.Default ServerSelectionMode.Fast)
                         (ElectrumClient.GetBlockchainScriptHashHistory scriptHash)
                         None

        let checkUnresolvedHtlcs (spendingTxInfo: BlockchainScriptHashHistoryInnerResult) : Async<bool> =
            async {
                if spendingTxInfo.Height > 0u then
                    let spendingTxId = spendingTxInfo.TxHash

                    let! spendingTxString =
                        Server.Query
                            currency
                            (QuerySettings.Default ServerSelectionMode.Fast)
                            (ElectrumClient.GetBlockchainTransaction spendingTxId)
                            None

                    let spendingTx =
                        Transaction.Parse(spendingTxString, network)
                    let obscuredCommitmentNumberOpt =
                        tryGetObscuredCommitmentNumber fundingScriptCoin.Outpoint spendingTx
                    match obscuredCommitmentNumberOpt with
                    | Ok _obscuredCommitmentNumber ->
                        let! serializedHtlcsData =
                            async {
                                let invoiceStore =
                                    (InvoiceDataStore channelStore.Account)
                                        .LoadInvoiceData()

                                let hash2preimage hash =
                                    let invoiceOpt = invoiceStore.TryGetInvoice hash
                                    invoiceOpt
                                    |> Option.map(fun invoice -> invoice.PaymentPreimage)

                                let unresolvedHtlcsList =
                                    UnresolvedHtlcList
                                        serializedChannel.SavedChannelState
                                        serializedChannel.RemoteNextCommitInfo
                                        spendingTx
                                        hash2preimage

                                let GetElectrumScriptHashFromScriptPubKey (scriptPubKey: Script) =
                                    let sha = NBitcoin.Crypto.Hashes.SHA256(scriptPubKey.ToBytes())
                                    let reversedSha = sha.Reverse().ToArray()
                                    NBitcoin.DataEncoders.Encoders.Hex.EncodeData reversedSha

                                let rec removeSpentOutputs unresolvedHtlcsList unspentHtlcList =
                                    async {
                                        match unresolvedHtlcsList with
                                        | [] ->
                                            return unspentHtlcList
                                        | head::tail ->
                                            match head with
                                            | HtlcTransaction.Timeout (_, spk, _)
                                            | HtlcTransaction.Success (_, spk, _) ->
                                                let job =  GetElectrumScriptHashFromScriptPubKey spk |> ElectrumClient.GetUnspentTransactionOutputs
                                                let! utxos = Server.Query currency (QuerySettings.Default ServerSelectionMode.Fast) job None
                                                if utxos |> Seq.isEmpty then
                                                    return! removeSpentOutputs tail unspentHtlcList
                                                else
                                                    return! removeSpentOutputs tail (head :: unspentHtlcList)
                                            | HtlcTransaction.Penalty _redeemScript ->
                                                return! removeSpentOutputs tail (head :: unspentHtlcList)
                                    }

                                let! unresolvedHtlcsList =
                                    removeSpentOutputs unresolvedHtlcsList List.empty

                                return
                                    {
                                        ChannelId = channelId
                                        ClosingTx = spendingTxString
                                        ChannelHtlcsData = unresolvedHtlcsList
                                    }
                            }

                        (HtlcsDataStore channelStore.Account)
                            .SaveHtlcsData serializedHtlcsData
                        return true
                    | _ -> return false
                else
                    return false
            }

        let! result =
            historyList
            |> List.map checkUnresolvedHtlcs
            |> Async.Parallel

        return Array.exists id result
    }

    let CheckForForceCloseAndSaveUnresolvedHtlcs (accounts: seq<UtxoCoin.NormalUtxoAccount>)
                                                     : seq<Async<bool>> =
        seq {
            for account in accounts do
                let channelStore = ChannelStore account
                let channelIds = channelStore.ListChannelIds()

                for channelId in channelIds do
                    yield
                        CheckForChannelForceCloseAndSaveUnresolvedHtlcs channelId channelStore
        }

    let CheckForChannelReadyToBroadcastHtlcTransactions (channelId: ChannelIdentifier)
                                                 (channelStore: ChannelStore)
                                                     : Async<HtlcTxsList> = async {
        let currency = (channelStore.Account :> IAccount).Currency
        let network = UtxoCoin.Account.GetNetwork currency

        let! blockHeight =
            async {
                let! blockHeightResponse =
                    Server.Query currency
                        (QuerySettings.Default ServerSelectionMode.Fast)
                        (ElectrumClient.SubscribeHeaders ())
                        None
                return (uint32 blockHeightResponse.Height)
            }

        let htlcsData =
            (HtlcsDataStore channelStore.Account)
                .LoadHtlcsData(channelId)

        let readyHtlcs =
            htlcsData.ChannelHtlcsData
            |> List.choose (fun htlc ->
                match htlc with
                | HtlcTransaction.Timeout (_, _, cltvExpiry) when blockHeight > cltvExpiry ->
                    Some htlc
                | HtlcTransaction.Penalty _
                | HtlcTransaction.Success _ ->
                    Some htlc
                | _ -> None
            )

        return
            {
                HtlcTxsList.ChannelId = channelId
                ClosingTx = Transaction.Parse(htlcsData.ClosingTx, network)
                Currency = (channelStore.Account :> IAccount).Currency
                Transactions = readyHtlcs
                Done = htlcsData.ChannelHtlcsData.IsEmpty
            }
    }

    let CheckForReadyToBroadcastHtlcTransactions (accounts: seq<UtxoCoin.NormalUtxoAccount>): seq<Async<HtlcTxsList>> =
        seq {
            for account in accounts do
                let channelStore = ChannelStore account
                let channelIds = channelStore.ListChannelIds()

                for channelId in channelIds do
                    yield
                        CheckForChannelReadyToBroadcastHtlcTransactions channelId channelStore
        }

    let CheckForReadyToSpendDelayedHtlcTransactions (channelId: ChannelIdentifier)
                                                    (channelStore: ChannelStore)
                                                        : Async<List<TransactionIdentifier>> = async {
        let serializedChannel = channelStore.LoadChannel channelId
        let currency = (channelStore.Account :> IAccount).Currency
        let toSelfDelay = serializedChannel.SavedChannelState.StaticChannelConfig.RemoteParams.ToSelfDelay.Value

        let delayedTxs = serializedChannel.HtlcDelayedTxs

        let rec checkForConfirmations (transactionsList: List<TransactionIdentifier>) (readyToSpend: List<TransactionIdentifier>) =
            async {
                match transactionsList with
                | [] ->
                    return readyToSpend
                | head::tail ->
                    let! confirmationCount =
                        UtxoCoin.Server.Query
                            currency
                            (UtxoCoin.QuerySettings.Default ServerSelectionMode.Fast)
                            (UtxoCoin.ElectrumClient.GetConfirmations (head.ToString()))
                            None
                    if confirmationCount >= uint32 toSelfDelay then
                        return! checkForConfirmations tail (head::readyToSpend)
                    else
                        return! checkForConfirmations tail readyToSpend
                    
            }

        return! checkForConfirmations delayedTxs List.empty
    }