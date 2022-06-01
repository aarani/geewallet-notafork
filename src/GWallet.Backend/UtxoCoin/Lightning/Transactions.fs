﻿namespace GWallet.Backend.UtxoCoin.Lightning

open NBitcoin
open DotNetLightning.Channel.ClosingHelpers

open GWallet.Backend
open GWallet.Backend.UtxoCoin

type MutualCloseTx =
    {
        Tx: UtxoTransaction
    }

type ForceCloseTx =
    {
        Tx: UtxoTransaction
    }

type ClosingTx =
    | MutualClose of MutualCloseTx
    | ForceClose of ForceCloseTx

type MutualCloseCpfp =
    {
        ChannelId: ChannelIdentifier
        Currency: Currency
        Tx: UtxoTransaction
        Fee: MinerFee
    }

type FeeBumpTx =
    {
        ChannelId: ChannelIdentifier
        Currency: Currency
        Tx: UtxoTransaction
        Fee: MinerFee
    }

type RecoveryTx =
    {
        ChannelId: ChannelIdentifier
        Currency: Currency
        Tx: UtxoTransaction
        Fee: MinerFee
    }

type HtlcTx =
    {
        ChannelId: ChannelIdentifier
        Currency: Currency
        Tx: UtxoTransaction
        NeedsRecoveryTx: bool
        Fee: MinerFee
        AmountInSatoshis: int64
    }

    /// Returns true if htlc amount is less than or equal to the fees needed to spend it
    member self.IsDust () =
        if not self.NeedsRecoveryTx then
            self.AmountInSatoshis <= self.Fee.EstimatedFeeInSatoshis
        else
            let previousSize = self.Tx.NBitcoinTx.GetVirtualSize() |> double
            //FIXME: hardcoded value
            let newSize = previousSize + 273.
            self.AmountInSatoshis <= ((((self.Fee.EstimatedFeeInSatoshis |> double) / previousSize) * newSize) |> System.Convert.ToInt64)

type HtlcTxsList =
    internal {
        ChannelId: ChannelIdentifier
        ClosingTx: Transaction
        Currency: Currency
        Transactions: list<HtlcTransaction>
        Done: bool
    }

    member self.IsEmpty () =
        Seq.isEmpty self.Transactions
    member self.IsDone () =
        self.Done