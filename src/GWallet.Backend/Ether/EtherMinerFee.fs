﻿namespace GWallet.Backend.Ether

open System
open System.Numerics

open GWallet.Backend

open Nethereum.Util

type MinerFee(gasLimit: Int64, gasPriceInWei: Int64, estimationTime: DateTime, currency: Currency) =

    member val GasLimit = gasLimit with get
    member val GasPriceInWei = gasPriceInWei with get
    member val Currency = currency with get
    member val EstimationTime = estimationTime with get

    member __.CalculateAbsoluteValue() =
        let gasPriceInWei = BigInteger(gasPriceInWei)
        let costInWei = BigInteger.Multiply(gasPriceInWei, BigInteger(gasLimit))
        UnitConversion.Convert.FromWei(costInWei, UnitConversion.EthUnit.Ether)

    static member GetHigherFeeThanRidiculousFee (exchangeRateToFiat: decimal)

                                                //public nodes as in the equivalent ones to Electrum Servers
                                                (initialFeeWithAMinimumGasPriceInWeiDictatedByPublicNodes: MinerFee)

                                                //namely USD0.01 but let's pass it as param
                                                (smallestFiatFeeThatIsNoLongerRidiculous: decimal)
                                                =

        let initialAbsoluteValue = initialFeeWithAMinimumGasPriceInWeiDictatedByPublicNodes.CalculateAbsoluteValue()
        if initialAbsoluteValue * exchangeRateToFiat >= smallestFiatFeeThatIsNoLongerRidiculous then
            initialFeeWithAMinimumGasPriceInWeiDictatedByPublicNodes
        else
            let gasLimit = initialFeeWithAMinimumGasPriceInWeiDictatedByPublicNodes.GasLimit

            let biggerFee = smallestFiatFeeThatIsNoLongerRidiculous / exchangeRateToFiat
            let biggerFeeInWei = UnitConversion.Convert.ToWei(BigDecimal biggerFee, UnitConversion.EthUnit.Ether)
            let biggerGasPriceInWei: int64 = BigInteger.Divide(biggerFeeInWei, BigInteger(gasLimit))
                                             |> BigInteger.op_Explicit

            let estimationTime = initialFeeWithAMinimumGasPriceInWeiDictatedByPublicNodes.EstimationTime
            let currency = initialFeeWithAMinimumGasPriceInWeiDictatedByPublicNodes.Currency
            MinerFee(gasLimit, biggerGasPriceInWei, estimationTime, currency)

    interface IBlockchainFeeInfo with
        member self.FeeEstimationTime = self.EstimationTime
        member self.FeeValue = self.CalculateAbsoluteValue()
        member self.Currency = self.Currency
