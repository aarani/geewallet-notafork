namespace GWallet.Backend.UtxoCoin.Lightning

open DotNetLightning.Serialization
open DotNetLightning.Serialization.Msgs
open ResultUtils.Portability

open GWallet.Backend.FSharpUtil

module Validation =
    type internal InitMsgValidationError =
        | NoAnchorSupport
        interface IErrorMsg with
            member self.Message =
                match self with
                | NoAnchorSupport ->
                    "no anchor channel support found"
            member __.ChannelBreakdown: bool =
                false

    let internal ValidateRemoteInitMsg (remoteInit: InitMsg): Result<unit, InitMsgValidationError> =
        let hasAmchorSupport =
            remoteInit.Features.HasFeature Feature.OptionAnchorZeroFeeHtlcTx

        if not hasAmchorSupport then
            Error InitMsgValidationError.NoAnchorSupport
        else
            Ok ()
