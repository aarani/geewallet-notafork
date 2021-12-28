namespace GWallet.Backend.UtxoCoin.Lightning

open System
open System.Web
open System.Net

open NOnion.Network
open NOnion.Services

open GWallet.Backend
open GWallet.Backend.FSharpUtil.UwpHacks

type NodeNOnionIntroductionPoint =
    { NodeId: PublicKey
      IntroductionPointPublicInfo: IntroductionPointPublicInfo }

    static member IsNOnionConnection(text: string) : bool =
        text.StartsWith "geewallet+nonion://"

    static member Parse (currency: Currency) (text: string) : NodeNOnionIntroductionPoint =
        if not (NodeNOnionIntroductionPoint.IsNOnionConnection text) then
            raise <| FormatException "Not an onion address"

        let uri = System.Uri(text)
        let kvps = HttpUtility.ParseQueryString uri.Query

        let encryptionKey = kvps.["EncryptionKey"]
        let authKey = kvps.["AuthKey"]
        let onionKey = kvps.["OnionKey"]
        let fingerPrint = kvps.["Fingerprint"]
        let masterPublicKey = kvps.["MasterPublicKey"]

        let replaceForDecode (text:string) =
            text.Replace('.','+').Replace('_','/').Replace('-','=')
        let decodedOnionKey = replaceForDecode onionKey
        { NodeId = PublicKey.Parse currency uri.UserInfo
          IntroductionPointPublicInfo =
              { Address = uri.Host
                Port = uri.Port
                EncryptionKey = replaceForDecode encryptionKey
                AuthKey = replaceForDecode authKey
                OnionKey = decodedOnionKey
                Fingerprint = replaceForDecode fingerPrint
                MasterPublicKey = replaceForDecode masterPublicKey } }

    override self.ToString() =
        let replaceForEncode (text:string) =
            text.Replace('+','.').Replace('/','_').Replace('=','-')
        let introPoint = self.IntroductionPointPublicInfo
        let encodedOnionKey = replaceForEncode self.IntroductionPointPublicInfo.OnionKey
        SPrintF8 "geewallet+nonion://%s@%s:%d?EncryptionKey=%s&AuthKey=%s&OnionKey=%s&Fingerprint=%s&MasterPublicKey=%s"
                        (self.NodeId.ToString()) introPoint.Address introPoint.Port
                        (replaceForEncode introPoint.EncryptionKey)
                        (replaceForEncode introPoint.AuthKey)
                        (encodedOnionKey)
                        (replaceForEncode introPoint.Fingerprint)
                        (replaceForEncode introPoint.MasterPublicKey)

type NodeIdentifier =
    | EndPoint of NodeEndPoint
    | NOnionIntroductionPoint of NodeNOnionIntroductionPoint

type NodeClientType =
    | TcpClient
    | TorClient
