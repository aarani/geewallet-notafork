namespace GWallet.Backend

open System
open System.IO
open System.Security.Cryptography
open System.Text

open Newtonsoft.Json

module SymmetricEncryptionManager =
    type PrivateSeedInfo =
        {
            PrivateKey: string
            RecoveryPhrase: string
        }

    type EncryptedSeedInfo =
        {
            DecryptionIV: string
            CipherText: string
        }

    let private Encrypt (plainText: string) (password: string) =
        let passwordBytes = Encoding.UTF8.GetBytes password
        let plainTextBytes = Encoding.UTF8.GetBytes plainText

        use aes =
            Aes.Create(Key = NBitcoin.Crypto.Hashes.SHA256 passwordBytes)

        aes.GenerateIV()
        let encryptor = aes.CreateEncryptor(aes.Key, aes.IV)
        use ms = new MemoryStream()

        use cs =
            new CryptoStream(ms, encryptor, CryptoStreamMode.Write)

        cs.Write(plainTextBytes, 0, plainTextBytes.Length)
        cs.FlushFinalBlock()
        let encryptedData = ms.ToArray()

        {
            DecryptionIV = Convert.ToBase64String aes.IV
            CipherText = Convert.ToBase64String encryptedData
        }
        |> JsonConvert.SerializeObject

    let private Decrypt (encryptedInfo: string) (password: string) =
        let passwordBytes = Encoding.UTF8.GetBytes password

        let encryptedData, decryptionIv = 
            let encryptedInfoObj =
                encryptedInfo 
                |> JsonConvert.DeserializeObject<EncryptedSeedInfo>

            encryptedInfoObj.CipherText
            |> Convert.FromBase64String,
            encryptedInfoObj.DecryptionIV 
            |> Convert.FromBase64String

        use aes =
            Aes.Create(Key = NBitcoin.Crypto.Hashes.SHA256 passwordBytes, IV = decryptionIv)

        let decryptor = aes.CreateDecryptor(aes.Key, aes.IV)
        use ms = new MemoryStream()

        use cs =
            new CryptoStream(ms, decryptor, CryptoStreamMode.Write)

        cs.Write(encryptedData, 0, encryptedData.Length)
        cs.FlushFinalBlock()

        ms.ToArray() |> Encoding.UTF8.GetString

    let Save (keyBytes: array<byte>) (recoveryPhraseBytes: array<byte>) (password: string) = 
        {
            PrivateKey = keyBytes |> Convert.ToBase64String
            RecoveryPhrase = recoveryPhraseBytes |> Convert.ToBase64String
        }
        |> JsonConvert.SerializeObject
        |> Encrypt password

    let Load (fileContent: string) (password: string) =
        let key =
            Decrypt fileContent password
            |> JsonConvert.DeserializeObject<PrivateSeedInfo>
        key.PrivateKey |> Convert.FromBase64String,
        key.RecoveryPhrase |> Convert.FromBase64String
