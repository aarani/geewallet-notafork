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

        //FIXME: use PBKDF or SCrypt here
        use aes =
            Aes.Create(Key = NBitcoin.Crypto.Hashes.SHA256 passwordBytes)

        aes.GenerateIV()
        let encryptor = aes.CreateEncryptor(aes.Key, aes.IV)
        use memStream = new MemoryStream()

        use cryptoStream =
            new CryptoStream(memStream, encryptor, CryptoStreamMode.Write)

        cryptoStream.Write(plainTextBytes, 0, plainTextBytes.Length)
        cryptoStream.FlushFinalBlock()
        let encryptedData = memStream.ToArray()

        {
            DecryptionIV = Convert.ToBase64String aes.IV
            CipherText = Convert.ToBase64String encryptedData
        }
        |> JsonConvert.SerializeObject

    let private Decrypt (encryptedInfo: string) (password: string) =
        let passwordBytes = Encoding.UTF8.GetBytes password

        let encryptedData, decryptionIv = 
            let encryptedInfoObj =
                JsonConvert.DeserializeObject<EncryptedSeedInfo> encryptedInfo

            encryptedInfoObj.CipherText
            |> Convert.FromBase64String,
            encryptedInfoObj.DecryptionIV 
            |> Convert.FromBase64String

        use aes =
            Aes.Create(Key = NBitcoin.Crypto.Hashes.SHA256 passwordBytes, IV = decryptionIv)

        let decryptor = aes.CreateDecryptor(aes.Key, aes.IV)
        use memStream = new MemoryStream()

        use cryptoStream =
            new CryptoStream(memStream, decryptor, CryptoStreamMode.Write)

        cryptoStream.Write(encryptedData, 0, encryptedData.Length)
        cryptoStream.FlushFinalBlock()

        memStream.ToArray() |> Encoding.UTF8.GetString

    let Save (keyBytes: array<byte>) (recoveryPhrase: string) (password: string) =
        let seedInfoJson =
            {
                PrivateKey = keyBytes |> Convert.ToBase64String
                RecoveryPhrase = recoveryPhrase
            }
            |> JsonConvert.SerializeObject

        Encrypt seedInfoJson password

    let Load (fileContent: string) (password: string) =
        //FIXME:
        //  There is a chance that aes decryption with invalid key
        //  don't throw CryptographicException, we might need to look
        //  for Json parsing errors or add digests to make sure the decrypted
        //  plaintext is correct
        let seedInfo =
            JsonConvert.DeserializeObject<PrivateSeedInfo>
                (Decrypt fileContent password)
        seedInfo.PrivateKey |> Convert.FromBase64String,
        seedInfo.RecoveryPhrase
