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
            Salt: string
            DecryptionIV: string
            CipherText: string
        }

    let ScryptSaltSizeInBytes = 32
    let AesKeySizeInBytes = 32

    let private Encrypt (plainText: string) (password: string) =
        let passwordBytes = Encoding.UTF8.GetBytes password
        let plainTextBytes = Encoding.UTF8.GetBytes plainText
        use secureRandomEngine = new RNGCryptoServiceProvider()

        let salt = Array.zeroCreate ScryptSaltSizeInBytes
        secureRandomEngine.GetNonZeroBytes salt

        let keyBytes = NBitcoin.Crypto.SCrypt.BitcoinComputeDerivedKey (passwordBytes, salt, AesKeySizeInBytes)

        use aes =
            Aes.Create(Key = keyBytes)
        aes.GenerateIV()

        if aes.KeySize <> AesKeySizeInBytes * 8 then
            failwith "Invalid key size, aes.KeySize should be == 256"

        let encryptor = aes.CreateEncryptor(aes.Key, aes.IV)
        use memStream = new MemoryStream()

        use cryptoStream =
            new CryptoStream(memStream, encryptor, CryptoStreamMode.Write)

        cryptoStream.Write(plainTextBytes, 0, plainTextBytes.Length)
        cryptoStream.FlushFinalBlock()
        let encryptedData = memStream.ToArray()

        {
            Salt = Convert.ToBase64String salt
            DecryptionIV = Convert.ToBase64String aes.IV
            CipherText = Convert.ToBase64String encryptedData
        }
        |> JsonConvert.SerializeObject

    let private Decrypt (encryptedInfo: EncryptedSeedInfo) (password: string) =
        let passwordBytes = Encoding.UTF8.GetBytes password

        let encryptedData, decryptionIv, salt =
            encryptedInfo.CipherText
            |> Convert.FromBase64String,
            encryptedInfo.DecryptionIV
            |> Convert.FromBase64String,
            encryptedInfo.Salt
            |> Convert.FromBase64String

        let keyBytes = NBitcoin.Crypto.SCrypt.BitcoinComputeDerivedKey (passwordBytes, salt, AesKeySizeInBytes)

        use aes =
            Aes.Create(Key = keyBytes, IV = decryptionIv)

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

    let Load (encryptedSeedInfo: EncryptedSeedInfo) (password: string) =
        //FIXME:
        //  There is a chance that aes decryption with invalid key
        //  don't throw CryptographicException, we might need to look
        //  for Json parsing errors or add digests to make sure the decrypted
        //  plaintext is correct
        let seedInfo =
            JsonConvert.DeserializeObject<PrivateSeedInfo>
                (Decrypt encryptedSeedInfo password)
        seedInfo.PrivateKey |> Convert.FromBase64String,
        seedInfo.RecoveryPhrase
