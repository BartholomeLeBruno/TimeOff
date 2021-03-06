module ServerCode.Auth.JsonWebToken

//  Learn about JWT https://jwt.io/introduction/
//  This module uses the JOSE-JWT library https://github.com/dvsekhvalnov/jose-jwt

open System.IO
open Newtonsoft.Json

let private createPassPhrase() =
    let crypto = System.Security.Cryptography.RandomNumberGenerator.Create()
    let randomNumber = Array.init 32 byte
    crypto.GetBytes(randomNumber)
    randomNumber

let private passPhrase =
    let fi = FileInfo("./.jwt/token.txt")
    if not fi.Exists then
        let passPhrase = createPassPhrase()
        if not fi.Directory.Exists then
            fi.Directory.Create()
        File.WriteAllBytes(fi.FullName,passPhrase)
    File.ReadAllBytes(fi.FullName)

let private encodeString (payload:string) =
    Jose.JWT.Encode(payload, passPhrase, Jose.JweAlgorithm.A256KW, Jose.JweEncryption.A256CBC_HS512)

let private decodeString (jwt:string) =
    Jose.JWT.Decode(jwt, passPhrase, Jose.JweAlgorithm.A256KW, Jose.JweEncryption.A256CBC_HS512)

let encode token =
    JsonConvert.SerializeObject token
    |> encodeString

let decode<'a> (jwt:string) : 'a =
    decodeString jwt
    |> JsonConvert.DeserializeObject<'a>