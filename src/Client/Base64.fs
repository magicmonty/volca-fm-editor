[<RequireQualifiedAccess>]
module Base64

open Fable.Core

[<Emit("atob($0)")>]
let atob (s: string) : string = jsNative

[<Emit("btoa($0)")>]
let btoa (s: string) : string = jsNative

let toByteArray (s: string) : byte array =
    let byteChars = atob s
    match byteChars.Length with
    | 0 -> [||]
    | l -> [| for i in [0..(l - 1)] do yield byteChars.[i] |> byte |]

let fromByteArray (d: byte array) : string =
    let byteString = d |> Array.map char |> Array.fold (fun c n -> sprintf "%s%c" c n) ""
    btoa byteString
