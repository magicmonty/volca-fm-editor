module Midi

open Fable.Core
open Fable.Import.JS
open Fable.PowerPack

let sendSysex (data : byte array) =
    data |> string |> printfn "%A"


[<Emit("navigator.requestMIDIAccess({ sysex: true })")>]
let internal requestMIDIAccessInternal () = jsNative

let requestMIDIAccess () =
    promise {
        requestMIDIAccessInternal ()
    }
