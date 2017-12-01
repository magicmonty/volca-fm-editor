module Midi

open Fable.Core
open Fable.Import.JS
open Fable.PowerPack
open Fable.Option
open Fable.Core.JsInterop
open Fable.Import.Browser

type IMIDIOptions =
    abstract sysex: bool with get, set
    abstract software: bool with get, set

[<StringEnum>]
type MIDIPortType =
    | Input
    | Output

[<StringEnum>]
type MIDIPortDeviceState =
    | Disconnected
    | Connected

[<StringEnum>]
type MIDIPortConnectionState =
    | Open
    | Closed
    | Pending

type IMIDIPort = 
    abstract id: string with get
    abstract manufacturer: string option with get
    abstract name: string option with get
    abstract ``type``: MIDIPortType with get
    abstract version: string option with get
    abstract state: MIDIPortDeviceState with get
    abstract connection: MIDIPortConnectionState with get
    abstract onstatechange: (IMIDIPort -> unit)
    abstract ``open``: (unit -> Promise<IMIDIPort>)
    abstract close: (unit -> Promise<IMIDIPort>)

type IMIDIMessageEvent =
    abstract data: byte list

type IMIDIInput =
    inherit IMIDIPort
    abstract onmidimessage: (IMIDIMessageEvent -> unit)

type IMIDIInputMap =
    abstract size: int with get
    abstract values: IMIDIInput list

type IMIDIOutput =
    inherit IMIDIPort
    abstract send: (byte list -> unit)
    abstract clear: (unit -> unit)

type IMIDIOutputMap =
    abstract size: int with get
    abstract values: IMIDIOutput list

type IMIDIAccess =
    abstract inputs: IMIDIInput list
    abstract outputs: IMIDIOutput list
    abstract onstatechange: (IMIDIPort -> unit)
    abstract sysexEnabled: bool

module internal Intern =

    [<Emit("navigator.requestMIDIAccess($1)")>]
    let requestMIDIAccess (options: IMIDIOptions) : Promise<IMIDIAccess> = jsNative

type MIDIOptions = { SysEx: bool option
                     Software: bool option }


let requestMIDIAccess (options: MIDIOptions) =
    let data = createEmpty<IMIDIOptions>
    match options.SysEx with
    | Some v -> data.sysex <- v
    | _ -> ()

    match options.Software with
    | Some v -> data.software <- v
    | _ -> ()
    
    Intern.requestMIDIAccess data

let sendSysex (data : byte array) =
    data |> string |> printfn "%A"

