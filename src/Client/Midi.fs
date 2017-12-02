module Midi

open Fable.Core
open Fable.Import.JS
open Fable.PowerPack
open Fable.Option
open Fable.Core.JsInterop
open Fable.Import

type MIDIOptions =
    | Sysex of bool
    | Software of bool

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
    [<Emit("$0.id")>]
    abstract Id: string with get
    [<Emit("$0.manufacturer")>]
    abstract Manufacturer: string option with get
    [<Emit("$0.name")>]
    abstract Name: string option with get
    [<Emit("$0.type")>]
    abstract Type: MIDIPortType with get
    [<Emit("$0.version")>]
    abstract Version: string option with get
    [<Emit("$0.state")>]
    abstract State: MIDIPortDeviceState with get
    [<Emit("$0.connection")>]
    abstract Connection: MIDIPortConnectionState with get
    [<Emit("$0.OnStateChange")>]
    abstract OnStateChange: obj with get, set
    [<Emit("$0.open()")>]
    abstract Open: (unit -> Promise<IMIDIPort>)
    [<Emit("$0.close()")>]
    abstract Close: (unit -> Promise<IMIDIPort>)

type IMIDIMessageEvent = obj

type IMIDIConnectionEvent = obj

type IMIDIInput =
    inherit IMIDIPort
    [<Emit("$0.onmidimessage")>]
    abstract OnMidiMessage: obj with get

type IMIDIInputMap = (string*IMIDIInput) list

type IMIDIOutput =
    inherit IMIDIPort
    [<Emit("$0.send($1)")>]
    abstract Send: (byte array -> unit)
    [<Emit("$0.clear($1)")>]
    abstract Clear: (unit -> unit)

type IMIDIOutputMap = (string*IMIDIOutput) list

type IMIDIAccess =
    [<Emit("$0.inputs")>]
    abstract Inputs: IMIDIInputMap with get
    [<Emit("$0.outputs")>]
    abstract Outputs: IMIDIOutputMap with get
    [<Emit("$0.onstatechange")>]
    abstract OnStateChange: obj with get, set
    [<Emit("$0.sysexEnabled")>]
    abstract SysexEnabled: bool with get, set

module internal Intern =

    [<Emit("navigator.requestMIDIAccess($0)")>]
    let requestMIDIAccess options : Promise<obj> = jsNative

module P = Fable.PowerPack.Promise


[<RequireQualifiedAccess>]
module MIDI =
    [<Emit("navigator.requestMIDIAccess($2)")>]
    let requestMIDIAccess options : Promise<obj> = jsNative

    let requestAccess (options: MIDIOptions list) =
        promise {
            let! midiAccess = Intern.requestMIDIAccess (keyValueList CaseRules.LowerFirst options)
            return midiAccess
        }

    let send (output: obj) (data : uint8 array) =
        promise {            
            !! output?send(data)
        }

