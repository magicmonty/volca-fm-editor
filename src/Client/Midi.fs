module Midi

open Fable.Core
open Fable.Import.JS
open Fable.PowerPack
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
    [<Emit("$0.onstatechange")>]
    abstract OnStateChange: obj with get, set
    [<Emit("$0.open")>]
    abstract Open: (unit -> Promise<IMIDIPort>)
    [<Emit("$0.close")>]
    abstract Close: (unit -> Promise<IMIDIPort>)

type IMIDIMessageEvent = obj

type IMIDIInput =
    inherit IMIDIPort
    [<Emit("$0.onmidimessage")>]
    abstract OnMidiMessage: obj with get

type IMIDIInputMap = JS.Map<string, IMIDIInput>

type IMIDIOutput =
    inherit IMIDIPort
    [<Emit("$0.send")>]
    abstract Send: (uint8 array -> unit)
    [<Emit("$0.clear")>]
    abstract Clear: (unit -> unit)

type IMIDIOutputMap = JS.Map<string, IMIDIOutput>

type IMIDIAccess =
    [<Emit("$0.inputs")>]
    abstract Inputs: IMIDIInputMap with get
    [<Emit("$0.outputs")>]
    abstract Outputs: IMIDIOutputMap with get
    [<Emit("$0.onstatechange")>]
    abstract OnStateChange: obj with get, set
    [<Emit("$0.sysexEnabled")>]
    abstract SysexEnabled: bool with get, set

type IMIDIConnectionEvent = 
    [<Emit("$0.port")>]
    abstract Port: IMIDIPort with get
    [<Emit("$0.target")>]
    abstract Target: IMIDIAccess with get

module internal Intern =

    [<Emit("navigator.requestMIDIAccess($0)")>]
    let requestMIDIAccess options : Promise<IMIDIAccess> = jsNative

module P = Fable.PowerPack.Promise

module Map =
    let toList (m: JS.Map<'a,'b>): ('a*'b) list =
        let mutable result : ('a*'b) list = []
        m.forEach (fun b a _ -> result <- (a, b) :: result)
        result |> List.ofSeq

module ArrayBuffer =
    let toArray (m: ArrayBuffer) : byte array =
        let c = JS.Uint8ClampedArray.Create(m)
        let mutable result : byte list = []
        c.forEach (fun a _ _ -> result <- (byte a) :: result)
        result |> List.rev |> List.toArray

[<RequireQualifiedAccess>]
module MIDI =
    let requestAccess (options: MIDIOptions list) =
        promise {
            let! midiAccess = Intern.requestMIDIAccess (JsInterop.keyValueList CaseRules.LowerFirst options)
            return midiAccess
        }

    let send (output: IMIDIOutput) (data : uint8 array) =
        promise {
            output.Send data
        }

