module VolcaEditor.State

open Aether
open Aether.Operators
open Fable.Core.JsInterop
open Fable.Import
open WebMIDI
open Elmish
open VolcaFM

module S = Client.Style
module R = Fable.Helpers.React
module P = Fable.Helpers.React.Props
module K = VolcaEditor.Knob

module String =
  let isNotEmpty v = not (System.String.IsNullOrEmpty v)

let DoNothing _ = ()

type Model = { MidiEnabled : bool
               Patch : Patch
               Operator1Type : OperatorType
               Operator2Type : OperatorType
               Operator3Type : OperatorType
               Operator4Type : OperatorType
               Operator5Type : OperatorType
               Operator6Type : OperatorType
               ErrorMessage: string option
               MidiErrorMessage: string option
               MidiMessages : S.Alert list
               MidiOutputs : (string*IMIDIOutput) list
               SelectedMIDIOutput : string option
               MidiInputs : (string*IMIDIInput) list
               SelectedMIDIInput : string option
               SelectedMIDIChannel : byte
               FileToLoad : Browser.Blob option
               PermaLink : string }

             static member patch = (fun m -> m.Patch), (fun value m -> { m with Patch = value })

type OperatorMsg =
  | EnabledChanged of bool
  | EGRate1Changed of byte
  | EGRate2Changed of byte
  | EGRate3Changed of byte
  | EGRate4Changed of byte
  | EGLevel1Changed of byte
  | EGLevel2Changed of byte
  | EGLevel3Changed of byte
  | EGLevel4Changed of byte
  | LevelScaleBreakpointChanged of byte
  | LevelScaleLeftDepthChanged of byte
  | LevelScaleRightDepthChanged of byte
  | LevelScaleLeftCurveChanged of byte
  | LevelScaleRightCurveChanged of byte
  | OscillatorRateScaleChanged of byte
  | DetuneChanged of byte
  | FrequencyCoarseChanged of byte
  | FrequencyFineChanged of byte
  | OscillatorModeChanged of byte
  | AmpModSenseChanged of byte
  | KeyVelocitySenseChanged of byte
  | OperatorOutputLevelChanged of byte
  | OperatorSliderComplete

type Msg =
  | AlgorithmChanged of byte
  | FeedbackChanged of byte
  | OscillatorKeySyncChanged of byte
  | TransposeChanged of byte
  | PitchRate1Changed of byte
  | PitchRate2Changed of byte
  | PitchRate3Changed of byte
  | PitchRate4Changed of byte
  | PitchLevel1Changed of byte
  | PitchLevel2Changed of byte
  | PitchLevel3Changed of byte
  | PitchLevel4Changed of byte
  | LFOSpeedChanged of byte
  | LFOWaveShapeChanged of byte
  | LFOPitchModDepthChanged of byte
  | LFOAmpModDepthChanged of byte
  | LFODelayChanged of byte
  | LFOKeySyncChanged of byte
  | PitchModSensitivityChanged of byte
  | PatchNameChanged of string
  | SliderComplete
  | Operator1Msg of OperatorMsg
  | Operator2Msg of OperatorMsg
  | Operator3Msg of OperatorMsg
  | Operator4Msg of OperatorMsg
  | Operator5Msg of OperatorMsg
  | Operator6Msg of OperatorMsg
  | SendError of string
  | SendSuccess
  | MidiSuccess of IMIDIAccess
  | MidiStateChange of IMIDIAccess
  | MidiError of exn
  | MidiMessage of S.Alert
  | MidiOutputChanged of string
  | MidiInputChanged of string
  | MidiChannelChanged of byte
  | SendSysex
  | InitPatch
  | SavePatch
  | FileToLoadChanged of Browser.Blob list
  | LoadPatch
  | PatchLoaded of Patch
  | PermalinkChanged

let sysexData model = model.Patch |> toSysexMessage |> List.toArray

let makeBlob data =
  let parts : ResizeArray<obj> = new ResizeArray<obj>()
  parts.Add(data)
  Browser.Blob.Create(blobParts = parts)

let clickTemporaryLink url fileName =
  let a = Browser.document.createElement "a"
  a?href <- url
  a?download <- fileName
  Browser.document.body.appendChild a |> ignore
  a?click() |> ignore
  Browser.document.body.removeChild(a) |> ignore

let savePatch (patch: VolcaFM.Patch) =
  let file = patch |> toSysexMessage |> List.toArray |> makeBlob
  let url = Browser.window.URL.createObjectURL file
  let fileName = sprintf "%s_patch.syx" patch.PatchName

  clickTemporaryLink url fileName
  Browser.window.URL.revokeObjectURL url

let checkSysexFileLoad (theFile : Browser.Blob) =
  JS.Promise.Create(fun resolve _ ->
    let reader = Browser.FileReader.Create()
    reader.onload <-
      (fun (e: Browser.Event) ->
        let data : JS.ArrayBuffer = !! e.target?result
        let content = data |> ArrayBuffer.toArray
        let patch = match validateSysexData content with
                    | None ->
                      let p = loadPatch content
                      match p with
                      | Some p -> PatchLoaded p
                      | _ -> MidiMessage (S.Error "Could not load patch!")
                    | Some msg -> MidiMessage (S.Error msg)
        resolve.Invoke(Fable.Core.U2.Case1 patch)
        null)

    reader.readAsArrayBuffer theFile)


let updateOperatorTypes model =
  let op1, op2, op3, op4, op5, op6 = algorithms.[int model.Patch.Algorithm]

  { model with Operator1Type = op1
               Operator2Type = op2
               Operator3Type = op3
               Operator4Type = op4
               Operator5Type = op5
               Operator6Type = op6 }

let init () : Model*Cmd<Msg> =
  let search = match Browser.window.location.hash with
               | "" -> Browser.window.location.search
               | h -> h.Substring 1

  let patch =
    match search with
    | s when s.StartsWith "?patch=" && s.Length > 7 && not (s.Contains "&") ->
      s.Substring 7
      |> Patch.decode
      |> Option.defaultValue (initPatch ())
    | _ -> initPatch ()

  let m =
    { MidiEnabled = false
      Patch = patch
      Operator1Type = Carrier
      Operator2Type = Carrier
      Operator3Type = Carrier
      Operator4Type = Carrier
      Operator5Type = Carrier
      Operator6Type = Carrier
      ErrorMessage = None
      MidiErrorMessage = None
      MidiMessages = []
      MidiOutputs = []
      SelectedMIDIOutput = None
      MidiInputs = []
      SelectedMIDIInput = None
      SelectedMIDIChannel = 1uy
      FileToLoad = None
      PermaLink = patch |> Patch.encode }
    |> updateOperatorTypes

  m, (Cmd.ofPromise MIDI.requestAccess [ Sysex true ] MidiSuccess MidiError)

let updateOperator msg model op: Model =
  let operator = Model.patch >-> op

  match msg with
  | EnabledChanged v -> model |> Optic.set (operator >-> Operator.enabled) v
  | EGRate1Changed v -> model |> Optic.set (operator >-> Operator.eGRate1) v
  | EGRate2Changed v -> model |> Optic.set (operator >-> Operator.eGRate2) v
  | EGRate3Changed v -> model |> Optic.set (operator >-> Operator.eGRate3) v
  | EGRate4Changed v -> model |> Optic.set (operator >-> Operator.eGRate4) v
  | EGLevel1Changed v -> model |> Optic.set (operator >-> Operator.eGLevel1) v
  | EGLevel2Changed v -> model |> Optic.set (operator >-> Operator.eGLevel2) v
  | EGLevel3Changed v -> model |> Optic.set (operator >-> Operator.eGLevel3) v
  | EGLevel4Changed v -> model |> Optic.set (operator >-> Operator.eGLevel4) v
  | LevelScaleBreakpointChanged v -> model |> Optic.set (operator >-> Operator.levelScaleBreakpoint) v
  | LevelScaleLeftDepthChanged v -> model |> Optic.set (operator >-> Operator.levelScaleLeftDepth) v
  | LevelScaleRightDepthChanged v -> model |> Optic.set (operator >-> Operator.levelScaleRightDepth) v
  | LevelScaleLeftCurveChanged v -> model |> Optic.set (operator >-> Operator.levelScaleLeftCurve) v
  | LevelScaleRightCurveChanged v -> model |> Optic.set (operator >-> Operator.levelScaleRightCurve) v
  | OscillatorRateScaleChanged v -> model |> Optic.set (operator >-> Operator.oscillatorRateScale) v
  | DetuneChanged v -> model |> Optic.set (operator >-> Operator.detune) v
  | FrequencyCoarseChanged v -> model |> Optic.set (operator >-> Operator.frequencyCoarse) v
  | FrequencyFineChanged v -> model |> Optic.set (operator >-> Operator.frequencyFine) v
  | OscillatorModeChanged v -> model |> Optic.set (operator >-> Operator.oscillatorMode) v
  | AmpModSenseChanged v -> model |> Optic.set (operator >-> Operator.ampModSense) v
  | KeyVelocitySenseChanged v -> model |> Optic.set (operator >-> Operator.keyVelocitySense) v
  | OperatorOutputLevelChanged v -> model |> Optic.set (operator >-> Operator.operatorOutputLevel) v
  | OperatorSliderComplete -> model

let onStateChange (ev: IMIDIConnectionEvent) =
  Browser.console.log ev

let update (msg: Msg) (model: Model) : Model*Cmd<Msg> =
  let patch = Model.patch
  let success msg = Cmd.ofMsg (MidiMessage (S.Success msg))
  let error msg = Cmd.ofMsg (MidiMessage (S.Error msg))
  let info msg = Cmd.ofMsg (MidiMessage (S.Info msg))
  let warning  msg = Cmd.ofMsg (MidiMessage (S.Warning msg))

  match msg with
  | AlgorithmChanged v -> (model |> Optic.set (patch >-> Patch.algorithm) v |> updateOperatorTypes), Cmd.none
  | FeedbackChanged v -> (model |> Optic.set (patch >-> Patch.feedback) v), Cmd.none
  | OscillatorKeySyncChanged v -> (model |> Optic.set (patch >-> Patch.oscillatorKeySync) v), Cmd.none
  | TransposeChanged v -> (model |> Optic.set (patch >-> Patch.transpose) v), Cmd.none
  | PitchRate1Changed v -> (model |> Optic.set (patch >-> Patch.pitchRate1) v), Cmd.none
  | PitchRate2Changed v -> (model |> Optic.set (patch >-> Patch.pitchRate2) v), Cmd.none
  | PitchRate3Changed v -> (model |> Optic.set (patch >-> Patch.pitchRate3) v), Cmd.none
  | PitchRate4Changed v -> (model |> Optic.set (patch >-> Patch.pitchRate4) v), Cmd.none
  | PitchLevel1Changed v -> (model |> Optic.set (patch >-> Patch.pitchLevel1) v), Cmd.none
  | PitchLevel2Changed v -> (model |> Optic.set (patch >-> Patch.pitchLevel2) v), Cmd.none
  | PitchLevel3Changed v -> (model |> Optic.set (patch >-> Patch.pitchLevel3) v), Cmd.none
  | PitchLevel4Changed v -> (model |> Optic.set (patch >-> Patch.pitchLevel4) v), Cmd.none
  | LFOSpeedChanged v -> (model |> Optic.set (patch >-> Patch.lFOSpeed) v), Cmd.none
  | LFOWaveShapeChanged v -> (model |> Optic.set (patch >-> Patch.lFOWaveShape) v), Cmd.none
  | LFOPitchModDepthChanged v -> (model |> Optic.set (patch >-> Patch.lFOPitchModDepth) v), Cmd.none
  | LFOAmpModDepthChanged v -> (model |> Optic.set (patch >-> Patch.lFOAmpModDepth) v), Cmd.none
  | LFODelayChanged v -> (model |> Optic.set (patch >-> Patch.lFODelay) v), Cmd.none
  | LFOKeySyncChanged v -> (model |> Optic.set (patch >-> Patch.lFOKeySync) v), Cmd.none
  | PitchModSensitivityChanged v -> (model |> Optic.set (patch >-> Patch.pitchModSensitivity) v), Cmd.none
  | PatchNameChanged v -> 
      let patchName = match v with
                      | v when v.Length > 10 -> v.Substring(0, 10)
                      | _ -> v
      (model |> Optic.set (patch >-> Patch.patchName) patchName), Cmd.ofMsg SliderComplete
  | Operator1Msg OperatorSliderComplete -> model, Cmd.ofMsg SliderComplete
  | Operator1Msg msg -> (updateOperator msg model Patch.operator1), Cmd.none
  | Operator2Msg OperatorSliderComplete -> model, Cmd.ofMsg SliderComplete
  | Operator2Msg msg -> (updateOperator msg model Patch.operator2), Cmd.none
  | Operator3Msg OperatorSliderComplete -> model, Cmd.ofMsg SliderComplete
  | Operator3Msg msg -> (updateOperator msg model Patch.operator3), Cmd.none
  | Operator4Msg OperatorSliderComplete -> model, Cmd.ofMsg SliderComplete
  | Operator4Msg msg -> (updateOperator msg model Patch.operator4), Cmd.none
  | Operator5Msg OperatorSliderComplete -> model, Cmd.ofMsg SliderComplete
  | Operator5Msg msg -> (updateOperator msg model Patch.operator5), Cmd.none
  | Operator6Msg OperatorSliderComplete -> model, Cmd.ofMsg SliderComplete
  | Operator6Msg msg -> (updateOperator msg model Patch.operator6), Cmd.none
  | SliderComplete ->
    model, Cmd.batch [ Cmd.ofMsg SendSysex
                       Cmd.ofMsg PermalinkChanged ]
  | SendSuccess -> { model with ErrorMessage = None }, Cmd.none
  | SendError e -> { model with ErrorMessage = Some e }, error e
  | MidiSuccess midiAccess ->
    let stateChangeSub dispatch =
      let onStateChange _ =
        dispatch (MidiStateChange midiAccess)
      midiAccess.OnStateChange <- onStateChange

    model, Cmd.batch [ Cmd.ofSub stateChangeSub
                       success "MIDI connected"
                       Cmd.ofMsg (MidiStateChange midiAccess)
                       Cmd.ofMsg PermalinkChanged ]
  | MidiError _ ->
    { model with MidiEnabled = false
                 MidiErrorMessage = Some "WebMidi is currently only supported in Chrome!" }, (error "WebMidi is currently only supported in Chrome!")
  | MidiStateChange midiAccess ->
    let getSelectedId map selected =
      match map |> Map.toList with
      | [] -> None
      | (id, _)::rest -> 
        match selected with
        | (Some oId) when (oId = id) || (rest |> List.exists (fun (key, _) -> oId = key)) -> Some oId
        | _ -> Some id
    
    let getNotificationMessage newList originalList selected originalSelected typeName =
      match newList with
      | [] -> 
        true, (warning (sprintf "No %ss found" typeName) |> Some)
      | os when os |> List.compareWith (fun (e1, _) (e2, _) -> (int e1) - (int e2)) originalList = 0 -> 
        false, None
      | os when os.Length > originalList.Length -> 
        false, (info (sprintf "New MIDI %s found" typeName) |> Some)
      | os when os.Length < originalList.Length -> 
        false, (info (sprintf "MIDI %s removed" typeName) |> Some)
      | _ -> if selected <> originalSelected
             then false, (warning (sprintf "MIDI %s changed!" typeName) |> Some)
             else false, None

    let outputs = midiAccess.Outputs 
                  |> Map.toList
                  |> List.filter (fun (_, o) -> o.Name |> Option.filter (fun v -> v <> "") |> Option.isSome)
    let selectedOutput = getSelectedId midiAccess.Outputs model.SelectedMIDIOutput 
    let outputsError, outputsNotification = 
      getNotificationMessage outputs 
                             model.MidiOutputs
                             selectedOutput
                             model.SelectedMIDIOutput
                             "output"

    let inputs = midiAccess.Inputs 
                 |> Map.toList 
                 |> List.filter (fun (_, i) -> i.Name |> Option.filter (fun v -> v <> "") |> Option.isSome)
    let selectedInput = getSelectedId midiAccess.Inputs model.SelectedMIDIInput
    let inputsError, inputsNotification =
      getNotificationMessage inputs
                             model.MidiInputs
                             selectedInput
                             model.SelectedMIDIInput
                             "input"

    let errorMessage =
      if inputsError && outputsError 
      then Some "No inputs or outputs found!"
      else None
    
    let newModel =
      { model with MidiErrorMessage = errorMessage
                   MidiEnabled = errorMessage |> Option.isNone
                   MidiOutputs = outputs
                   MidiInputs = inputs }
    let cmd = 
      match inputsNotification, outputsNotification with
      | Some inError, Some outError -> Cmd.batch [ outError; inError ]
      | Some inError, _ -> inError
      | _, Some outError -> outError
      | _-> Cmd.none

    newModel, Cmd.batch [ yield cmd
                          match selectedOutput with
                          | Some o -> yield (MidiOutputChanged o) |> Cmd.ofMsg
                          | None -> ()
                          match selectedInput with
                          | Some i -> yield (MidiInputChanged i) |> Cmd.ofMsg
                          | None -> ()
                        ]

  | MidiMessage m -> { model with MidiMessages = (m :: model.MidiMessages) |> List.truncate 5 }, Cmd.none
  | MidiOutputChanged id -> { model with SelectedMIDIOutput = Some id }, Cmd.none
  | MidiInputChanged id -> 
    let onMidiMessageSub (dispatch: Dispatch<Msg>) = 
      let onMidiMessage (ev: IMIDIMessageEvent) =
        match ev.Data |> validateSysexData with
        | None ->
          match ev.Data |> loadPatch with
          | Some patch -> dispatch (PatchLoaded patch)
          | None -> ()
        | Some _ -> ()

      match id, model.SelectedMIDIInput with
      | ("", Some oldId) ->
        match model.MidiInputs |> List.tryFind (fun (i, _)  -> i = oldId) |> Option.map snd with
        | Some oldInput -> oldInput.OnMidiMessage <- DoNothing
        | None -> ()
      | (id, None) when id <> "" ->
        match model.MidiInputs |> List.tryFind (fun (i, _)  -> i = id) |> Option.map snd with
        | Some i -> 
          i.OnMidiMessage <- onMidiMessage
        | None -> ()
      | (id, Some oldId) when oldId <> id ->
        match model.MidiInputs |> List.tryFind (fun (i, _)  -> i = oldId) |> Option.map snd with
        | Some oldInput -> oldInput.OnMidiMessage <- DoNothing
        | None -> ()

        match model.MidiInputs |> List.tryFind (fun (i, _)  -> i = id) |> Option.map snd with
        | Some i -> 
          Browser.console.log i
          i.OnMidiMessage <- onMidiMessage
        | None -> ()
      | _ -> ()

    { model with SelectedMIDIInput = Some id }, Cmd.ofSub onMidiMessageSub
  | MidiChannelChanged c -> { model with SelectedMIDIChannel = c }, Cmd.none
  | InitPatch -> model, Cmd.ofMsg (PatchLoaded (initPatch ()))
  | SavePatch -> model, Cmd.ofFunc savePatch 
                                   model.Patch 
                                   (fun _ -> MidiMessage (S.Success "saved.")) 
                                   (fun ex -> SendError ex.Message)
  | SendSysex ->
    match model.SelectedMIDIOutput with
    | None -> model, error "No Output selected!"
    | Some oId ->
      match model.MidiOutputs |> List.filter (fun (id, _) -> oId = id) with
      | [] -> { model with SelectedMIDIOutput = None }, error "Output not available"
      | (_, output)::_ ->
        let data = sysexData model
        match validateSysexData data with
        | Some msg -> model, error msg
        | _ -> model, Cmd.ofPromise (sysexData >> MIDI.send output) model (fun _ -> SendSuccess) (fun ex -> SendError ex.Message)
  | PermalinkChanged ->
    let permalink = model.Patch |> Patch.encode
    Browser.window.location.hash <- ("?patch=" + permalink)
    { model with PermaLink = permalink }, Cmd.none    
  | FileToLoadChanged fileList ->
    if fileList.Length = 0
    then model, Cmd.none
    else { model with FileToLoad = Some fileList.[0] }, Cmd.ofMsg LoadPatch
  | LoadPatch ->
    match model.FileToLoad with
    | Some blob ->
      model, Cmd.ofPromise checkSysexFileLoad blob id (fun ex -> SendError ex.Message)
    | _ -> model, Cmd.none
  | PatchLoaded patch -> 
    { model with Patch = patch } |> updateOperatorTypes, Cmd.batch [ Cmd.ofMsg SendSysex
                                                                     Cmd.ofMsg PermalinkChanged
                                                                     success (sprintf "Loaded patch %s" patch.PatchName) ]

/// Constructs the view for the application given the model.
let viewOperator (model: Operator) operatorType title (dispatch: OperatorMsg -> unit) : React.ReactElement =
  let operatorSliderComplete () = dispatch OperatorSliderComplete
  let mkKnob99 = S.knob 0 99 string dispatch operatorSliderComplete
  let mkKnob3 = S.knob 0 3 string dispatch operatorSliderComplete
  let mkKnobCurve =
    let format = function
                 | 0 -> "-LIN"
                 | 1 -> "-EXP"
                 | 2 -> "EXP"
                 | 3 -> "LIN"
                 | _ -> "?"

    S.knob 0 3 format dispatch operatorSliderComplete

  let mkKnob7 = S.knob 0 7 string dispatch operatorSliderComplete

  let bodyClassName = if model.Enabled then " show" else ""
  let cardClass = if operatorType = Carrier then " text-white bg-success" else " text-white bg-info"

  R.div [ P.ClassName ("card mt-2") ] [
    R.div [ P.ClassName ("card-header" + cardClass) ] [
      R.label [ P.ClassName "custom-control custom-checkbox" ] [
        R.input [ P.Type "checkbox"
                  P.ClassName "custom-control-input"
                  P.Checked model.Enabled
                  P.OnChange (fun _ -> dispatch (EnabledChanged (not model.Enabled))) ]
        R.span [ P.ClassName "custom-control-indicator" ] []
        R.strong [ P.ClassName "custom-control-description" ] [ R.str (sprintf "%s (%A)" title operatorType) ]
      ]
    ]

    R.div [ P.ClassName ("card-body collapse" + bodyClassName) ] [
      S.row [
        S.col [
          S.card  "Envelope rates" [
            S.row [
              S.col [ mkKnob99 "EG Rate 1" model.EGRate1 EGRate1Changed ]
              S.col [ mkKnob99 "EG Rate 2" model.EGRate2 EGRate2Changed ]
            ]
            S.row [
              S.col [ mkKnob99 "EG Rate 3" model.EGRate3 EGRate3Changed ]
              S.col [ mkKnob99 "EG Rate 4" model.EGRate4 EGRate4Changed ]
            ]
          ]
        ]
        S.col [
          S.card "Operator tuning" [
            S.row [
              S.col [ S.knob 0 14 string dispatch operatorSliderComplete "Detune" model.Detune DetuneChanged ]
              S.col [ S.knob 0 31 string dispatch operatorSliderComplete "Frequency Coarse" model.FrequencyCoarse FrequencyCoarseChanged ]
            ]
            S.row [
              S.col [ S.toggle "Oscillator mode" "Ratio" 0uy "Fixed" 1uy model.OscillatorMode (OscillatorModeChanged >> dispatch) ]
              S.col [ mkKnob99 "Frequency Fine" model.FrequencyFine FrequencyFineChanged ]
            ]
          ]
        ]
      ]
      S.row [
        S.col [
          S.card "Envelope Levels" [
            S.row [
              S.col [ mkKnob99 "EG Level 1" model.EGLevel1 EGLevel1Changed ]
              S.col [ mkKnob99 "EG Level 2" model.EGLevel2 EGLevel2Changed ]
            ]
            S.row [
              S.col [ mkKnob99 "EG Level 3" model.EGLevel3 EGLevel3Changed ]
              S.col [ mkKnob99 "EG Level 4" model.EGLevel4 EGLevel4Changed ]
            ]
          ]
        ]
        S.col [
          S.card "Operator Levels and Sensitivity" [
            S.row [
              S.col [ mkKnob3 "Amp Mod Sense" model.AmpModSense AmpModSenseChanged ]
              S.col [ mkKnob7 "Key Velocity Sense" model.KeyVelocitySense KeyVelocitySenseChanged ]
            ]
            S.row [
              S.col [ mkKnob99 "Operator Output Level" model.OperatorOutputLevel OperatorOutputLevelChanged ]
            ]
          ]
        ]
      ]
      S.rowcol [
        S.card "Operator scaling" [
          S.row [
            S.col [ mkKnob99 "Level Scale Breakpoint" model.LevelScaleBreakpoint LevelScaleBreakpointChanged ]
            S.col [ mkKnob99 "Level Scale Left Depth" model.LevelScaleLeftDepth LevelScaleLeftDepthChanged ]
            S.col [ mkKnob99 "Level Scale Right Depth" model.LevelScaleRightDepth LevelScaleRightDepthChanged ]              
            S.col [ mkKnobCurve "Level Scale Left Curve" model.LevelScaleLeftCurve LevelScaleLeftCurveChanged ]
            S.col [ mkKnobCurve "Level Scale Right Curve" model.LevelScaleRightCurve LevelScaleRightCurveChanged ]
            S.col [ mkKnob7 "Oscillator Rate Scale" model.OscillatorRateScale OscillatorRateScaleChanged ]
          ]
        ]              
      ]
    ]
  ]

let view model dispatch =
  let sliderComplete () = dispatch SliderComplete
  let mkKnob99 = S.knob 0 99 string dispatch sliderComplete
  let formatWave =
    function
    | 0 -> "TRI"
    | 1 -> "SWU"
    | 2 -> "SWD"
    | 3 -> "SQU"
    | 4 -> "SIN"
    | 5 -> "HLD"
    | _ -> "?"

  let formatAlgorithm = ((+) 1) >> string

  let operator title opType opModel opMsg =
    viewOperator opModel opType title (opMsg >> dispatch)

  R.div [ P.ClassName "container-fluid"] [
    yield S.githubBanner
    yield R.h1 [ ] [
        R.str "Volca FM Patch editor "
        R.span [ P.ClassName "badge badge-pill badge-secondary" ] [ R.str (sprintf "V%s" ReleaseNotes.Version) ]
    ]

    yield S.rowcol [
      S.card "Setup" [
        S.row [
          S.col [
            S.card "Midi device setup" [
              match model.MidiEnabled with
              | false ->
                match model.MidiErrorMessage with
                | Some m -> yield R.str m
                | _ -> ()
              | true ->
                match model.MidiOutputs with
                | [] -> ()
                | _ ->
                  yield S.select "Send to MIDI device" [ P.Value (model.SelectedMIDIOutput |> Option.defaultValue "")
                                                         P.OnChange (fun (ev:React.FormEvent) -> dispatch (MidiOutputChanged (!! ev.target?value))) ] [
                      for k, o in model.MidiOutputs do
                        yield R.option [ P.Value k ] [ R.str (o.Name |> Option.defaultValue "?") ]
                  ]

                match model.MidiInputs with
                | [] -> ()
                | _ ->
                  yield S.select "Receive from MIDI device" [ P.Value (model.SelectedMIDIInput |> Option.defaultValue "")
                                                              P.OnChange (fun (ev:React.FormEvent) -> dispatch (MidiInputChanged (!! ev.target?value))) ] [
                      for k, o in model.MidiInputs do
                        yield R.option [ P.Value k ] [ R.str (o.Name |> Option.defaultValue "?") ]
                  ]

                yield S.select "MIDI channel" [ P.Value (string model.SelectedMIDIChannel)
                                                P.OnChange (fun (ev:React.FormEvent) -> dispatch (MidiChannelChanged (byte !! ev.target?value))) ] [
                    for i in 1..16 do
                      yield R.option [ i |> string |> P.Value ] [ i |> string |> R.str ]
                ]
            ]
          ]

          S.col [
            S.card "Save / Load / Share" [
              R.div [ P.ClassName "form-group" ] [
                R.div [ P.ClassName "btn-group" ] [
                  R.button [ P.ClassName "btn btn-primary"
                             P.Type "button"
                             P.OnClick (fun _ -> dispatch InitPatch)] [ R.str "Init Patch" ]
                  R.button [ P.ClassName "btn btn-primary"
                             P.Type "button"
                             P.OnClick (fun _ -> dispatch SavePatch)] [ R.str "Save Patch" ]
                ]
              ]

              R.div [ P.ClassName "form-group" ] [
                R.label [ P.ClassName " col-form-label" ] [ R.strong [] [ R.str "Load patch" ] ]
                R.input [ P.Type "file"
                          P.ClassName "form-control-file"
                          P.Value ""
                          P.OnChange (fun (ev:React.FormEvent) -> dispatch (FileToLoadChanged (!! ev.target?files)))]
              ]

              R.a [ P.Href (sprintf "#?patch=%s" model.PermaLink) ] [ R.str "Permalink"]
            ]
          ]

          S.col [
            S.card "Messages" [
              for msg in model.MidiMessages do
                yield S.alert msg
            ]
          ]
        ]
      ]
    ]

    if model.MidiEnabled then
      let operatorSettings =
        S.card "Operator settings" [
          S.row [
            S.col [ S.knob 0 7 string dispatch sliderComplete "Feedback" model.Patch.Feedback FeedbackChanged ]
            S.col [ S.knob 0 48 string dispatch sliderComplete "Transpose" model.Patch.Transpose TransposeChanged ]
          ]
          S.row [
            S.col [ S.toggle "Oscillator Key Sync" "Off" 0uy "On" 1uy model.Patch.OscillatorKeySync (OscillatorKeySyncChanged >> dispatch) ]
            S.col [ S.knob 0 31 formatAlgorithm dispatch sliderComplete "Algorithm" model.Patch.Algorithm AlgorithmChanged ]
          ]
        ]
      let pitchEnvelopeRates =
        S.card "Pitch envelope rates" [
          S.row [
            S.col [ mkKnob99 "Pitch EG R1" model.Patch.PitchRate1 PitchRate1Changed ]
            S.col [ mkKnob99 "Pitch EG R2" model.Patch.PitchRate2 PitchRate2Changed ]
            S.col [ mkKnob99 "Pitch EG R3" model.Patch.PitchRate3 PitchRate3Changed ]
            S.col [ mkKnob99 "Pitch EG R4" model.Patch.PitchRate4 PitchRate4Changed ]
          ]
        ]
      let patchSettings =
        S.card "Patch settings" [
          R.div [ P.ClassName "form-group" ] [
            R.label [ P.ClassName "col-form-label col-form-label-sm" ] [ R.str "Patch name" ]
            R.input [ P.ClassName "form-control form-control-sm"
                      P.Type "text"
                      P.Value model.Patch.PatchName
                      P.OnChange (fun (ev:React.FormEvent) -> dispatch (PatchNameChanged !! ev.target?value)) ]
          ]
        ]
      let pitchEnvelopeLevels =
        S.card "Pitch envelope levels" [
          S.row [
            S.col [ mkKnob99 "Pitch EG L1" model.Patch.PitchLevel1 PitchLevel1Changed ]
            S.col [ mkKnob99 "Pitch EG L2" model.Patch.PitchLevel2 PitchLevel2Changed ]
            S.col [ mkKnob99 "Pitch EG L3" model.Patch.PitchLevel3 PitchLevel3Changed ]
            S.col [ mkKnob99 "Pitch EG L4" model.Patch.PitchLevel4 PitchLevel4Changed ]
          ]
        ]
      let lfoSettings = 
        S.card "LFO settings" [
          S.row [
            S.col [ mkKnob99 "LFO Speed" model.Patch.LFOSpeed LFOSpeedChanged ]
            S.col [ S.knob 0 5 formatWave dispatch sliderComplete "LFO Wave Shape" model.Patch.LFOWaveShape LFOWaveShapeChanged ]
            S.col [ mkKnob99 "LFO Pitch Mod Depth" model.Patch.LFOPitchModDepth LFOPitchModDepthChanged ]
          ]
          S.row [
            S.col [ mkKnob99 "LFO Amp Mod Depth" model.Patch.LFOAmpModDepth LFOAmpModDepthChanged ]
            S.col [ mkKnob99 "LFO Delay" model.Patch.LFODelay LFODelayChanged ]
            S.col [ S.knob 0 7 string dispatch sliderComplete "Pitch Mod Sensitivity" model.Patch.PitchModSensitivity PitchModSensitivityChanged ]
          ]
          S.rowcol [ S.toggle "LFO Key Sync" "Off" 0uy "On" 0uy model.Patch.LFOKeySync (LFOKeySyncChanged >> dispatch) ]
        ]
        
      yield S.row [
        S.col [
          S.card "Global voice controls" [
            S.row [
              S.col [
                S.container [
                  S.rowcol [ patchSettings ]
                  S.rowcol [ operatorSettings]
                ]
              ]
              S.col [ lfoSettings ]
              S.col [
                S.container [
                  S.rowcol [ pitchEnvelopeRates]
                  S.rowcol [ pitchEnvelopeLevels ]
                ]
              ]
            ]
          ]
        ]
      ]

      yield S.row [ 
        S.col [ operator "Operator 1" model.Operator1Type model.Patch.Operator1 Operator1Msg ] 
        S.col [ operator "Operator 2" model.Operator2Type model.Patch.Operator2 Operator2Msg ] 
      ]
      yield S.row [ 
        S.col [ operator "Operator 3" model.Operator3Type model.Patch.Operator3 Operator3Msg ] 
        S.col [ operator "Operator 4" model.Operator4Type model.Patch.Operator4 Operator4Msg ] 
      ]
      yield S.row [ 
        S.col [ operator "Operator 5" model.Operator5Type model.Patch.Operator5 Operator5Msg ] 
        S.col [ operator "Operator 6" model.Operator6Type model.Patch.Operator6 Operator6Msg ] 
      ]
  ]
