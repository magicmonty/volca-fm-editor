module Client.App

open Aether
open Aether.Operators
open Fable.Core.JsInterop
open Fable.Import

open Elmish
open Elmish.React

open Bindings.Slider
open VolcaFM
open Midi

importSideEffects "whatwg-fetch"
importSideEffects "babel-polyfill"

module S = Client.Style
module R = Fable.Helpers.React
module P = Fable.Helpers.React.Props

module String = 
  let isNotEmpty v = not (System.String.IsNullOrEmpty v)

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
               MidiAccess: IMIDIAccess option
               MidiMessages : S.Alert list
               SelectedMIDIOutput : IMIDIOutput option
               SelectedMIDIChannel : byte
               FileToLoad : Browser.Blob option }

             static member patch = (fun m -> m.Patch), (fun value m -> { m with Patch = value })
  
let sysexData model = model.Patch |> toSysexMessage |> List.toArray

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
  | MidiOutputChanged of IMIDIOutput
  | MidiChannelChanged of byte
  | SendSysex
  | InitPatch
  | SavePatch
  | FileToLoadChanged of Browser.Blob list
  | LoadPatch
  | PatchLoaded of Patch


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
  
open Fable.PowerPack

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
                      | _ -> MidiMessage (S.Error, "Could not load patch!")
                    | Some msg -> MidiMessage (S.Error, msg)
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
  let m =
    { MidiEnabled = false
      Patch = initPatch ()
      Operator1Type = Carrier
      Operator2Type = Carrier
      Operator3Type = Carrier
      Operator4Type = Carrier
      Operator5Type = Carrier
      Operator6Type = Carrier
      ErrorMessage = None
      MidiErrorMessage = None
      MidiAccess = None
      MidiMessages = []
      SelectedMIDIOutput = None
      SelectedMIDIChannel = 1uy 
      FileToLoad = None }
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
  let success msg = Cmd.ofMsg (MidiMessage (S.Success, msg))
  let error msg = Cmd.ofMsg (MidiMessage (S.Error, msg))
  let warning  msg = Cmd.ofMsg (MidiMessage (S.Warning, msg))

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
  | PatchNameChanged v -> (model |> Optic.set (patch >-> Patch.patchName) v), Cmd.none
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
  | SliderComplete -> model, Cmd.ofMsg SendSysex
  | SendSuccess -> { model with ErrorMessage = None }, Cmd.none
  | SendError e -> { model with ErrorMessage = Some e }, error e
  | MidiSuccess midiAccess -> 
    midiAccess?onstatechange <- onStateChange
    model, Cmd.ofMsg (MidiStateChange midiAccess)
  | MidiStateChange midiAccess -> 
    let result = { model with MidiEnabled = true
                              MidiErrorMessage = None
                              MidiAccess = Some midiAccess }

    match midiAccess.Outputs.size with
    | 0. -> { result with MidiErrorMessage = Some "No outputs found" }, Cmd.batch [ (success "MIDI connected")
                                                                                    (warning "No outputs found") ]
    | _ -> { result with SelectedMIDIOutput = midiAccess.Outputs.values().next().value }, (success "MIDI connected")
    
  | MidiError _ ->
    { model with MidiEnabled = false
                 MidiErrorMessage = Some "WebMidi is currently only supported in Chrome!"
                 MidiAccess = None }, (error "WebMidi is currently only supported in Chrome!")
  | MidiMessage m -> { model with MidiMessages = (m :: model.MidiMessages) |> List.truncate 5 }, Cmd.none
  | MidiOutputChanged o -> { model with SelectedMIDIOutput = Some o }, Cmd.none
  | MidiChannelChanged c -> { model with SelectedMIDIChannel = c }, Cmd.none
  | InitPatch -> { model with Patch = initPatch () }, Cmd.ofMsg SendSysex
  | SavePatch -> model, Cmd.ofFunc savePatch model.Patch (fun _ -> MidiMessage (S.Success, "saved.")) (fun ex -> SendError ex.Message)
  | SendSysex -> 
    match model.SelectedMIDIOutput with
    | None -> model, error "No Output selected!"
    | Some o -> 
      let data = sysexData model
      match validateSysexData data with
      | Some msg -> model, error msg
      | _ -> model, Cmd.ofPromise (sysexData >> MIDI.send o) model (fun _ -> SendSuccess) (fun ex -> SendError ex.Message)
  | FileToLoadChanged fileList -> 
    if fileList.Length = 0 
    then model, Cmd.none
    else { model with FileToLoad = Some fileList.[0] }, Cmd.ofMsg LoadPatch
  | LoadPatch -> 
    match model.FileToLoad with
    | Some blob -> 
      model, Cmd.ofPromise checkSysexFileLoad blob id (fun ex -> SendError ex.Message)
    | _ -> model, Cmd.none
  | PatchLoaded patch -> { model with Patch = patch }, Cmd.batch [ Cmd.ofMsg SendSysex
                                                                   success (sprintf "Loaded patch %s" patch.PatchName) ]
      
let mkSlider min max format dispatch onComplete description (value: byte) event =
  R.div [ P.ClassName "form-group slider custom-labels"] [
    R.label [ P.ClassName "col-form-label" ] [ R.str description ]
    slider [ Min min
             Max max
             Value (int value)
             Format format
             HandleLabel (format (int value))
             OnChange (byte >> event >> dispatch)
             OnChangeComplete onComplete ] 
  ]

let card title content =
  R.div [ P.ClassName "col" ] [
    R.div [ P.ClassName "card" ] [
      R.div [ P.ClassName "card-header" ] [ R.strong [] [ R.str title ] ]
      R.div [ P.ClassName "card-body" ] content
    ]
  ]

/// Constructs the view for the application given the model.
let viewOperator (model: Operator) operatorType title (dispatch: OperatorMsg -> unit) : React.ReactElement =
  let operatorSliderComplete () = dispatch OperatorSliderComplete
  let mkSlider99 = mkSlider 0 99 string dispatch operatorSliderComplete
  let mkSlider3 = mkSlider 0 3 string dispatch operatorSliderComplete
  let mkSliderCurve =
    let format = function
                 | 0 -> "-LIN"
                 | 1 -> "-EXP"
                 | 2 -> "EXP"
                 | 3 -> "LIN"
                 | _ -> "?"

    mkSlider 0 3 format dispatch operatorSliderComplete
    
  let mkSlider7 = mkSlider 0 7 string dispatch operatorSliderComplete

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

      R.div [ P.ClassName "row" ] [
        card  "Envelope rates" [
          mkSlider99 "EG Rate 1" model.EGRate1 EGRate1Changed
          mkSlider99 "EG Rate 2" model.EGRate2 EGRate2Changed
          mkSlider99 "EG Rate 3" model.EGRate3 EGRate3Changed
          mkSlider99 "EG Rate 4" model.EGRate4 EGRate4Changed
        ]
        card "Envelope Levels" [
          mkSlider99 "EG Level 1" model.EGLevel1 EGLevel1Changed
          mkSlider99 "EG Level 2" model.EGLevel2 EGLevel2Changed
          mkSlider99 "EG Level 3" model.EGLevel3 EGLevel3Changed
          mkSlider99 "EG Level 4" model.EGLevel4 EGLevel4Changed
        ]
        card "Operator scaling" [
          mkSlider99 "Level Scale Breakpoint" model.LevelScaleBreakpoint LevelScaleBreakpointChanged
          mkSlider99 "Level Scale Left Depth" model.LevelScaleLeftDepth LevelScaleLeftDepthChanged
          mkSlider99 "Level Scale Right Depth" model.LevelScaleRightDepth LevelScaleRightDepthChanged
          mkSliderCurve "Level Scale Left Curve" model.LevelScaleLeftCurve LevelScaleLeftCurveChanged
          mkSliderCurve "Level Scale Right Curve" model.LevelScaleRightCurve LevelScaleRightCurveChanged
          mkSlider7 "Oscillator Rate Scale" model.OscillatorRateScale OscillatorRateScaleChanged
        ]
        card "Operator tuning" [
          mkSlider 0 14 string dispatch operatorSliderComplete "Detune" model.Detune DetuneChanged
          mkSlider 0 31 string dispatch operatorSliderComplete "Frequency Coarse" model.FrequencyCoarse FrequencyCoarseChanged
          mkSlider99 "Frequency Fine" model.FrequencyFine FrequencyFineChanged

          R.div [ P.ClassName "form-group" ] [
            R.label [ P.ClassName "col-form-label" ] [ R.str "Oscillator mode" ]
            R.br []
            S.radioInline "Ratio" "0" (model.OscillatorMode = 0uy) (fun _ -> dispatch (OscillatorModeChanged 0uy))
            S.radioInline "Fixed" "1" (model.OscillatorMode = 1uy) (fun _ -> dispatch (OscillatorModeChanged 1uy))
          ]
        ]
        card "Operator Levels and Sensitivity" [
          mkSlider3 "Amp Mod Sense" model.AmpModSense AmpModSenseChanged
          mkSlider7 "Key Velocity Sense" model.KeyVelocitySense KeyVelocitySenseChanged
          mkSlider99 "Operator Output Level" model.OperatorOutputLevel OperatorOutputLevelChanged
        ]
      ]
    ]
  ]

let view model dispatch =
  let sliderComplete () = dispatch SliderComplete
  let mkSlider99 = mkSlider 0 99 string dispatch sliderComplete
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
    R.div [ P.ClassName "row" ] [ 
      R.div [ P.ClassName "col" ] [
        viewOperator opModel opType title (opMsg >> dispatch)
      ]
    ]

  R.div [ P.ClassName "container-fluid"] [
    yield R.div [ P.ClassName "row mt-2" ] [
      card "Setup" [ 
        R.div [ P.ClassName "row" ] [ 
          card "Midi device setup" [
            match model.MidiErrorMessage with
            | Some m -> yield R.str m
            | _ -> ()

            match model.MidiAccess with
            | None -> () 
            | Some midiAccess ->

              yield S.select "MIDI input device" [ P.Value (model.SelectedMIDIOutput |> Option.map (fun o -> !! o?id) |> Option.defaultValue "")
                                                   P.OnChange (fun (ev:React.FormEvent) -> dispatch (MidiOutputChanged (midiAccess.Outputs.get (!! ev.target?value)))) ] [
                  for k, o in (midiAccess.Outputs |> Map.toList) do
                    yield R.option [ P.Value k ] [ R.str (o.Name |> Option.defaultValue "?") ]
              ]

              yield S.select "MIDI channel" [ P.Value (string model.SelectedMIDIChannel)
                                              P.OnChange (fun (ev:React.FormEvent) -> dispatch (MidiChannelChanged (byte !! ev.target?value))) ] [
                  for i in 1..16 do
                    yield R.option [ i |> string |> P.Key ] [ i |> string |> R.str ]
              ]
          ]

          card "Save / Load / Share" [
            R.div [ P.ClassName "btn-group" ] [
              R.button [ P.ClassName "btn btn-primary" 
                         P.Type "button" 
                         P.OnClick (fun _ -> dispatch InitPatch)] [ R.str "Init Patch" ]
              R.button [ P.ClassName "btn btn-primary" 
                         P.Type "button" 
                         P.OnClick (fun _ -> dispatch SavePatch)] [ R.str "Save Patch" ]              
            ]
                        
            R.input [ P.Type "file"
                      P.Value ""
                      P.OnChange (fun (ev:React.FormEvent) -> dispatch (FileToLoadChanged (!! ev.target?files)))]
          ]
          card "Midi messages" [
            for msg in model.MidiMessages do
              yield S.alert msg
          ]
        ]
      ]
    ]

    if model.MidiEnabled then
      yield R.div [ P.ClassName "row mt-2" ] [
        card "Global voice controls" [
          R.div [ P.ClassName "row" ] [ 
            card "Operator settings" [
              mkSlider 0 31 formatAlgorithm dispatch sliderComplete "Algorithm" model.Patch.Algorithm AlgorithmChanged
              mkSlider 0 7 string dispatch sliderComplete "Feedback" model.Patch.Feedback FeedbackChanged
              R.div [ P.ClassName "form-group" ] [
                R.label [ P.ClassName "col-form-label" ] [ R.str "Oscillator Key Sync" ]
                R.br []
                S.radioInline "Off" "0" (model.Patch.OscillatorKeySync = 0uy) (fun _ -> dispatch (OscillatorKeySyncChanged 0uy))
                S.radioInline "On" "1" (model.Patch.OscillatorKeySync = 1uy) (fun _ -> dispatch (OscillatorKeySyncChanged 1uy))
              ]
              mkSlider 0 48 string dispatch sliderComplete "Transpose" model.Patch.Transpose TransposeChanged
            ]
            card "Pitch envelope rates" [
              mkSlider99 "Pitch EG R1" model.Patch.PitchRate1 PitchRate1Changed
              mkSlider99 "Pitch EG R2" model.Patch.PitchRate2 PitchRate2Changed
              mkSlider99 "Pitch EG R3" model.Patch.PitchRate3 PitchRate3Changed
              mkSlider99 "Pitch EG R4" model.Patch.PitchRate4 PitchRate4Changed
            ]
            card "Pitch envelope levels" [
              mkSlider99 "Pitch EG L1" model.Patch.PitchLevel1 PitchLevel1Changed
              mkSlider99 "Pitch EG L2" model.Patch.PitchLevel2 PitchLevel2Changed
              mkSlider99 "Pitch EG L3" model.Patch.PitchLevel3 PitchLevel3Changed
              mkSlider99 "Pitch EG L4" model.Patch.PitchLevel4 PitchLevel4Changed
            ]
            card "LFO settings" [
              mkSlider99 "LFO Speed" model.Patch.LFOSpeed LFOSpeedChanged
              mkSlider 0 5 formatWave dispatch sliderComplete "LFO Wave Shape" model.Patch.LFOWaveShape LFOWaveShapeChanged
              mkSlider99 "LFO Pitch Mod Depth" model.Patch.LFOPitchModDepth LFOPitchModDepthChanged
              mkSlider99 "LFO Amp Mod Depth" model.Patch.LFOAmpModDepth LFOAmpModDepthChanged
              mkSlider99 "LFO Delay" model.Patch.LFODelay LFODelayChanged
              R.div [ P.ClassName "form-group" ] [
                R.label [ P.ClassName "col-form-label" ] [ R.str "LFO Key Sync" ]
                R.br []
                S.radioInline "Off" "0" (model.Patch.LFOKeySync = 0uy) (fun _ -> dispatch (LFOKeySyncChanged 0uy))
                S.radioInline "On" "1" (model.Patch.LFOKeySync = 1uy) (fun _ -> dispatch (LFOKeySyncChanged 1uy))
              ]
              mkSlider 0 7 string dispatch sliderComplete "Pitch Mod Sensitivity" model.Patch.PitchModSensitivity PitchModSensitivityChanged
            ]
            card "Patch settings" [
              R.div [ P.ClassName "form-group" ] [
                R.label [ P.ClassName "col-form-label" ] [ R.str "Patch name" ]
                R.input [ P.ClassName "form-control"
                          P.Type "text"
                          P.Value model.Patch.PatchName
                          P.OnChange (fun (ev:React.FormEvent) -> dispatch (PatchNameChanged !! ev.target?value)) ]
              ]
            ]
          ]
        ]
      ]

      yield operator "Operator 1" model.Operator1Type model.Patch.Operator1 Operator1Msg
      yield operator "Operator 2" model.Operator2Type model.Patch.Operator2 Operator2Msg
      yield operator "Operator 3" model.Operator3Type model.Patch.Operator3 Operator3Msg
      yield operator "Operator 4" model.Operator4Type model.Patch.Operator4 Operator4Msg
      yield operator "Operator 5" model.Operator5Type model.Patch.Operator5 Operator5Msg
      yield operator "Operator 6" model.Operator6Type model.Patch.Operator6 Operator6Msg
  ]


#if DEBUG
open Elmish.HMR
// open Elmish.Debug
#endif

// App
Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
|> Program.withHMR
#endif
|> Program.withReact "elmish-app"

#if DEBUG
// |> Program.withDebugger
#endif
|> Program.run
