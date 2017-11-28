module Client.App

open Fable.Core
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props

open Fable.Import
open Elmish
open Elmish.React
open Fable.Import.Browser
open Elmish.Browser.Navigation
open Elmish.HMR

JsInterop.importSideEffects "whatwg-fetch"
JsInterop.importSideEffects "babel-polyfill"

module P = Fable.Helpers.React.Props
module S = Client.Style

type OperatorType = 
  | Carrier
  | Modulator

type OscillatorMode =
  | Ratio
  | Fixed

// Model
type OperatorModel =
  { Title : string
    Type : OperatorType
    Enabled : bool
    EGRate1 : int
    EGRate2 : int
    EGRate3 : int
    EGRate4 : int
    EGLevel1 : int
    EGLevel2 : int
    EGLevel3 : int
    EGLevel4 : int
    LevelScaleBreakpoint : int
    LevelScaleLeftDepth : int
    LevelScaleRightDepth : int
    LevelScaleLeftCurve : int
    LevelScaleRightCurve : int
    OscillatorRateScale : int
    Detune : int
    FrequencyCoarse : int
    FrequencyFine : int 
    OscillatorMode : OscillatorMode
    AmpModSense : int
    KeyVelocitySense : int
    OperatorOutputLevel : int }

type Model = { Algorithm: int
               Feedback: int
               OscillatorKeySync: int
               Transpose: int
               PitchRate1: int
               PitchRate2: int
               PitchRate3: int
               PitchRate4: int
               PitchLevel1: int
               PitchLevel2: int
               PitchLevel3: int
               PitchLevel4: int
               LFOSpeed: int
               LFOWaveShape: int
               LFOPitchModDepth: int
               LFOAmpModDepth: int
               LFODelay: int
               LFOKeySync: int
               PitchModSensitivity: int
               PatchName: string
               Operator1: OperatorModel
               Operator2: OperatorModel
               Operator3: OperatorModel
               Operator4: OperatorModel
               Operator5: OperatorModel
               Operator6: OperatorModel }

type OperatorMsg = 
  | TypeChanged of OperatorType
  | EnabledChanged of bool
  | EGRate1Changed of int
  | EGRate2Changed of int
  | EGRate3Changed of int
  | EGRate4Changed of int
  | EGLevel1Changed of int
  | EGLevel2Changed of int
  | EGLevel3Changed of int
  | EGLevel4Changed of int
  | LevelScaleBreakpointChanged of int
  | LevelScaleLeftDepthChanged of int
  | LevelScaleRightDepthChanged of int
  | LevelScaleLeftCurveChanged of int
  | LevelScaleRightCurveChanged of int
  | OscillatorRateScaleChanged of int
  | DetuneChanged of int
  | FrequencyCoarseChanged of int
  | FrequencyFineChanged of int
  | OscillatorModeChanged of string
  | AmpModSenseChanged of int
  | KeyVelocitySenseChanged of int
  | OperatorOutputLevelChanged of int

type Msg =
  | AlgorithmChanged of int
  | FeedbackChanged of int
  | OscillatorKeySyncChanged of int
  | TransposeChanged of int
  | PitchRate1Changed of int
  | PitchRate2Changed of int
  | PitchRate3Changed of int
  | PitchRate4Changed of int
  | PitchLevel1Changed of int
  | PitchLevel2Changed of int
  | PitchLevel3Changed of int
  | PitchLevel4Changed of int
  | LFOSpeedChanged of int
  | LFOWaveShapeChanged of int
  | LFOPitchModDepthChanged of int
  | LFOAmpModDepthChanged of int
  | LFODelayChanged of int
  | LFOKeySyncChanged of int
  | PitchModSensitivityChanged of int
  | PatchNameChanged of string
  | Operator1Msg of OperatorMsg
  | Operator2Msg of OperatorMsg
  | Operator3Msg of OperatorMsg
  | Operator4Msg of OperatorMsg
  | Operator5Msg of OperatorMsg
  | Operator6Msg of OperatorMsg

let initOperator title opType : OperatorModel =
  { Title = title
    Type = opType
    Enabled = false
    EGRate1 = 0
    EGRate2 = 0
    EGRate3 = 0
    EGRate4 = 0
    EGLevel1 = 0
    EGLevel2 = 0
    EGLevel3 = 0
    EGLevel4 = 0
    LevelScaleBreakpoint = 0
    LevelScaleLeftDepth = 0
    LevelScaleRightDepth = 0
    LevelScaleLeftCurve = 0
    LevelScaleRightCurve = 0
    OscillatorRateScale = 0
    Detune = 0
    FrequencyCoarse = 0
    FrequencyFine = 0
    OscillatorMode = Ratio
    AmpModSense = 0
    KeyVelocitySense = 0
    OperatorOutputLevel = 0 }

let init () : Model*Cmd<Msg> =
  { Algorithm = 1
    Feedback = 0
    OscillatorKeySync = 0
    Transpose = 0
    PitchRate1 = 0
    PitchRate2 = 0
    PitchRate3 = 0
    PitchRate4 = 0
    PitchLevel1 = 0
    PitchLevel2 = 0
    PitchLevel3 = 0
    PitchLevel4 = 0
    LFOSpeed = 0
    LFOWaveShape = 0
    LFOPitchModDepth = 0
    LFOAmpModDepth = 0
    LFODelay = 0
    LFOKeySync = 0
    PitchModSensitivity = 0
    PatchName = ""
    Operator1 = initOperator "Operator 1" Carrier
    Operator2 = initOperator "Operator 2" Modulator
    Operator3 = initOperator "Operator 3" Carrier
    Operator4 = initOperator "Operator 4" Modulator
    Operator5 = initOperator "Operator 5" Modulator
    Operator6 = initOperator "Operator 6" Modulator }, Cmd.none

let updateOperator (msg: OperatorMsg) (model: OperatorModel) : OperatorModel*Cmd<OperatorMsg> =
    match msg with
    | TypeChanged v -> { model with Type = v }, Cmd.none
    | EnabledChanged v -> { model with Enabled = v }, Cmd.none
    | EGRate1Changed v -> { model with EGRate1 = v }, Cmd.none
    | EGRate2Changed v -> { model with EGRate2 = v }, Cmd.none
    | EGRate3Changed v -> { model with EGRate3 = v }, Cmd.none
    | EGRate4Changed v -> { model with EGRate4 = v }, Cmd.none
    | EGLevel1Changed v -> { model with EGLevel1 = v }, Cmd.none
    | EGLevel2Changed v -> { model with EGLevel2 = v }, Cmd.none
    | EGLevel3Changed v -> { model with EGLevel3 = v }, Cmd.none
    | EGLevel4Changed v -> { model with EGLevel4 = v }, Cmd.none
    | LevelScaleBreakpointChanged v -> { model with LevelScaleBreakpoint = v }, Cmd.none
    | LevelScaleLeftDepthChanged v -> { model with LevelScaleLeftDepth = v }, Cmd.none
    | LevelScaleRightDepthChanged v -> { model with LevelScaleRightDepth = v }, Cmd.none
    | LevelScaleLeftCurveChanged v -> { model with LevelScaleLeftCurve = v }, Cmd.none
    | LevelScaleRightCurveChanged v -> { model with LevelScaleRightCurve = v }, Cmd.none
    | OscillatorRateScaleChanged v -> { model with OscillatorRateScale = v }, Cmd.none
    | DetuneChanged v -> { model with Detune = v }, Cmd.none
    | FrequencyCoarseChanged v -> { model with FrequencyCoarse = v }, Cmd.none
    | FrequencyFineChanged v -> { model with FrequencyFine = v }, Cmd.none
    | OscillatorModeChanged v -> { model with OscillatorMode = if v = "0" then Ratio else Fixed }, Cmd.none
    | AmpModSenseChanged v -> { model with AmpModSense = v }, Cmd.none
    | KeyVelocitySenseChanged v -> { model with KeyVelocitySense = v }, Cmd.none
    | OperatorOutputLevelChanged v -> { model with OperatorOutputLevel = v }, Cmd.none
  
let algorithms = [
  Carrier, Modulator, Carrier, Modulator, Modulator, Modulator
  Carrier, Modulator, Carrier, Modulator, Modulator, Modulator
  Carrier, Modulator, Modulator, Carrier, Modulator, Modulator
  Carrier, Modulator, Modulator, Carrier, Modulator, Modulator
  Carrier, Modulator, Carrier, Modulator, Carrier, Modulator
  Carrier, Modulator, Carrier, Modulator, Carrier, Modulator
  Carrier, Modulator, Carrier, Modulator, Modulator, Modulator
  Carrier, Modulator, Carrier, Modulator, Modulator, Modulator
  Carrier, Modulator, Carrier, Modulator, Modulator, Modulator
  Carrier, Modulator, Modulator, Carrier, Modulator, Modulator

  Carrier, Modulator, Modulator, Carrier, Modulator, Modulator
  Carrier, Modulator, Carrier, Modulator, Modulator, Modulator
  Carrier, Modulator, Carrier, Modulator, Modulator, Modulator
  Carrier, Modulator, Carrier, Modulator, Modulator, Modulator
  Carrier, Modulator, Carrier, Modulator, Modulator, Modulator
  Carrier, Modulator, Modulator, Modulator, Modulator, Modulator
  Carrier, Modulator, Modulator, Modulator, Modulator, Modulator
  Carrier, Modulator, Modulator, Modulator, Modulator, Modulator
  
  Carrier, Modulator, Modulator, Carrier, Carrier, Modulator
  Carrier, Carrier, Modulator, Carrier, Modulator, Modulator
  Carrier, Carrier, Modulator, Carrier, Carrier, Modulator
  Carrier, Modulator, Carrier, Carrier, Carrier, Modulator
  Carrier, Carrier, Modulator, Carrier, Carrier, Modulator
  Carrier, Carrier, Carrier, Carrier, Carrier, Modulator
  Carrier, Carrier, Carrier, Carrier, Carrier, Modulator
  
  Carrier, Carrier, Modulator, Carrier, Modulator, Modulator
  Carrier, Carrier, Modulator, Carrier, Modulator, Modulator
  Carrier, Modulator, Carrier, Modulator, Modulator, Carrier
  Carrier, Carrier, Carrier, Modulator, Carrier, Modulator
  Carrier, Carrier, Carrier, Modulator, Modulator, Carrier
  Carrier, Carrier, Carrier, Carrier, Carrier, Modulator
  Carrier, Carrier, Carrier, Carrier, Carrier, Carrier
]

let updateAlgorithm model algo =
  let op1, op2, op3, op4, op5, op6 = algorithms.[algo - 1]
  { model with Algorithm = algo
               Operator1 = { model.Operator1 with Type = op1 }
               Operator2 = { model.Operator2 with Type = op2 }
               Operator3 = { model.Operator3 with Type = op3 }
               Operator4 = { model.Operator4 with Type = op4 }
               Operator5 = { model.Operator5 with Type = op5 }
               Operator6 = { model.Operator6 with Type = op6 } }

let update (msg: Msg) (model: Model) : Model*Cmd<Msg> =
  match msg with
  | AlgorithmChanged v -> (updateAlgorithm model v), Cmd.none
  | FeedbackChanged v -> { model with Feedback = v }, Cmd.none
  | OscillatorKeySyncChanged v -> { model with OscillatorKeySync = v }, Cmd.none
  | TransposeChanged v -> { model with Transpose = v }, Cmd.none
  | PitchRate1Changed v -> { model with PitchRate1 = v}, Cmd.none
  | PitchRate2Changed v -> { model with PitchRate2 = v}, Cmd.none
  | PitchRate3Changed v -> { model with PitchRate3 = v}, Cmd.none
  | PitchRate4Changed v -> { model with PitchRate4 = v}, Cmd.none
  | PitchLevel1Changed v -> { model with PitchLevel1 = v}, Cmd.none
  | PitchLevel2Changed v -> { model with PitchLevel2 = v}, Cmd.none
  | PitchLevel3Changed v -> { model with PitchLevel3 = v}, Cmd.none
  | PitchLevel4Changed v -> { model with PitchLevel4 = v}, Cmd.none
  | LFOSpeedChanged v -> { model with LFOSpeed = v }, Cmd.none
  | LFOWaveShapeChanged v -> { model with LFOWaveShape = v }, Cmd.none
  | LFOPitchModDepthChanged v -> { model with LFOPitchModDepth = v }, Cmd.none
  | LFOAmpModDepthChanged v -> { model with LFOAmpModDepth = v }, Cmd.none
  | LFODelayChanged v -> { model with LFODelay = v }, Cmd.none
  | LFOKeySyncChanged v -> { model with LFOKeySync = v }, Cmd.none
  | PitchModSensitivityChanged v -> { model with PitchModSensitivity = v }, Cmd.none
  | PatchNameChanged v -> { model with PatchName = v }, Cmd.none
  | Operator1Msg msg ->
    let m, c = updateOperator msg model.Operator1
    { model with Operator1 = m }, Cmd.map Operator1Msg c
  | Operator2Msg msg ->
    let m, c = updateOperator msg model.Operator2
    { model with Operator2 = m }, Cmd.map Operator2Msg c
  | Operator3Msg msg ->
    let m, c = updateOperator msg model.Operator3
    { model with Operator3 = m }, Cmd.map Operator3Msg c
  | Operator4Msg msg ->
    let m, c = updateOperator msg model.Operator4
    { model with Operator4 = m }, Cmd.map Operator4Msg c
  | Operator5Msg msg ->
    let m, c = updateOperator msg model.Operator5
    { model with Operator5 = m }, Cmd.map Operator5Msg c
  | Operator6Msg msg ->
    let m, c = updateOperator msg model.Operator6
    { model with Operator6 = m }, Cmd.map Operator6Msg c

open Client.Bindings.Slider
open Fable.Import.React

let mkSlider min max format dispatch description value event =
  div [ ClassName "form-group slider custom-labels"] [
    label [ ClassName "col-form-label" ] [ str description ]    
    slider [ Min min
             Max max
             Value value
             Format format
             HandleLabel (format value)
             OnChange (fun i -> dispatch (event i)) ] 
  ]

let card title content =
  div [ ClassName "col" ] [
    div [ ClassName "card" ] [
      div [ ClassName "card-header" ] [ strong [] [ str title ] ]
      div [ ClassName "card-body" ] content
    ]
  ]

/// Constructs the view for the application given the model.
let viewOperator (model: OperatorModel) (dispatch: OperatorMsg -> unit) : ReactElement =

  let mkSlider99 = mkSlider 0 99 string dispatch
  let mkSlider3 = mkSlider 0 3 string dispatch
  let mkSliderCurve =
    let format v = match v with
                   | v when v <= 0 -> "-LIN"
                   | 1 -> "-EXP"
                   | 2 -> "EXP"
                   | v when v >= 3 -> "LIN"
    mkSlider 0 4 format dispatch
    
  let mkSlider7 = mkSlider 0 7 string dispatch

  let bodyClassName = if model.Enabled then " show" else ""
  let cardClass = if model.Type = Carrier then " text-white bg-success" else " text-white bg-info"

  div [ ClassName ("card mt-2") ] [
    div [ ClassName ("card-header" + cardClass) ] [ 
      label [ ClassName "custom-control custom-checkbox" ] [
        input [ Type "checkbox" 
                ClassName "custom-control-input"
                Checked model.Enabled
                OnClick (fun _ -> dispatch (EnabledChanged (not model.Enabled))) ]
        span [ ClassName "custom-control-indicator" ] []
        strong [ ClassName "custom-control-description" ] [ str model.Title ]
      ]
    ]

    div [ ClassName ("card-body collapse" + bodyClassName) ] [

      div [ ClassName "row" ] [
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
          mkSlider 0 14 string dispatch "Detune" model.Detune DetuneChanged
          mkSlider 0 31 string dispatch "Frequency Coarse" model.FrequencyCoarse FrequencyCoarseChanged
          mkSlider99 "Frequency Fine" model.FrequencyFine FrequencyFineChanged

          div [ ClassName "form-group" ] [
            label [ ClassName "col-form-label" ] [ str "Oscillator mode" ]
            br []
            S.radioInline "Ratio" "0" (model.OscillatorMode = Ratio) (fun _ -> dispatch (OscillatorModeChanged "0"))
            S.radioInline "Fixed" "1" (model.OscillatorMode = Fixed) (fun _ -> dispatch (OscillatorModeChanged "1"))
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
  let mkSlider99 = mkSlider 0 99 string dispatch
  let formatWave =
    function
    | 0 -> "TRI"
    | 1 -> "SWU"
    | 2 -> "SWD"
    | 3 -> "SQU"
    | 4 -> "SIN"
    | 5 -> "HLD"
    | _ -> "?"

  let operator opModel opMsg =
    div [ ClassName "row" ] [ 
      div [ ClassName "col" ] [
        viewOperator opModel (opMsg >> dispatch)
      ]
    ]

  div [ ClassName "container-fluid"] [
    yield div [ ClassName "row mt-2" ] [
      card "Setup" [ 
        div [ ClassName "row" ] [ 
          card "Midi device setup" []
          card "Save / Load / Share" []
        ]
      ]
    ]

    yield div [ ClassName "row mt-2" ] [
      card "Global voice controls" [
        div [ ClassName "row" ] [ 
          card "Operator settings" [
            mkSlider 1 32 string dispatch "Algorithm" model.Algorithm AlgorithmChanged
            mkSlider 0 7 string dispatch "Feedback" model.Feedback FeedbackChanged
            div [ ClassName "form-group" ] [
              label [ ClassName "col-form-label" ] [ str "Oscillator Key Sync" ]
              br []
              S.radioInline "Off" "0" (model.OscillatorKeySync = 0) (fun _ -> dispatch (OscillatorKeySyncChanged 0))
              S.radioInline "On" "1" (model.OscillatorKeySync = 1) (fun _ -> dispatch (OscillatorKeySyncChanged 1))
            ]
            mkSlider 0 48 string dispatch "Transpose" model.Transpose TransposeChanged
          ]
          card "Pitch envelope rates" [
            mkSlider99 "Pitch EG R1" model.PitchRate1 PitchRate1Changed
            mkSlider99 "Pitch EG R2" model.PitchRate2 PitchRate2Changed
            mkSlider99 "Pitch EG R3" model.PitchRate3 PitchRate3Changed
            mkSlider99 "Pitch EG R4" model.PitchRate4 PitchRate4Changed
          ]
          card "Pitch envelope levels" [
            mkSlider99 "Pitch EG L1" model.PitchLevel1 PitchLevel1Changed
            mkSlider99 "Pitch EG L2" model.PitchLevel2 PitchLevel2Changed
            mkSlider99 "Pitch EG L3" model.PitchLevel3 PitchLevel3Changed
            mkSlider99 "Pitch EG L4" model.PitchLevel4 PitchLevel4Changed
          ]
          card "LFO settings" [
            mkSlider99 "LFO Speed" model.LFOSpeed LFOSpeedChanged
            mkSlider 0 5 formatWave dispatch "LFO Wave Shape" model.LFOWaveShape LFOWaveShapeChanged
            mkSlider99 "LFO Pitch Mod Depth" model.LFOPitchModDepth LFOPitchModDepthChanged
            mkSlider99 "LFO Amp Mod Depth" model.LFOAmpModDepth LFOAmpModDepthChanged
            mkSlider99 "LFO Delay" model.LFODelay LFODelayChanged
            div [ ClassName "form-group" ] [
              label [ ClassName "col-form-label" ] [ str "LFO Key Sync" ]
              br []
              S.radioInline "Off" "0" (model.LFOKeySync = 0) (fun _ -> dispatch (LFOKeySyncChanged 0))
              S.radioInline "On" "1" (model.LFOKeySync = 1) (fun _ -> dispatch (LFOKeySyncChanged 1))
            ]
            mkSlider 0 7 string dispatch "Pitch Mod Sensitivity" model.PitchModSensitivity PitchModSensitivityChanged
          ]
          card "Patch settings" [
            div [ ClassName "form-group" ] [
              label [ ClassName "col-form-label" ] [ str "Patch name" ]
              input [ ClassName "form-control"
                      Type "text"
                      P.Value model.PatchName
                      P.OnChange (fun (ev:React.FormEvent) -> dispatch (PatchNameChanged !! ev.target?value)) ]
            ]
          ]
        ]
      ]
    ]

    yield operator model.Operator1 Operator1Msg
    yield operator model.Operator2 Operator2Msg
    yield operator model.Operator3 Operator3Msg
    yield operator model.Operator4 Operator4Msg
    yield operator model.Operator5 Operator5Msg
    yield operator model.Operator6 Operator6Msg
  ]

open Elmish.React
open Elmish.Debug

// App
Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
|> Program.withHMR
#endif
|> Program.withReact "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
