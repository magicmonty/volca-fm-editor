module VolcaFM

open Aether

type Operator =
  { Enabled : bool
    EGRate1 : byte
    EGRate2 : byte
    EGRate3 : byte
    EGRate4 : byte
    EGLevel1 : byte
    EGLevel2 : byte
    EGLevel3 : byte
    EGLevel4 : byte
    LevelScaleBreakpoint : byte
    LevelScaleLeftDepth : byte
    LevelScaleRightDepth : byte
    LevelScaleLeftCurve : byte
    LevelScaleRightCurve : byte
    OscillatorRateScale : byte
    AmpModSense : byte
    KeyVelocitySense : byte
    OperatorOutputLevel : byte 
    OscillatorMode : byte 
    FrequencyCoarse : byte
    FrequencyFine : byte
    Detune : byte }

  static member enabled = (fun o -> o.Enabled), (fun value o -> { o with Enabled = value })
  static member eGRate1 = (fun o -> o.EGRate1), (fun value o -> { o with EGRate1 = value })
  static member eGRate2 = (fun o -> o.EGRate2), (fun value o -> { o with EGRate2 = value })
  static member eGRate3 = (fun o -> o.EGRate3), (fun value o -> { o with EGRate3 = value })
  static member eGRate4 = (fun o -> o.EGRate4), (fun value o -> { o with EGRate4 = value })
  static member eGLevel1 = (fun o -> o.EGLevel1), (fun value o -> { o with EGLevel1 = value })
  static member eGLevel2 = (fun o -> o.EGLevel2), (fun value o -> { o with EGLevel2 = value })
  static member eGLevel3 = (fun o -> o.EGLevel3), (fun value o -> { o with EGLevel3 = value })
  static member eGLevel4 = (fun o -> o.EGLevel4), (fun value o -> { o with EGLevel4 = value })
  static member levelScaleBreakpoint = (fun o -> o.LevelScaleBreakpoint), (fun value o -> { o with LevelScaleBreakpoint = value })
  static member levelScaleLeftDepth = (fun o -> o.LevelScaleLeftDepth), (fun value o -> { o with LevelScaleLeftDepth = value })
  static member levelScaleRightDepth = (fun o -> o.LevelScaleRightDepth), (fun value o -> { o with LevelScaleRightDepth = value })
  static member levelScaleLeftCurve = (fun o -> o.LevelScaleLeftCurve), (fun value o -> { o with LevelScaleLeftCurve = value })
  static member levelScaleRightCurve = (fun o -> o.LevelScaleRightCurve), (fun value o -> { o with LevelScaleRightCurve = value })
  static member oscillatorRateScale = (fun o -> o.OscillatorRateScale), (fun value o -> { o with OscillatorRateScale = value })
  static member ampModSense = (fun o -> o.AmpModSense), (fun value o -> { o with AmpModSense = value })
  static member keyVelocitySense = (fun o -> o.KeyVelocitySense), (fun value o -> { o with KeyVelocitySense = value })
  static member operatorOutputLevel = (fun o -> o.OperatorOutputLevel), (fun value o -> { o with OperatorOutputLevel = value })
  static member oscillatorMode = (fun o -> o.OscillatorMode), (fun value o -> { o with OscillatorMode = value })
  static member frequencyCoarse = (fun o -> o.FrequencyCoarse), (fun value o -> { o with FrequencyCoarse = value })
  static member frequencyFine = (fun o -> o.FrequencyFine), (fun value o -> { o with FrequencyFine = value })
  static member detune = (fun o -> o.Detune), (fun value o -> { o with Detune = value })

let initOperator() =
  { Enabled = false
    EGRate1 = 0uy
    EGRate2 = 0uy
    EGRate3 = 0uy
    EGRate4 = 0uy
    EGLevel1 = 0uy
    EGLevel2 = 0uy
    EGLevel3 = 0uy
    EGLevel4 = 0uy
    LevelScaleBreakpoint = 0uy
    LevelScaleLeftDepth = 0uy
    LevelScaleRightDepth = 0uy
    LevelScaleLeftCurve = 0uy
    LevelScaleRightCurve = 0uy
    OscillatorRateScale = 0uy
    Detune = 0uy
    FrequencyCoarse = 0uy
    FrequencyFine = 0uy
    OscillatorMode = 0uy
    AmpModSense = 0uy
    KeyVelocitySense = 0uy
    OperatorOutputLevel = 0uy }

type Patch = 
    { Algorithm: byte
      Feedback: byte
      OscillatorKeySync: byte
      Transpose: byte
      PitchRate1: byte
      PitchRate2: byte
      PitchRate3: byte
      PitchRate4: byte
      PitchLevel1: byte
      PitchLevel2: byte
      PitchLevel3: byte
      PitchLevel4: byte
      LFOSpeed: byte
      LFOWaveShape: byte
      LFOPitchModDepth: byte
      LFOAmpModDepth: byte
      LFODelay: byte
      LFOKeySync: byte
      PitchModSensitivity: byte
      PatchName: string
      Operator1: Operator
      Operator2: Operator
      Operator3: Operator
      Operator4: Operator
      Operator5: Operator
      Operator6: Operator }
    
    static member algorithm = (fun p -> p.Algorithm), (fun value p -> { p with Algorithm = value })
    static member feedback = (fun p -> p.Feedback), (fun value p -> { p with Feedback = value })
    static member oscillatorKeySync = (fun p -> p.OscillatorKeySync), (fun value p -> { p with OscillatorKeySync = value })
    static member transpose = (fun p -> p.Transpose), (fun value p -> { p with Transpose = value })
    static member pitchRate1 = (fun p -> p.PitchRate1), (fun value p -> { p with PitchRate1 = value })
    static member pitchRate2 = (fun p -> p.PitchRate2), (fun value p -> { p with PitchRate2 = value })
    static member pitchRate3 = (fun p -> p.PitchRate3), (fun value p -> { p with PitchRate3 = value })
    static member pitchRate4 = (fun p -> p.PitchRate4), (fun value p -> { p with PitchRate4 = value })
    static member pitchLevel1 = (fun p -> p.PitchLevel1), (fun value p -> { p with PitchLevel1 = value })
    static member pitchLevel2 = (fun p -> p.PitchLevel2), (fun value p -> { p with PitchLevel2 = value })
    static member pitchLevel3 = (fun p -> p.PitchLevel3), (fun value p -> { p with PitchLevel3 = value })
    static member pitchLevel4 = (fun p -> p.PitchLevel4), (fun value p -> { p with PitchLevel4 = value })
    static member lFOSpeed = (fun p -> p.LFOSpeed), (fun value p -> { p with LFOSpeed = value })
    static member lFOWaveShape = (fun p -> p.LFOWaveShape), (fun value p -> { p with LFOWaveShape = value })
    static member lFOPitchModDepth = (fun p -> p.LFOPitchModDepth), (fun value p -> { p with LFOPitchModDepth = value })
    static member lFOAmpModDepth = (fun p -> p.LFOAmpModDepth), (fun value p -> { p with LFOAmpModDepth = value })
    static member lFODelay = (fun p -> p.LFODelay), (fun value p -> { p with LFODelay = value })
    static member lFOKeySync = (fun p -> p.LFOKeySync), (fun value p -> { p with LFOKeySync = value })
    static member pitchModSensitivity = (fun p -> p.PitchModSensitivity), (fun value p -> { p with PitchModSensitivity = value })
    static member patchName = (fun p -> p.PatchName), (fun value p -> { p with PatchName = value })
    static member operator1 = (fun p -> p.Operator1), (fun value p -> { p with Operator1 = value })
    static member operator2 = (fun p -> p.Operator2), (fun value p -> { p with Operator2 = value })
    static member operator3 = (fun p -> p.Operator3), (fun value p -> { p with Operator3 = value })
    static member operator4 = (fun p -> p.Operator4), (fun value p -> { p with Operator4 = value })
    static member operator5 = (fun p -> p.Operator5), (fun value p -> { p with Operator5 = value })
    static member operator6 = (fun p -> p.Operator6), (fun value p -> { p with Operator6 = value })

type OperatorType = 
  | Carrier
  | Modulator

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

let initPatch () =
  { Algorithm = 0uy
    Feedback = 0uy
    OscillatorKeySync = 0uy
    Transpose = 0uy
    PitchRate1 = 0uy
    PitchRate2 = 0uy
    PitchRate3 = 0uy
    PitchRate4 = 0uy
    PitchLevel1 = 0uy
    PitchLevel2 = 0uy
    PitchLevel3 = 0uy
    PitchLevel4 = 0uy
    LFOSpeed = 0uy
    LFOWaveShape = 0uy
    LFOPitchModDepth = 0uy
    LFOAmpModDepth = 0uy
    LFODelay = 0uy
    LFOKeySync = 0uy
    PitchModSensitivity = 0uy
    PatchName = "Init"
    Operator1 = initOperator ()
    Operator2 = initOperator ()
    Operator3 = initOperator ()
    Operator4 = initOperator ()
    Operator5 = initOperator ()
    Operator6 = initOperator () }
let sysexHeader : byte list = 
    [ 0xf0uy // Status byte - start sysex
      0x43uy // ID # (i=67; Yamaha)
      0x10uy // Sub-status (s=1) & channel number (n=0; ch 1)
      0x00uy // format number (g=0; 1 voice)
      0x01uy // byte count MS byte
      0x1Buy // byte count LS byte (b=155; 1 voice)
    ]
 
let sysexEnd = [ 0xf7uy ]

let operatorSysex operatorModel =
  [ operatorModel.EGRate1
    operatorModel.EGRate2
    operatorModel.EGRate3
    operatorModel.EGRate4
    operatorModel.EGLevel1
    operatorModel.EGLevel2
    operatorModel.EGLevel3
    operatorModel.EGLevel4
    operatorModel.LevelScaleBreakpoint
    operatorModel.LevelScaleLeftDepth
    operatorModel.LevelScaleRightDepth
    operatorModel.LevelScaleLeftCurve
    operatorModel.LevelScaleRightCurve
    operatorModel.OscillatorRateScale
    operatorModel.AmpModSense
    operatorModel.KeyVelocitySense
    operatorModel.OperatorOutputLevel
    operatorModel.OscillatorMode
    operatorModel.FrequencyCoarse
    operatorModel.FrequencyFine
    operatorModel.Detune ]

let toSysexMessage patch =

  let operatorBits = 
    let o1 = if patch.Operator1.Enabled then 0b00100000uy else 0b00000000uy
    let o2 = if patch.Operator2.Enabled then 0b00010000uy else 0b00000000uy
    let o3 = if patch.Operator3.Enabled then 0b00001000uy else 0b00000000uy
    let o4 = if patch.Operator4.Enabled then 0b00000100uy else 0b00000000uy
    let o5 = if patch.Operator5.Enabled then 0b00000010uy else 0b00000000uy
    let o6 = if patch.Operator6.Enabled then 0b00000001uy else 0b00000000uy
    
    [ o1 ||| o2 ||| o3 ||| o4 ||| o5 ||| o6 ]

  let voiceName = 
    (patch.PatchName.PadRight(10, ' ').ToCharArray ())
    |> Array.toList
    |> List.map byte
    |> List.take 10

  sysexHeader
  |> List.append (operatorSysex patch.Operator6)
  |> List.append (operatorSysex patch.Operator5)
  |> List.append (operatorSysex patch.Operator4)
  |> List.append (operatorSysex patch.Operator3)
  |> List.append (operatorSysex patch.Operator2)
  |> List.append (operatorSysex patch.Operator1)
  |> List.append [ patch.PitchRate1
                   patch.PitchRate2
                   patch.PitchRate3
                   patch.PitchRate4
                   patch.PitchLevel1
                   patch.PitchLevel2
                   patch.PitchLevel3
                   patch.PitchLevel4
                   patch.Algorithm - 1uy
                   patch.Feedback
                   patch.OscillatorKeySync
                   patch.LFOSpeed
                   patch.LFODelay
                   patch.LFOPitchModDepth
                   patch.LFOAmpModDepth
                   patch.LFOKeySync
                   patch.LFOWaveShape
                   patch.PitchModSensitivity
                   patch.Transpose ]
   |> List.append voiceName
   |> List.append operatorBits
   |> List.append sysexEnd
   |> List.rev
   |> List.toArray
