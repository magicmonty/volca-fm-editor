module VolcaFM

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
  { Enabled = true
    EGRate1 = 80uy
    EGRate2 = 0uy
    EGRate3 = 0uy
    EGRate4 = 80uy
    EGLevel1 = 99uy
    EGLevel2 = 0uy
    EGLevel3 = 0uy
    EGLevel4 = 0uy
    LevelScaleBreakpoint = 50uy
    LevelScaleLeftDepth = 0uy
    LevelScaleRightDepth = 0uy
    LevelScaleLeftCurve = 0uy
    LevelScaleRightCurve = 0uy
    OscillatorRateScale = 0uy
    Detune = 0uy
    FrequencyCoarse = 2uy
    FrequencyFine = 0uy
    OscillatorMode = 0uy
    AmpModSense = 0uy
    KeyVelocitySense = 0uy
    OperatorOutputLevel = 99uy }

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
  { Algorithm = 31uy
    Feedback = 0uy
    OscillatorKeySync = 0uy
    Transpose = 24uy
    PitchRate1 = 50uy
    PitchRate2 = 50uy
    PitchRate3 = 50uy
    PitchRate4 = 50uy
    PitchLevel1 = 50uy
    PitchLevel2 = 50uy
    PitchLevel3 = 50uy
    PitchLevel4 = 50uy
    LFOSpeed = 0uy
    LFOWaveShape = 0uy
    LFOPitchModDepth = 0uy
    LFOAmpModDepth = 0uy
    LFODelay = 0uy
    LFOKeySync = 0uy
    PitchModSensitivity = 0uy
    PatchName = "Init"
    Operator1 = initOperator ()
    Operator2 = { initOperator () with FrequencyCoarse = 1uy; OperatorOutputLevel = 92uy }
    Operator3 = { initOperator () with OperatorOutputLevel = 92uy }
    Operator4 = { initOperator () with FrequencyCoarse = 1uy; OperatorOutputLevel = 92uy }
    Operator5 = { initOperator () with OperatorOutputLevel = 92uy }
    Operator6 = { initOperator () with FrequencyCoarse = 1uy; OperatorOutputLevel = 92uy } }
let sysexHeader : byte list = 
    [ 0xf0uy // Status byte - start sysex
      0x43uy // ID # (i=67; Yamaha)
      0x00uy // Sub-status (s=0) & channel number (n=0; ch 1)
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

let calcChecksum (data: byte array) =
  let sum = data
            |> Array.skip 6
            |> Array.take 155
            |> Array.sum
  ((~~~(sum &&& 0xFFuy)) + 1uy) &&& 0x7Fuy

let toSysexMessage patch : byte list =

  let operatorBits = 
    let o1 = if patch.Operator1.Enabled then 0b00100000uy else 0b00000000uy
    let o2 = if patch.Operator2.Enabled then 0b00010000uy else 0b00000000uy
    let o3 = if patch.Operator3.Enabled then 0b00001000uy else 0b00000000uy
    let o4 = if patch.Operator4.Enabled then 0b00000100uy else 0b00000000uy
    let o5 = if patch.Operator5.Enabled then 0b00000010uy else 0b00000000uy
    let o6 = if patch.Operator6.Enabled then 0b00000001uy else 0b00000000uy
    
    [ 0b01000000uy ||| o1 ||| o2 ||| o3 ||| o4 ||| o5 ||| o6 ]

  let voiceName = 
    (patch.PatchName.PadRight(10, ' ').ToCharArray ())
    |> Array.toList
    |> List.map byte
    |> List.take 10

  let result =
    List.concat [
      sysexHeader
      operatorSysex patch.Operator6
      operatorSysex patch.Operator5
      operatorSysex patch.Operator4
      operatorSysex patch.Operator3
      operatorSysex patch.Operator2
      operatorSysex patch.Operator1
      [ patch.PitchRate1
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
      voiceName
      operatorBits ]

  List.append result [ calcChecksum (result |> List.toArray)
                       0xF7uy ]

let validateSysexData (data: byte array) : string option =
  if data.Length = 163 then
    if data.[0] <> 0xF0uy then
      Some "doesn't start with sysex byte"
    elif data.[1] <> 0x43uy then
      Some "not a yamaha sysex"
    elif data.[162] <> 0xf7uy then
      Some "doesn't end with EOX"
    elif (data.[2] &&& 0x70uy) <> 0x00uy then
      Some "sub status is not correct"
    elif data.[3] <> 0x00uy then
      Some "format isn't voice"
    elif (data.[4] <> 0x01uy || data.[5] <> 0x1Buy) then
      Some "length indicator is not correct"
    elif (calcChecksum data <> data.[161]) then
      Some "checksum failed"
    else None
  else
    if data.Length <> 164 then
      Some "wrong length"
    elif data.[0] <> 0xF0uy then
      Some "doesn't start with sysex byte"
    elif data.[1] <> 0x43uy then
      Some "not a yamaha sysex"
    elif data.[163] <> 0xf7uy then
      Some "doesn't end with EOX"
    elif (data.[2] &&& 0x70uy) <> 0x00uy then
      Some "sub status is not correct"
    elif data.[3] <> 0x00uy then
      Some "format isn't voice"
    elif (data.[4] <> 0x01uy || data.[5] <> 0x1Buy) then
      Some "length indicator is not correct"
    elif (calcChecksum data <> data.[162]) then
      Some "checksum failed"
    else None

let loadPatch (data: byte array) =
  let hasEnabledByte = data.Length = 164

  let loadOperator isEnabled (opData: byte array) =
    { Enabled = isEnabled
      EGRate1 = opData.[0]
      EGRate2 = opData.[1]
      EGRate3 = opData.[2]
      EGRate4 = opData.[3]
      EGLevel1 = opData.[4]
      EGLevel2 = opData.[5]
      EGLevel3 = opData.[6]
      EGLevel4 = opData.[7]
      LevelScaleBreakpoint = opData.[8]
      LevelScaleLeftDepth = opData.[9]
      LevelScaleRightDepth = opData.[10]
      LevelScaleLeftCurve = opData.[11]
      LevelScaleRightCurve = opData.[12]
      OscillatorRateScale = opData.[13]
      AmpModSense = opData.[14]
      KeyVelocitySense = opData.[15]
      OperatorOutputLevel = opData.[16]
      OscillatorMode = opData.[17]
      FrequencyCoarse = opData.[18]
      FrequencyFine = opData.[19]
      Detune = opData.[20] }

  match validateSysexData data with
  | Some _ -> None
  | _ ->
    let operatorDataSize = 21
    let body = data |> Array.skip 6
    let getOperator offset isEnabled =
      body 
      |> Array.skip (offset * operatorDataSize) 
      |> Array.take operatorDataSize 
      |> loadOperator isEnabled

    let globalData = body |> Array.skip (6 * operatorDataSize)
    let patchName = 
      globalData
      |> Array.skip 19 
      |> Array.take 10
      |> Array.map char
      |> Array.fold (fun c n -> sprintf "%s%c" c n) ""
      |> fun s -> s.Trim()

    let isEnabled mask =
      (not hasEnabledByte) || (globalData.[29] &&& mask <> 0uy)

    { PitchRate1 = globalData.[0]
      PitchRate2 = globalData.[1]
      PitchRate3 = globalData.[2]
      PitchRate4 = globalData.[3]
      PitchLevel1 = globalData.[4]
      PitchLevel2 = globalData.[5]
      PitchLevel3 = globalData.[6]
      PitchLevel4 = globalData.[7]
      Algorithm = globalData.[8]
      Feedback = globalData.[9]
      OscillatorKeySync = globalData.[10]
      LFOSpeed = globalData.[11]
      LFODelay = globalData.[12]
      LFOPitchModDepth = globalData.[13]
      LFOAmpModDepth = globalData.[14]
      LFOKeySync = globalData.[15]
      LFOWaveShape = globalData.[16]
      PitchModSensitivity = globalData.[17]
      Transpose = globalData.[18]
      PatchName = patchName
      Operator1 = getOperator 5 (isEnabled 0b00100000uy)
      Operator2 = getOperator 4 (isEnabled 0b00010000uy)
      Operator3 = getOperator 3 (isEnabled 0b00001000uy)
      Operator4 = getOperator 2 (isEnabled 0b00000100uy)
      Operator5 = getOperator 1 (isEnabled 0b00000010uy)
      Operator6 = getOperator 0 (isEnabled 0b00000001uy) } |> Some

[<RequireQualifiedAccess>]
module Patch =
  let encode patch : string =
    patch |> toSysexMessage |> List.toArray |> Base64.fromByteArray

  let decode s : Patch option =
    s |> Base64.toByteArray |> loadPatch
