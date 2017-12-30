module VolcaEditor.Knob

open Fable.Core
open Fable.Import
open Fable.Helpers
open Fable.Import.React

type IKnobProp = 
    inherit React.Props.IHTMLProp
    inherit React.Props.ICSSProp

[<StringEnum>]
type LineCapType = | Butt | Round

type KnobProps = 
    | Value of float
    | OnChange of (float -> unit)
    | OnChangeEnd of (float -> unit)
    | Min of float
    | Max of float
    | Step of float
    | Log of bool
    | Width of int
    | Height of int
    | Thickness of float
    | LineCap of LineCapType
    | BgColor of string
    | FgColor of string
    | InputColor of string
    | Font of string
    | FontWeight of string
    | Clockwise of bool
    | Stopper of bool
    | Readonly of bool
    | DisableTextInput of bool
    | DisplayInput of bool
    | AngleArc of float
    | AngleOffset of float
    | DisableMouseWheel of bool
    | Title of string
    | CanvasClassName of string
    | DisplayCustom of (unit -> ReactElement)
    interface IKnobProp

let Knob : React.ComponentClass<obj> = JsInterop.importDefault "react-canvas-uiknob/index.js"

let inline knob (props: IKnobProp list) =
    React.from Knob (JsInterop.keyValueList CaseRules.LowerFirst props) []
