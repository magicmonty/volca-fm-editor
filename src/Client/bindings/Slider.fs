module Client.Bindings.Slider

open Fable.Core
open Fable.Import
open Fable.Import.React
open Fable.Import.Browser
open Fable.PowerPack.Date.Local
open Fable.Core
open Fable.Helpers

type ISliderProp = 
    inherit Fable.Helpers.React.Props.IHTMLProp
    inherit Fable.Helpers.React.Props.ICSSProp

type SliderProps = 
    | Min of int
    | Max of int
    | Step of int
    | Value of int
    | Orientation of string
    | Tooltip of bool
    | Reverse of bool
    | Labels of int*string list
    | HandleLabel of string
    | Format of (int -> string)
    | OnChange of (int -> unit)
    | OnChangeStart of (unit -> unit)
    | OnChangeComplete of (unit -> unit)
    interface ISliderProp

JsInterop.importSideEffects "react-rangeslider/lib/index.css"
let Slider : React.ComponentClass<obj> = JsInterop.importDefault "react-rangeslider/lib/index.js"

let inline slider (props: ISliderProp list) =
    Fable.Helpers.React.from Slider (JsInterop.keyValueList CaseRules.LowerFirst props) []