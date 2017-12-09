module Client.Bindings.Slider

open Fable.Core
open Fable.Import
open Fable.Helpers

type ISliderProp = 
    inherit React.Props.IHTMLProp
    inherit React.Props.ICSSProp

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
    React.from Slider (JsInterop.keyValueList CaseRules.LowerFirst props) []