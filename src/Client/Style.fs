module Client.Style

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Import.React

let radioInline title value isChecked (onClick: MouseEvent -> unit) =
    div [ ClassName "form-check form-check-inline" ] [
      label [ ClassName "form-check-label" ] [
        input [ Type "radio"
                ClassName "form-check-input"
                Checked isChecked
                Value value
                OnClick onClick ]
        str title
      ]
    ]
