module Client.Style

open Fable.Import.React

module R = Fable.Helpers.React
module P = R.Props

type MessagePriority =
  | Info
  | Success
  | Warning
  | Error

type Alert = MessagePriority*string

let alert (p, msg) =
  let alertClass = match p with
                   | Info -> "alert-info"      
                   | Success -> "alert-success"      
                   | Warning -> "alert-warning"
                   | Error -> "alert-danger"
  R.div [ P.ClassName ("alert " + alertClass) ] [ R.str msg ]


let radioInline title value isChecked (onClick: MouseEvent -> unit) =
    R.div [ P.ClassName "form-check form-check-inline" ] [
      R.label [ P.ClassName "form-check-label" ] [
        R.input [ P.Type "radio"
                  P.ClassName "form-check-input"
                  P.Checked isChecked
                  P.Value value
                  P.OnClick onClick ]
        R.str title
      ]
    ]


let select title props content = 
  R.div [ P.ClassName "form-group" ] [
    R.label (List.append [ P.ClassName "col-form-label" ] props) [ R.str title ]
    R.select [ P.ClassName "form-control" ] content ]
