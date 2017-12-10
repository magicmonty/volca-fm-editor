module Client.Style

open Fable.Import.React

module R = Fable.Helpers.React
module P = R.Props

type Alert =
  | Info of string
  | Success of string
  | Warning of string
  | Error of string

let alert p =
  let alertClass = match p with
                   | Info _ -> "alert-info"      
                   | Success _ -> "alert-success"      
                   | Warning _ -> "alert-warning"
                   | Error  _-> "alert-danger"

  let msg = match p with
            | Info msg -> msg
            | Success msg -> msg
            | Warning msg -> msg
            | Error msg -> msg

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
    R.label [ P.ClassName "col-form-label col-form-label-sm" ] [ R.str title ]
    R.select (List.append [ P.ClassName "form-control form-control-sm" ] props) content ]


let githubBanner =
  R.a [ P.Href "https://github.com/magicmonty/volca-fm-editor" ] [
    R.img [ P.Style [ P.Position "absolute" 
                      P.Top "0"
                      P.Right "0" 
                      P.Border "0"
                      P.ZIndex 9999. ] 
            P.Src "https://camo.githubusercontent.com/e7bbb0521b397edbd5fe43e7f760759336b5e05f/68747470733a2f2f73332e616d617a6f6e6177732e636f6d2f6769746875622f726962626f6e732f666f726b6d655f72696768745f677265656e5f3030373230302e706e67"
            P.Alt "Fork me on GitHub"
            P.Data ("canonical-src", "https://s3.amazonaws.com/github/ribbons/forkme_right_green_007200.png")
            P.Target "_blank" ]
    ]