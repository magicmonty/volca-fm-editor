module Client.Style

open Fable.Import.React
open Fable
open Fable.AST.Fable

module R = Fable.Helpers.React
module P = R.Props
module K = VolcaEditor.Knob
module ED = VolcaEditor.EnvDisplay

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

  R.div [ P.ClassName ("alert " + alertClass + " small") ] [ R.str msg ]


let radioInline title value isChecked (onClick: MouseEvent -> unit) =
    R.div [ P.ClassName "form-check form-check-inline m-0 mr-2" ] [
      R.label [ P.ClassName "form-check-label small" ] [
        R.input [ P.Type "radio"
                  P.ClassName "mr-1"
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

let container content = R.div [ P.ClassName "container-fluid" ] content
let row content = R.div [ P.ClassName "row" ] content
let col content = R.div [ P.ClassName "col" ] content

let rowcol content = row [ col content ]

let card title content =
  R.div [ P.ClassName "card" ] [
    R.div [ P.ClassName "card-header" ] [ R.strong [] [ R.str title ] ]
    R.div [ P.ClassName "card-body container-fluid" ] content
  ]

let cardWithHeaderContent title (header: ReactElement list) content =
  R.div [ P.ClassName "card" ] [
    R.div [ P.ClassName "card-header" ] [ yield R.strong [] [ R.str title ]
                                          for e in header do
                                            yield e ]
    R.div [ P.ClassName "card-body container-fluid" ] content
  ]

let label text =
  R.label [ P.ClassName "text-muted text-center m-0 "
            P.Style [ P.CSSProp.FontSize "12px" ] ] [ R.str text ]

let toggle title value1Text value1 value2Text value2 currentValue (dispatch: byte -> unit) =
  R.div [ P.ClassName "text-center mb-3" ] [
    R.div [ P.ClassName "text-center m-0 mb-2 p-0 pt-4"
            P.Style [ P.CSSProp.Height "60px" ] ] [
      radioInline value1Text (string value1) (currentValue = value1) (fun _ -> dispatch value1)
      radioInline value2Text (string value2) (currentValue = value2) (fun _ -> dispatch value2)
    ]
    R.div [ P.ClassName "mt-0 p-0"
            P.Style [ P.LineHeight "14px" ] ] [
      label title
    ]
  ]

let knob min max format dispatch onComplete description (value: byte) event =
  let size = 55
  let digits = ([ (min |> string |> String.length); (max |> string |> String.length); 2 ] |> List.max) + 2
  let inputColor = "#666"
  let renderCustomCenter () =
    R.span [ P.Style [ P.CSSProp.Width (sprintf "%ipx" ((size / 2) + 4)) 
                       P.CSSProp.Height (sprintf "%ipx" (size / 3)) 
                       P.Position "absolute"
                       P.VerticalAlign "middle"
                       P.MarginTop (sprintf "%ipx" (size / 3))
                       P.MarginLeft (sprintf "-%ipx" ((size * 3 / 4) + 2))
                       P.Border "0"
                       P.BackgroundColor "none"
                       P.Font (sprintf "bold %ipx Arial" (size / digits))
                       P.TextAlign "center"
                       P.Color inputColor
                       P.Padding "0" ]
             P.Type "text"
             P.ReadOnly true ] [ R.str (format (int value)) ]
  R.div [ P.ClassName "text-center mb-3 " ] [
    R.div [ P.ClassName "text-center m-0 p-0"
            P.Style [ P.CSSProp.Height (sprintf "%ipx" (size + 5)) ] ] [
      K.knob [ K.Width size
               K.Min (float min)
               K.Max (float max)
               K.Value (float value)
               K.DisableTextInput true
               K.DisableMouseWheel true
               K.AngleArc 270.
               K.AngleOffset 225.
               K.Step 1.
               K.Stopper true
               K.Clockwise true
               K.LineCap K.Round
               K.InputColor inputColor
               K.FgColor "#7cb342"
               K.Title description
               K.OnChange (byte >> event >> dispatch)
               K.OnChangeEnd (fun _ -> onComplete ())
               K.DisplayInput false
               K.DisplayCustom renderCustomCenter]
    ]
    R.div [ P.ClassName "mt-0 p-0"
            P.Style [ P.LineHeight "14px" ] ] [
      label description
    ]
  ]