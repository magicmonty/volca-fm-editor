module VolcaEditor.EnvDisplay

open Fable.Core
open Fable.Helpers

module B = Fable.Import.Browser
module V = VolcaFM

type Model = { Operator: V.Operator
               Id: string }

type Msg =
    | OperatorChanged of V.Operator
    | Redraw

let init id operator : Model =
    { Operator = operator
      Id = id }

let draw model (canvas: B.HTMLCanvasElement) =
    let canvasHeight = canvas.height
    let canvasWidth = canvas.width
    let baseLine = canvasHeight
    let envHeight = canvasHeight
    let maxStepWidth = canvasWidth - 2. * (canvasWidth / 10.)

    let l1 = float model.Operator.EGLevel1
    let l2 = float model.Operator.EGLevel2
    let l3 = float model.Operator.EGLevel3
    let l4 = float model.Operator.EGLevel4
    let r1 = float model.Operator.EGRate1
    let r2 = float model.Operator.EGRate2
    let r3 = float model.Operator.EGRate3
    let r4 = float model.Operator.EGRate4

    // calculate final width so we can apply scaling if needed:
    let stepOneX = ((abs(l4 - l1) / (r1 + 1.)) * maxStepWidth) / 100.
    let stepTwoX = ((abs(l1 - l2) / (r2 + 1.)) * maxStepWidth) / 100.
    let stepThreeX = ((abs(l2 - l3) / (r3 + 1.)) * maxStepWidth) / 100.
    let stepFourX = ((abs(l3 - l4) / (r4 + 1.)) * maxStepWidth) / 100.
    let easeInWidth = (stepOneX + stepTwoX + stepThreeX + stepFourX) / 10.
    let finalWidth = (stepOneX + stepTwoX + stepThreeX + stepFourX) + (easeInWidth * 4.)
    let scale = finalWidth / canvasWidth

    let ctx = canvas.getContext_2d ()
    ctx.setTransform (1., 0., 0., 1., 0., 0.)
    ctx.clearRect(0., 0., canvasWidth, canvasHeight)
    ctx.fillStyle <- (U3<_,_,_>.Case1 "rgba(0,0,0,0)")
    ctx.fillRect(0., 0., canvasWidth, canvasHeight)
    ctx.scale(1./scale, 1.)
    ctx.lineWidth <- scale
    ctx.lineCap <- "round"
    ctx.lineJoin <- "round"
    ctx.strokeStyle <- (U3<_,_,_>.Case1 "rgb(41, 163, 67)")

    let levelScale l = baseLine - (l * envHeight) / 100. 
    let l1 = levelScale l1
    let l2 = levelScale l2
    let l3 = levelScale l3
    let l4 = levelScale l4

    // EASE IN
    ctx.beginPath()
    ctx.moveTo(0., l4)
    ctx.lineTo(easeInWidth, l4)

    // MAIN ENV PT.1
    let s1 = easeInWidth + stepOneX
    ctx.lineTo(s1, l1)
    let s2 = s1 + stepTwoX
    ctx.lineTo(s2, l2)
    let s3 = s2 + stepThreeX
    ctx.lineTo(s3, l3)

    // Sustain
    let s3ease = s3 + (easeInWidth * 2.)
    ctx.lineTo(s3ease, l3)
    
    // MAIN ENV PT.2
    let s4 = s3ease + stepFourX
    ctx.lineTo(s4, l4)

    // EASE OUT
    ctx.lineTo(s4 + easeInWidth, l4)
    ctx.strokeStyle <- (U3<_,_,_>.Case1 "rgb(0, 0, 0)")
    ctx.stroke()

let drawCanvas model =
    let canvas : B.HTMLCanvasElement option = unbox (B.document.getElementById model.Id)
    match canvas with
    | None -> B.console.log ("Canvas not initialized yet for element " + model.Id)
    | Some canvas -> draw model canvas

    model

let update msg model =
    match msg with
    | OperatorChanged op -> { model with Operator = op }
    | Redraw -> model
    |> drawCanvas

module R = Fable.Helpers.React
module P = Fable.Helpers.React.Props
let view (model: Model) dispatch =
    R.canvas [ P.Width 140
               P.Height 40
               P.Id model.Id
               P.Ref (fun element -> if element |> isNull |> not then
                                        draw model (unbox element))
               P.Style [ P.Right "0px"
                         P.Top "5px"
                         P.Position "absolute" ] ] [] 
