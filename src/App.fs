module VolcaEditor.App

open Fable.Core.JsInterop
open Elmish
open Elmish.React

importSideEffects "whatwg-fetch"
importSideEffects "babel-polyfill"

#if DEBUG
open Elmish.HMR
open Elmish.Debug
#endif

// App
Program.mkProgram State.init State.update State.view
#if DEBUG
|> Program.withConsoleTrace
|> Program.withHMR
#endif
|> Program.withReact "elmish-app"

#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
