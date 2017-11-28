// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------

#r @"packages/build/FAKE/tools/FakeLib.dll"
#r @"packages/build/Newtonsoft.Json/lib/net45/Newtonsoft.Json.dll"

open Fake
open Fake.Git
open Fake.AssemblyInfoFile
open Fake.ReleaseNotesHelper
open System
open System.IO
open Fake.Testing.Expecto

let project = "Volca FM Patch Editor"
let summary = "Volca FM Patch Editor"
let description = summary
let configuration = "Release"
let clientPath = "./src/Client" |> FullName
let serverPath = "./src/Server/" |> FullName

open Newtonsoft.Json
open Newtonsoft.Json.Linq

let dotnetcliVersion : string = 
    try
        let content = File.ReadAllText "global.json"
        let json = Newtonsoft.Json.Linq.JObject.Parse content
        let sdk = json.Item("sdk") :?> JObject
        let version = sdk.Property("version").Value.ToString()
        version
    with
    | exn -> failwithf "Could not parse global.json: %s" exn.Message

let mutable dotnetExePath = "dotnet"

let deployDir = "./deploy"

// --------------------------------------------------------------------------------------
// END TODO: The rest of the file includes standard build steps
// --------------------------------------------------------------------------------------


let run' timeout cmd args dir =
    if execProcess (fun info ->
        info.FileName <- cmd
        if not (String.IsNullOrWhiteSpace dir) then
            info.WorkingDirectory <- dir
        info.Arguments <- args
    ) timeout |> not then
        failwithf "Error while running '%s' with args: %s" cmd args

let run = run' System.TimeSpan.MaxValue

let runDotnet workingDir args =
    let result =
        ExecProcess (fun info ->
            info.FileName <- dotnetExePath
            info.WorkingDirectory <- workingDir
            info.Arguments <- args) TimeSpan.MaxValue
    if result <> 0 then failwithf "dotnet %s failed" args

let platformTool tool winTool =
    let tool = if isUnix then tool else winTool
    tool
    |> ProcessHelper.tryFindFileOnPath
    |> function Some t -> t | _ -> failwithf "%s not found" tool

let nodeTool = platformTool "node" "node.exe"
let npmTool = platformTool "npm" "npm.cmd"
let yarnTool = platformTool "yarn" "yarn.cmd"

do if not isWindows then
    // We have to set the FrameworkPathOverride so that dotnet sdk invocations know
    // where to look for full-framework base class libraries
    let mono = platformTool "mono" "mono"
    let frameworkPath = IO.Path.GetDirectoryName(mono) </> ".." </> "lib" </> "mono" </> "4.5"
    setEnvironVar "FrameworkPathOverride" frameworkPath


// Read additional information from the release notes document
let releaseNotes = File.ReadAllLines "RELEASE_NOTES.md"

let releaseNotesData =
    releaseNotes
    |> parseAllReleaseNotes

let release = List.head releaseNotesData

let packageVersion = SemVerHelper.parse release.NugetVersion


// --------------------------------------------------------------------------------------
// Clean build results

Target "Clean" (fun _ ->
    !!"src/**/bin"
    |> CleanDirs

    !! "src/**/obj/*.nuspec"
    |> DeleteFiles

    CleanDirs ["bin"; "temp"; "docs/output"; deployDir; Path.Combine(clientPath,"public/bundle")]
)

Target "InstallDotNetCore" (fun _ ->
    dotnetExePath <- DotNetCli.InstallDotNetSDK dotnetcliVersion
)

// --------------------------------------------------------------------------------------
// Build library & test project


Target "BuildServer" (fun _ ->
    runDotnet serverPath "build"
)


Target "InstallClient" (fun _ ->
    printfn "Node version:"
    run nodeTool "--version" __SOURCE_DIRECTORY__
    printfn "Yarn version:"
    run yarnTool "--version" __SOURCE_DIRECTORY__
    run yarnTool "install --frozen-lockfile" __SOURCE_DIRECTORY__
)

Target "BuildClient" (fun _ ->
    runDotnet clientPath "restore"
    runDotnet clientPath "fable webpack -- -p"
)

// --------------------------------------------------------------------------------------
// Run the Website

let ipAddress = "localhost"
let port = 8080

FinalTarget "KillProcess" (fun _ ->
    killProcess "dotnet"
    killProcess "dotnet.exe"
)


Target "Run" (fun _ ->
    runDotnet clientPath "restore"

    let fablewatch = async { runDotnet clientPath "fable webpack-dev-server" }
    let openBrowser = async {
        System.Threading.Thread.Sleep(5000)
        Diagnostics.Process.Start("http://"+ ipAddress + sprintf ":%d" port) |> ignore }

    Async.Parallel [| fablewatch; openBrowser |]
    |> Async.RunSynchronously
    |> ignore
)


// --------------------------------------------------------------------------------------
// Release Scripts


Target "SetReleaseNotes" (fun _ ->
    let lines = [
            "module internal ReleaseNotes"
            ""
            (sprintf "let Version = \"%s\"" release.NugetVersion)
            ""
            (sprintf "let IsPrerelease = %b" (release.SemVer.PreRelease <> None))
            ""
            "let Notes = \"\"\""] @ Array.toList releaseNotes @ ["\"\"\""]
    File.WriteAllLines("src/Client/ReleaseNotes.fs",lines)
)

Target "PrepareRelease" (fun _ ->
    Git.Branches.checkout "" false "master"
    Git.CommandHelper.directRunGitCommand "" "fetch origin" |> ignore
    Git.CommandHelper.directRunGitCommand "" "fetch origin --tags" |> ignore

    StageAll ""
    Git.Commit.Commit "" (sprintf "Bumping version to %O" release.NugetVersion)
    Git.Branches.pushBranch "" "origin" "master"

    let tagName = string release.NugetVersion
    Git.Branches.tag "" tagName
    Git.Branches.pushTag "" "origin" tagName
)

Target "BundleClient" (fun _ ->
    let result =
        ExecProcess (fun info ->
            info.FileName <- dotnetExePath
            info.WorkingDirectory <- serverPath
            info.Arguments <- "publish -c Release -o \"" + FullName deployDir + "\"") TimeSpan.MaxValue
    if result <> 0 then failwith "Publish failed"

    let clientDir = deployDir </> "client"
    let publicDir = clientDir </> "public"
    let jsDir = clientDir </> "js"
    let cssDir = clientDir </> "css"
    let imageDir = clientDir </> "Images"

    !! "src/Client/public/**/*.*" |> CopyFiles publicDir
    !! "src/Client/js/**/*.*" |> CopyFiles jsDir
    !! "src/Client/css/**/*.*" |> CopyFiles cssDir
    !! "src/Client/Images/**/*.*" |> CopyFiles imageDir

    "src/Client/index.html" |> CopyFile clientDir
)

// -------------------------------------------------------------------------------------
Target "Build" DoNothing
Target "All" DoNothing

"Clean"
  ==> "InstallDotNetCore"
  ==> "InstallClient"
  ==> "SetReleaseNotes"
  ==> "BuildServer"
  ==> "BuildClient"
  ==> "BundleClient"
  ==> "All"
  ==> "PrepareRelease"

"BuildClient"
  ==> "Build"

"InstallClient"
  ==> "Run"

RunTargetOrDefault "All"
