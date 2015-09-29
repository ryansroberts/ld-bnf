#I "../../packages/FSharp.Data/lib/net40/"
#I "../../packages/FParsec/lib/net40-client/"
#I "../../packages/Newtonsoft.Json/lib/net45/"
#I "../../packages/FSharp.RDF/lib/"

#r "../../packages/FParsec/lib/net40-client/FParsec.dll"
#r "../../packages/FParsec/lib/net40-client/FParsecCS.dll"
#r "../../packages/FSharp.Data/lib/net40/FSharp.Data.dll"
#r "../../packages/Arachne.Core/lib/net45/Arachne.Core.dll"
#r "../../packages/Newtonsoft.Json/lib/net45/Newtonsoft.Json.dll"
#r "../../packages/FSharp.RDF/lib/FSharp.RDF.dll"

#r "../../packages/Arachne.Uri/lib/net45/Arachne.Uri.dll"
#r "System.Xml.Linq.dll"
#load "./drugmodel.fs"
#load "./rdf.fs"

open System
open System.IO

open System.Xml.Linq
open FSharp.Data
open Arachne.Uri
open Bnf.Drug
open Bnf.DrugParser
open FSharp.RDF

open resource
open Assertion
open rdf
open Bnf.DrugRdf

let private xmlFromFileSynch (fileName : string) =
    let file = File.OpenText fileName
    drugProvider.Load file

let d = xmlFromFileSynch "../../process/PHP1616.xml"

let drugModel = parse d

//let s = ""
//let sb = new System.Text.StringBuilder(s)

//let graph = Graph.from drugModel

//graph |> Graph.writeTtl (toString sb) |> ignore
