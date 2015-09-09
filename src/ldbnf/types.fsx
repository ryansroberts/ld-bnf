#I "../../packages/FSharp.Data/lib/net40/"
#I "../../packages/FParsec/lib/net40-client/"
#r "../../packages/FParsec/lib/net40-client/FParsec.dll"
#r "../../packages/FParsec/lib/net40-client/FParsecCS.dll"
#r "../../packages/FSharp.Data/lib/net40/FSharp.Data.dll"
#r "../../packages/Arachne.Core/lib/net45/Arachne.Core.dll"


#r "../../packages/Arachne.Uri/lib/net45/Arachne.Uri.dll"
#r "System.Xml.Linq.dll"
#load "./drugmodel.fs"

open FSharp.Data
open Arachne.Uri
open Bnf.Drug
open Bnf.DrugParser

let drugXml = drugProvider.GetSample()

let drugModel = parse drugXml
