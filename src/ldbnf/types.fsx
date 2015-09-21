#I "../../packages/FSharp.Data/lib/net40/"
#I "../../packages/FParsec/lib/net40-client/"
#I "../../packages/FsPickler/lib/net40/"
#I "../../packages/FsPickler.Json/lib/net40/"
#I "../../packages/Newtonsoft.Json/lib/net45/"

#r "../../packages/FParsec/lib/net40-client/FParsec.dll"
#r "../../packages/FParsec/lib/net40-client/FParsecCS.dll"
#r "../../packages/FSharp.Data/lib/net40/FSharp.Data.dll"
#r "../../packages/Arachne.Core/lib/net45/Arachne.Core.dll"
#r "../../packages/Newtonsoft.Json/lib/net45/Newtonsoft.Json.dll"

#r "../../packages/FsPickler/lib/net40/FsPickler.dll"
#r "../../packages/FsPickler.Json/lib/net40/FsPickler.Json.dll"

#r "../../packages/Arachne.Uri/lib/net45/Arachne.Uri.dll"
#r "System.Xml.Linq.dll"
#load "./drugmodel.fs"

open System.Xml.Linq
open FSharp.Data
open Arachne.Uri
open Bnf.Drug
open Bnf.DrugParser
open Nessos.FsPickler
open Nessos.FsPickler.Json


let factory = 
  { new IPicklerFactory<XElement> with 
      member __.Create (resolver : IPicklerResolver) =
        let sp = resolver.Resolve<string>()
        let writer (ws: WriteState) (c: XElement) =
          let str = c.ToString()
          sp.Write ws "xml" str
        let reader (rs: ReadState) =
          let s = sp.Read rs "xml"
          XElement.Parse s

        Pickler.FromPrimitives(reader, writer)
  }

FsPickler.RegisterPicklerFactory factory

let drugModel = parse(drugProvider.GetSample())

let jsonSerializer = FsPickler.CreateJsonSerializer(indent = true)
let text = jsonSerializer.PickleToString drugModel
