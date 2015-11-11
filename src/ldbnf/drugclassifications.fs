namespace Bnf
open FSharp.Data
open System.Xml.Linq
open System.Xml.XPath
open FSharp.RDF
open FSharp.Data.Runtime
open Shared

module DrugClassification =
  type dcProvider = XmlProvider<"drugClassifications.xml", Global=true>

  type Classification = {key:string; value:string}

  type DrugClassifications = | DrugClassifications of Classification list

module DrugClassificationParser =
  open DrugClassification

  type Classification with
    static member from (x:dcProvider.Section) =
      let k = x.Ps.[0].Value
      let v = x.Ps.[1].Value
      {key = k; value = v}

  type DrugClassifications with
    static member parse (x:dcProvider.Topic) =
      let cs = x.Body.Sections |> Array.map Classification.from |> Array.toList
      DrugClassifications(cs)
