namespace Bnf
open FSharp.Data
open System.Xml.Linq
open System.Xml.XPath
open Shared

module TreatmentSummary =
  type tsProvider = XmlProvider<"supertreatmentsummary.xml", Global=true>

  type Title = | Title of string
  type TargetAudience = | TargetAudience of string
  type Link = {uri:string;label:string}
  type Content = | Content of tsProvider.Section * TargetAudience option
  type BodySystem = | BodySystem of string

  type Summary = {
    title:Title;
    doi:Doi;
    bodySystem:BodySystem;
    content:Content list;
    links:Link seq
  }

  type Treatment =
    | ComparativeInformation of Summary
    | ManagementOfConditions of Summary
    | MedicalEmergenciesBodySystems of Summary
    | TreatmentOfBodySystems of Summary
    | Generic of Summary

  type TreatmentSummary = | TreatmentSummary of Id * Treatment

module TreatmentSummaryParser =
  open TreatmentSummary

  let inline name arg =
    ( ^a : (member Name : string) arg)

  let inline (|HasName|_|) n x =
    if (name x) = n then Some(x)
    else None

  let inline hasName s x = name x = s

  let inline outputclass arg = ( ^a : (member Outputclass : string) arg)

  let inline (|HasOutputClass|_|) (n:string) x =
    let cs = (outputclass x).Split [|' '|]
    if (cs |> Array.exists (fun  c -> c = n)) then Some(x)
    else None

  let withname = (|HasName|_|)

  let (>>=) a b = Option.bind b a

  type Doi with
    static member from (x:tsProvider.Data) = Doi(x.Value) |> Some

  type BodySystem with
    static member from (x:tsProvider.Data) = BodySystem(x.Value) |> Some

  type Content with
    static member from (x:tsProvider.Section) =
      let ta = match x.Outputclass with
               | Some x -> TargetAudience x |> Some
               | None -> None
      Content(x,ta)

  type Link with
    static member from (x:XElement) = 
     let uri = x.Attribute(XName.Get "href").Value
     {uri = uri; label = x.Value}
 
  type Summary with
    static member from (x:tsProvider.Topic) =
      let ls = x.XElement.XPathSelectElements("//xref") |> Seq.map Link.from
      let t = Title(x.Title)
      let d = x.Body.Datas |> Array.choose (withname "doi") |> Array.pick Doi.from
      let bs = x.Body.Datas |> Array.choose (withname "bodySystem") |> Array.pick BodySystem.from
      let c = x.Body.Sections |> Array.map Content.from |> Array.toList
      Id(x.Id),{title = t; doi = d; bodySystem = bs; content = c; links = ls}

  type TreatmentSummary with
    static member from c (i,s) =
      TreatmentSummary(i, c s)

  type TreatmentSummary with
    static member parse (x:tsProvider.Topic) =
      match x with
        | HasOutputClass "comparativeInformation" t -> Summary.from t |> TreatmentSummary.from ComparativeInformation
        | HasOutputClass "managementOfConditions" t -> Summary.from t |> TreatmentSummary.from ManagementOfConditions
        | HasOutputClass "medicalEmergenciesBodySystems" t -> Summary.from t |> TreatmentSummary.from MedicalEmergenciesBodySystems
        | HasOutputClass "treatmentOfBodySystems" t -> Summary.from t |> TreatmentSummary.from TreatmentOfBodySystems
        | t -> Summary.from t |> TreatmentSummary.from Generic
