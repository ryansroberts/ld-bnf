namespace Bnf
open FSharp.Data
open Shared

module TreatmentSummary =
  type tsProvider = XmlProvider<"supertreatmentsummary.xml", Global=true>

  type Title = | Title of string
  type TargetAudience = | TargetAudience of string
  type Content = | Content of tsProvider.Section * TargetAudience
  type BodySystem = | BodySystem of string

  type Summary =
    | Summary
//=
    //{
    //title:Title;
    //doi:Doi;
    //bodySystem:BodySystem;
    //targetAudience:TargetAudience;
    //contents:Content seq;
    //}

  type TreatmentSummary =
    | ComparativeInformation of Id * Summary


module TratmentSummaryParser =
  open TreatmentSummary

  let inline outputclass arg = ( ^a : (member Outputclass : string) arg)

  let inline (|HasOutputClass|_|) (n:string) x =
    let cs = (outputclass x).Split [|' '|]
    if (cs |> Array.exists (fun  c -> c = n)) then Some(x)
    else None


  type Summary with
    static member from (x:tsProvider.Topic) = Summary


  type TreatmentSummary with
    static member parse (x:tsProvider.Topic) =


      match x with
        | HasOutputClass "comparativeInformation" t -> ComparativeInformation(Id(x.Id), Summary.from t)
        | _ -> failwith ( "missed one" + x.Outputclass)
