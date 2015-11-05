namespace Bnf
open FSharp.Data
open Shared

module TreatmentSummary =
  type tsProvider = XmlProvider<"supertreatmentsummary.xml", Global=true>

  type Title = | Title of string
  type TargetAudience = | TargetAudience of string
  type Content = | Content of tsProvider.Section * TargetAudience
  type Doi = | Doi of string
  type BodySystem = | BodySystem of string

  type Summary = {
    id:Id;
    title:Title;
    doi:Doi;
//    bodySystem:BodySystem;
  //contents:Content seq;
  }

  type TreatmentSummary =
    | ComparativeInformation of Summary


module TreatmentSummaryParser =
  open TreatmentSummary

  type Name = | Name of string

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

  type Summary with
    static member from (x:tsProvider.Topic) =
      let t = Title(x.Title)
      let d = x.Body.Datas |> Array.choose (withname "doi") |> Array.pick Doi.from
 //     let bs = BodySystem

      {id = Id(x.Id); title = t; doi = d}

  type TreatmentSummary with
    static member parse (x:tsProvider.Topic) =
      match x with
        | HasOutputClass "comparativeInformation" t -> Summary.from t |> ComparativeInformation
        | _ -> failwith ( "missed one" + x.Outputclass)
