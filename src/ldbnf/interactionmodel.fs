namespace Bnf
open FSharp.Data
open Shared
open prelude

module Interaction =
  type inProvider = XmlProvider<"superinteraction.xml", Global=true>

  type Title =
    | Title of string
    override __.ToString() = match __ with | Title x -> x

  type Link = {url: string; label: string;}

  type Importance =
    | High
    | NotSet
    override __.ToString() = toString __

  type InteractsWith =
    {id:Id;
     title:Title;
     importance:Importance;
     message:inProvider.P;
     interactswith:Link;}

  type InteractionList =
    | InteractionList of Id * Title * InteractsWith list


module InteracitonParser =
  open Interaction

  type InteractsWith with
    static member from (x:inProvider.Topic) =
      let t = Title x.Title
      let p,l = match x.Body.P with
                | Some p ->
                  let ds = p.Phs |> Array.filter (fun p -> p.Outputclass = "drug")
                  let l = match ds.[1].Xref with
                          | Some x -> {url=x.Href;label=x.Value}
                          | None -> failwith "cant find the link"
                  p,l
                | None -> failwith "cant find paragraph"
      let i = match x.Importance with
              | Some "high" -> High
              | _ -> NotSet
      {id=Id(x.Id); title=t; importance = i;message = p; interactswith = l}

  type InteractionList with
    static member parse (x:inProvider.Topic) =
      let is = x.Topics |> Array.map InteractsWith.from |> Array.toList
      InteractionList(Id(x.Id),Title x.Title,is)

