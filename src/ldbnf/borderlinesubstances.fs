namespace Bnf
open FSharp.Data
open Shared


module BorderlineSubstance =

  type bsProvider = XmlProvider<"borderlinesubstances.xml", Global=true>

  type Title =
    | Title of bsProvider.Title

  type Link = {Uri:string;Label:string;}

  type Category =
    | Category of string
    override __.ToString() = match __ with | Category x -> x

  type IntroductionNote = | IntroductionNote of string


  [<Measure>] type Kj
  [<Measure>] type Kcal
  [<Measure>] type g

  type Detail =
    | Formulation of string
    | EnergyKj of int<Kj>
    | EnergyKcal of int<Kcal>
    | ProteinGrams of int<g>
    | ProteinConstituents of string
    | CarbohydrateGrams of int<g>
    | CarbohydrateConstituents of string
    | FatGrams of int<g>
    | FatConstituents of string
    | FibreGrams of int<g>
    | SpecialCharacteristics of string
    | Acbs of bsProvider.P
    | Presentation of string

  type Manufacturer = | Manufacturer of string

  type PreparationTitle = | PreparationTitle of string * Manufacturer option


  type PackSize = | PackSize of int

  type UnitOfMeasure = | UnitOfMeasure of string

  type PackAcbs = | PackAcbs of string

  type PackInfo = | PackInfo of PackSize * UnitOfMeasure * PackAcbs


  type NhsIndicative = | NhsIndicative of string

  type PriceText = | PriceText of string

  type NhsIndicativePrice = | NhsIndicativePrice of string

  type NhsIndicativeInfo = | NhsIndicativeInfo of NhsIndicative * PriceText * NhsIndicativePrice


  type PackSizePriceTariff = | PackSizePriceTariff of PackInfo * NhsIndicativeInfo


  type BorderlineSubstancePrep = | BorderlineSubstancePrep of Title * PackSizePriceTariff list

  type BorderlinSubstance = {
    id:Id;
    title:Title;
    category:Category;
    intro:IntroductionNote;
    details:Detail list;
    //preparations:BorderlineSubstancePrep list;
  }


module BorderlinSubstanceParser =
  open prelude
  open BorderlineSubstance

  let inline withoc n x =
    let oc = (^a : (member Outputclass : string) x)
    if (oc = n) then Some (x)
    else None

  let inline withoco n x =
    let oc = (^a : (member Outputclass : Option<string>) x)
    match oc with
      | Some s ->
        if (s = n) then Some (x) else None
      | _ -> None

  let inline (|HasOutputClass|_|) (n:string) x =
    let oc = (^a : (member Outputclass : string) x)
    if oc = n then Some(x)
    else None

  let inline (|HasOutputClasso|_|) (n:string) x =
    let oc = (^a : (member Outputclass : Option<string>) x)
    match oc with
      | Some s -> if s = n then Some(x)
                  else None
      | None -> None

  type IntroductionNote with
    static member from (x:bsProvider.P) =
      match x with
        | HasOutputClasso "introductionNote" _ ->
          x.String >>= (IntroductionNote >> Some)
        | _ -> None

  let unit<[<Measure>]'u> = int >> LanguagePrimitives.Int32WithMeasure<'u>

  //need to cope with nil as a value????

  type Detail with
    static member from (x:bsProvider.P) =
      match x with
        | HasOutputClasso "formulation" p -> p.String >>= (Formulation >> Some)
        | HasOutputClasso "energyKj" p -> p.Number >>= (unit<Kj> >> EnergyKj >> Some)
        | HasOutputClasso "energyKcal" p -> p.Number >>= (unit<Kcal> >> EnergyKcal >> Some)
        | HasOutputClasso "proteinGrams" p -> p.Number >>= (unit<g> >> ProteinGrams >> Some)
        | HasOutputClasso "proteinConstituents" p -> p.String >>= (ProteinConstituents >> Some)
        | HasOutputClasso "carbohydrateGrams" p -> p.Number >>= (unit<g> >> CarbohydrateGrams >> Some)
        | HasOutputClasso "carbohydrateConstituents" p -> p.String >>= (CarbohydrateConstituents >> Some)
        | HasOutputClasso "fatGrams" p -> p.Number >>= (unit<g> >> FatGrams >> Some)
        | HasOutputClasso "fatConstituents" p -> p.String >>= (FatConstituents >> Some)
        | HasOutputClasso "fibreGrams" p -> p.Number >>= (unit<g> >> FibreGrams >> Some)
        | HasOutputClasso "specialCharacteristics" p -> p.String >>= (SpecialCharacteristics >> Some)
        | HasOutputClasso "acbs" p -> p |> (Acbs >> Some)
        | HasOutputClasso "presentation" p -> p.String >>= (Presentation >> Some)
        | _ -> None
    static member from (x:bsProvider.Section) =
      match x with
        | HasOutputClass "details" s ->
          s.Ps |> Array.choose Detail.from |> Array.toList
        | _ -> []

  //type BorderlineSubstancePrep with
  //  static member 

  type BorderlinSubstance with
    static member parse (x:bsProvider.Topic) =
      let t = x.Title |> Title
      let c = x.Body.Data.Value |> Category
      let note = x.Body.Ps |> Array.pick IntroductionNote.from
      let d = x.Body.Section |> Detail.from

      {id = Id(x.Id)
       title = t;
       category = c;
       intro = note;
       details = d;}
