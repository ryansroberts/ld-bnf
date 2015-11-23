namespace Bnf
open FSharp.Data
open Shared


module BorderlineSubstance =

  type bsProvider = XmlProvider<"borderlinesubstances.xml", Global=true, SampleIsList=true>

  type Title =
    | Title of bsProvider.Title
    override __.ToString() = match __ with | Title x -> string x

  type Link = {Uri:string;Label:string;}

  type Category =
    | Category of string
    override __.ToString() = match __ with | Category x -> x

  type IntroductionNote =
    | IntroductionNote of string
    override __.ToString() = match __ with | IntroductionNote x -> x


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

  type PreparationTitle = | PreparationTitle of bsProvider.P * Manufacturer option


  type PackSize = | PackSize of decimal

  type UnitOfMeasure = | UnitOfMeasure of string

  type PackAcbs = | PackAcbs of string

  type PackInfo = | PackInfo of PackSize option * UnitOfMeasure option * PackAcbs option


  type NhsIndicative = | NhsIndicative of string

  type PriceText = | PriceText of string

  type NhsIndicativePrice = | NhsIndicativePrice of string

  type NhsIndicativeInfo = | NhsIndicativeInfo of NhsIndicative option * PriceText option * NhsIndicativePrice option


  type PackSizePriceTariff = | PackSizePriceTariff of PackInfo option * NhsIndicativeInfo option


  type BorderlineSubstancePrep = | BorderlineSubstancePrep of PreparationTitle option * PackSizePriceTariff list

  type Details = | Details of Detail list * BorderlineSubstancePrep list

  type BorderlineSubstance = {
    id:Id;
    title:Title;
    category:Category;
    intro:IntroductionNote option;
    details:Details list;
  }


module BorderlineSubstanceParser =
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

  type Manufacturer with
    static member from (x:bsProvider.Ph) =
      match x with
        | HasOutputClass "manufacturer" ph -> ph.String >>= (Manufacturer >> Some)
        | _ -> None

  type PreparationTitle with
    static member from (x:bsProvider.P) =
      let m (p:bsProvider.P) = p.Phs |> Array.tryPick Manufacturer.from
      match x with
        | HasOutputClasso "title" p -> PreparationTitle(p, m p) |> Some
        | _ -> None

  let fromphn c (x:bsProvider.Ph) =
    x.Number >>= (c >> Some)

  let fromphs c (x:bsProvider.Ph) =
    x.String >>= (c >> Some)

  type UnitOfMeasure with
    static member from (x:bsProvider.Ph) =
      match x.String with
        | Some(s) -> UnitOfMeasure s |> Some
        | None -> None

 

  type PackInfo with
    static member from (x:bsProvider.P) =
      let ps = x.Phs |> Array.tryPick (withoc "packSize") >>= (fromphn PackSize)
      let uom = x.Phs |> Array.tryPick (withoc "unitOfMeasure") >>= UnitOfMeasure.from
      let acbs = x.Phs |> Array.tryPick (withoc "acbs") >>= (fromphs PackAcbs)
      PackInfo(ps,uom,acbs)

  type NhsIndicativeInfo with
    static member from (x:bsProvider.P) =
      let nhsi = x.Phs |> Array.tryPick (withoc "nhsIndicative") >>= (fromphs NhsIndicative)
      let pt = x.Phs |> Array.tryPick (withoc "priceText") >>= (fromphs PriceText)
      let nhsip = x.Phs |> Array.tryPick (withoc "nhsIndicativePrice") >>= (fromphs NhsIndicativePrice)
      NhsIndicativeInfo(nhsi,pt,nhsip)

  type PackSizePriceTariff with
    static member from (x:bsProvider.Li) =
      let pi = x.Ps |> Array.tryPick (withoco "packInfo") >>= (PackInfo.from >> Some)
      let nhs = x.Ps |> Array.tryPick (withoco "nhsIndicativeInfo") >>= (NhsIndicativeInfo.from >> Some)
      PackSizePriceTariff(pi,nhs)

  type BorderlineSubstancePrep with
    static member from (x:bsProvider.Sectiondiv) =
      let title = x.P >>= PreparationTitle.from
      let pt = match x.Ul with
                | Some ul -> ul.Lis |> Array.map PackSizePriceTariff.from |> Array.toList
                | None -> []
      BorderlineSubstancePrep(title,pt)

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
      let ds = match x with
               | HasOutputClass "details" s ->
                 s.Ps |> Array.choose Detail.from |> Array.toList
                 | _ -> []
      let bsps = match x.Sectiondiv with
                  | Some sd ->  sd.Sectiondivs |> Array.map BorderlineSubstancePrep.from |> Array.toList
                  | None -> []
      Details(ds,bsps)


  type BorderlineSubstance with
    static member parse (x:bsProvider.Topic) =
      let t = x.Title |> Title
      let c = x.Body.Data.Value |> Category
      let note = x.Body.Ps |> Array.tryPick IntroductionNote.from
      let ds = x.Body.Sections |> Array.map Detail.from |> Array.toList

      {id = Id(x.Id)
       title = t;
       category = c;
       intro = note;
       details = ds;
       }
