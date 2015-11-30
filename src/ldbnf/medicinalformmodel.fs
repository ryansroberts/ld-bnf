namespace Bnf
open FSharp.Data
open prelude

module MedicinalForm =
  open Shared

  type Title =
    | Title of string
    override __.ToString() = match __ with | Title x -> x

  type LabelNumber =
    | LabelNumber of decimal
    override __.ToString() = match __ with | LabelNumber x -> string x

  type CautionaryAndAdvisoryLabelsTitle =
    | CautionaryAndAdvisoryLabelsTitle of string
    override __.ToString() = match __ with | CautionaryAndAdvisoryLabelsTitle x -> x

  type CautionaryAdvisoryLabel = | CautionaryAdvisoryLabel of Option<LabelNumber> * drugProvider.P

  type CautionaryAdvisoryLabels = | CautionaryAdvisoryLabels of Option<CautionaryAndAdvisoryLabelsTitle> * CautionaryAdvisoryLabel []

  type Excipients = | Excipients of drugProvider.Section

  type Electrolytes = | Electrolytes of drugProvider.Section

  type Manufacturer = | Manufacturer of string

  type BlackTriangle = | BlackTriangle of string

  type MedicinalProductTitle = | MedicinalProductTitle of Option<Manufacturer> * Option<BlackTriangle> * drugProvider.Title

  type Ampid =
    | Ampid of int64
    override __.ToString() = match __ with | Ampid x -> string x

  type StrengthOfActiveIngredient = | StrengthOfActiveIngredient of drugProvider.P

  //To be extended in the future
  type UnitOfMeasure =
    | UnitOfMeasure of string
    override __.ToString() = match __ with | UnitOfMeasure x -> x

  type PackSize = | PackSize of decimal

  //I'm pretty sure that there will be more of these
  type LegalCategory =
    | POM
    | P
    | GSL
    override __.ToString() = toString __

  type PackInfo = | PackInfo of Option<PackSize> * Option<UnitOfMeasure> * Option<LegalCategory>

  type PriceText = | PriceText of string

  type NhsIndicative = | NhsIndicative of string

  type NhsIndicativePrice = | NhsIndicativePrice of decimal

  type NhsIndicativeInfo = | NhsIndicativeInfo of Option<NhsIndicative> * Option<PriceText> * Option<NhsIndicativePrice>


  type DrugTarrif = | DrugTarrif of string

  type DrugTariffPrice = | DrugTariffPrice of decimal

  type DrugTariffInfo = | DrugTariffInfo of Option<DrugTarrif> * Option<PriceText> * Option<DrugTariffPrice>

  type ControlledDrug = | ControlledDrug of drugProvider.P

  type Pack = | Pack of Option<PackInfo> * Option<NhsIndicativeInfo> * Option<DrugTariffInfo>

  type MedicinalProduct = {
    title:MedicinalProductTitle;
    ampid:Ampid;
    strengthOfActiveIngredient: StrengthOfActiveIngredient list;
    controlledDrugs: ControlledDrug list;
    packs: Pack list}

  type MedicinalForm = {
    id : Id;
    title : Title option;
    excipients : Excipients option;
    electrolytes : Electrolytes option;
    cautionaryAdvisoryLabels : CautionaryAdvisoryLabels option;
    medicinalProducts : MedicinalProduct list;
  }

module MedicinalFormParser =
  open prelude
  open MedicinalForm
  open Shared


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

  type CautionaryAdvisoryLabel with
    static member from (x:drugProvider.P) =
      let ln = x.Phs.[0].Number >>= (LabelNumber >> Some)
      CautionaryAdvisoryLabel(ln,x)

  type CautionaryAdvisoryLabels with
    static member from (x:drugProvider.Section) =
      let ls = x.Ps |> Array.map CautionaryAdvisoryLabel.from
      let t = match x.Title with
                | Some t -> t.Value >>= (CautionaryAndAdvisoryLabelsTitle >> Some)
                | None -> failwith "CautionaryAdvisoryLabels must have a Title"
      CautionaryAdvisoryLabels(t,ls)

  let fromphn c (x:drugProvider.Ph) =
    x.Number >>= (c >> Some)

  let fromphs c (x:drugProvider.Ph) =
    x.String >>= (c >> Some)

  type UnitOfMeasure with
    static member from (x:drugProvider.Ph) =
      match x.String with
        | Some(s) -> UnitOfMeasure s |> Some
        | None -> None

  type LegalCategory with
    static member from (x:drugProvider.Ph) =
      match x.String with
        | Some(s) -> match s with
                     | "POM" -> Some(POM)
                     | "P" -> Some(P)
                     | "GSL" -> Some(GSL)
                     | _ ->
                       printfn "Unknown LegalCatgory %s" s
                       None
        | None -> None

  type PackInfo with
    static member from (x:drugProvider.P) =
      let ps = x.Phs |> Array.tryPick (withoc "packSize") >>= (fromphn PackSize)
      let uom = x.Phs |> Array.tryPick (withoc "unitOfMeasure") >>= UnitOfMeasure.from
      let lc = x.Phs |> Array.tryPick (withoc "legalCategory") >>= LegalCategory.from
      PackInfo(ps,uom,lc)

  type NhsIndicativeInfo with
    static member from (x:drugProvider.P) =
      let nhsi = x.Phs |> Array.tryPick (withoc "nhsIndicative") >>= (fromphs NhsIndicative)
      let pt = x.Phs |> Array.tryPick (withoc "priceText") >>= (fromphs PriceText)
      let nhsip = x.Phs |> Array.tryPick (withoc "nhsIndicativePrice") >>= (fromphn NhsIndicativePrice)
      NhsIndicativeInfo(nhsi,pt,nhsip)

  type DrugTariffInfo with
    static member from (x:drugProvider.P) =
      let dt = x.Phs |> Array.tryPick (withoc "drugTariff") >>= (fromphs DrugTarrif)
      let pt = x.Phs |> Array.tryPick (withoc "priceText") >>= (fromphs PriceText)
      let dtp = x.Phs |> Array.tryPick (withoc "drugTariffPrice") >>= (fromphn DrugTariffPrice)
      DrugTariffInfo(dt,pt,dtp)

  type MedicinalProductTitle with
    static member from (x:drugProvider.Title) =
      let mph = x.Phs |> Array.tryPick (withoc "manufacturer")
      let btph = x.Phs |> Array.tryPick (withoc "blackTriangle")
      if (mph.IsSome) then mph.Value.XElement.Remove() //edit the xml
      if (btph.IsSome) then btph.Value.XElement.Remove()

      let m =  mph >>= (fromphs Manufacturer)
      let bt = btph >>= (fromphs BlackTriangle)

      MedicinalProductTitle(m,bt,x)

  type Ampid with
    static member from (x:drugProvider.Data) =
      Ampid(x.Number.Value)

  type Pack with
    static member from (x:drugProvider.Ul) =
      x.Lis |> Array.map Pack.from
    static member from (x:drugProvider.Li) =
      let pi = x.Ps |> Array.tryPick (withoco "packInfo") >>= (PackInfo.from >> Some)
      let nio = x.Ps |> Array.tryPick (withoco "nhsIndicativeInfo") >>= (NhsIndicativeInfo.from >> Some)
      let dti = x.Ps |> Array.tryPick (withoco "drugTariffInfo") >>= (DrugTariffInfo.from >> Some)
      Pack(pi,nio,dti)

  type MedicinalProduct with
    static member from (x:drugProvider.Section) =
      let t = match x.Title with
               | Some t -> MedicinalProductTitle.from t
               | None -> failwith "MedicinalProduct must have a Title"
      let str = x.Ps |> Array.choose (withoco "strengthOfActiveIngredient")
                     |> Array.map StrengthOfActiveIngredient
                     |> Array.toList
      let con = x.Ps |> Array.choose (withoco "controlledDrugs")
                     |> Array.map ControlledDrug
                     |> Array.toList
      let ps = x.Uls |> Array.map Pack.from |> Array.collect id |> Array.toList
      let a = match x.Data with
              | Some d -> Ampid.from d
              | None -> failwith "MedicinalProduct must have an Ampid"
      {title = t; ampid = a; strengthOfActiveIngredient = str; packs = ps; controlledDrugs = con;}

  type MedicinalForm with
    static member parse (x:drugProvider.Topic) =
      let sections (x:drugProvider.Topic) =
        match x.Body with
        | Some (b) -> b.Sections
        | _ -> failwith "body is required"

      let t = match x.Title.Value with
              | Some(v) -> Some(Title v)
              | None -> None
      let cals = x |> sections
               |> Array.choose (withoco "cautionaryAndAdvisoryLabels")
               |> Array.map (CautionaryAdvisoryLabels.from >> Some)
               |> Array.tryPick id
      let mps = x |> sections
               |> Array.choose (withoco "medicinalProduct")
               |> Array.map MedicinalProduct.from
               |> Array.toList
      let ex = x |> sections
               |> Array.choose (withoco "excipients")
               |> Array.map (Excipients >> Some)
               |> Array.tryPick id
      let el = x |> sections
               |> Array.choose (withoco "electrolytes")
               |> Array.map (Electrolytes >> Some)
               |> Array.tryPick id
      {id = Id(x.Id); title = t; excipients=ex; electrolytes=el; cautionaryAdvisoryLabels = cals; medicinalProducts = mps}
