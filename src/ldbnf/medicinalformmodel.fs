namespace Bnf
open FSharp.Data
open Shared

module MedicinalForm =
  type mfProvider = XmlProvider<"supermedicinalform.xml", Global=true>

  type Title = | Title of string

  type LabelNumber = | LabelNumber of decimal

  type CautionaryAndAdvisoryLabelsTitle = | CautionaryAndAdvisoryLabelsTitle of string

  type CautionaryAdvisoryLabel = | CautionaryAdvisoryLabel of Option<LabelNumber> * mfProvider.P

  type CautionaryAdvisoryLabels = | CautionaryAdvisoryLabels of Option<CautionaryAndAdvisoryLabelsTitle> * CautionaryAdvisoryLabel []

  type Manufacturer = | Manufacturer of string

  type BlackTriangle = | BlackTriangle of string

  type MedicinalProductTitle = | MedicinalProductTitle of Option<Manufacturer> * Option<BlackTriangle> * mfProvider.Title

  type Ampid = | Ampid of int64

  type StrengthOfActiveIngredient = | StrengthOfActiveIngredient of mfProvider.P

  //To be extended in the future
  type UnitOfMeasure =
    | Tablet

  type PackSize = | PackSize of decimal

  //I'm pretty sure that there will be more of these
  type LegalCategory =
    | POM

  type PackInfo = | PackInfo of Option<PackSize> * Option<UnitOfMeasure> * Option<LegalCategory>

  type PriceText = | PriceText of string


  type NhsIndicative = | NhsIndicative of string

  type NhsIndicativePrice = | NhsIndicativePrice of decimal

  type NhsIndicativeInfo = | NhsIndicativeInfo of Option<NhsIndicative> * Option<PriceText> * Option<NhsIndicativePrice>


  type DrugTarrif = | DrugTarrif of string

  type DrugTariffPrice = | DrugTariffPrice of string

  type DrugTariffInfo = | DrugTariffInfo of Option<DrugTarrif> * Option<PriceText> * Option<DrugTariffPrice>



  type Pack = | Pack of Option<PackInfo> * Option<NhsIndicativeInfo> * Option<DrugTariffInfo>

  type MedicinalProduct = {
    title:MedicinalProductTitle;
    ampid:Option<Ampid>;
    strengthOfActiveIngredient:Option<StrengthOfActiveIngredient>;
    packs: Pack list}

  type MedicinalForm = {
    id : Id;
    title : Option<Title>;
    cautionaryAdvisoryLabels : Option<CautionaryAdvisoryLabels>;
    medicinalProducts : MedicinalProduct list;
  }

module MedicinalFormParser =
  open prelude
  open MedicinalForm


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
    static member from (x:mfProvider.P) =
      let ln = x.Phs.[0].Number >>= (LabelNumber >> Some)
      CautionaryAdvisoryLabel(ln,x)

  type CautionaryAdvisoryLabels with
    static member from (x:mfProvider.Section) =
      let ls = x.Ps |> Array.map CautionaryAdvisoryLabel.from
      let t = x.Title.Value >>= (CautionaryAndAdvisoryLabelsTitle >> Some)
      CautionaryAdvisoryLabels(t,ls)

  let fromphn c (x:mfProvider.Ph) =
    x.Number >>= (c >> Some)

  let fromphs c (x:mfProvider.Ph) =
    x.String >>= (c >> Some)

  type UnitOfMeasure with
    static member from (x:mfProvider.Ph) =
      match x.String with
        | Some(s) -> match s with
                     | "tablet" -> Some(Tablet)
                     | _ -> None
        | None -> None

  type LegalCategory with
    static member from (x:mfProvider.Ph) =
      match x.String with
        | Some(s) -> match s with
                     | "POM" -> Some(POM)
                     | _ -> None
        | None -> None

  type PackInfo with
    static member from (x:mfProvider.P) =
      let ps = x.Phs |> Array.tryPick (withoc "packSize") >>= (fromphn PackSize)
      let uom = x.Phs |> Array.tryPick (withoc "unitOfMeasure") >>= UnitOfMeasure.from
      let lc = x.Phs |> Array.tryPick (withoc "legalCategory") >>= LegalCategory.from
      PackInfo(ps,uom,lc)

  type NhsIndicativeInfo with
    static member from (x:mfProvider.P) =
      let nhsi = x.Phs |> Array.tryPick (withoc "nhsIndicative") >>= (fromphs NhsIndicative)
      let pt = x.Phs |> Array.tryPick (withoc "priceText") >>= (fromphs PriceText)
      let nhsip = x.Phs |> Array.tryPick (withoc "nhsIndicativePrice") >>= (fromphn NhsIndicativePrice)
      NhsIndicativeInfo(nhsi,pt,nhsip)

  type DrugTariffInfo with
    static member from (x:mfProvider.P) =
      let dt = x.Phs |> Array.tryPick (withoc "drugTariff") >>= (fromphs DrugTarrif)
      let pt = x.Phs |> Array.tryPick (withoc "priceText") >>= (fromphs PriceText)
      let dtp = x.Phs |> Array.tryPick (withoc "drugTariffPrice") >>= (fromphs DrugTariffPrice)
      DrugTariffInfo(dt,pt,dtp)

  type MedicinalProductTitle with
    static member from (x:mfProvider.Title) =
      let m = x.Phs |> Array.tryPick (withoc "manufacturer") >>= (fromphs Manufacturer)
      let bt = x.Phs |> Array.tryPick (withoc "blackTriangle") >>= (fromphs BlackTriangle)
      MedicinalProductTitle(m,bt,x)

  type Ampid with
    static member from (x:mfProvider.Data) =
      Ampid(x.Value)

  type Pack with
    static member from (x:mfProvider.Ul) =
      x.Lis |> Array.map Pack.from |> Array.toList
    static member from (x:mfProvider.Li) =
      let pi = x.Ps |> Array.tryPick (withoco "packInfo") >>= (PackInfo.from >> Some)
      let nio = x.Ps |> Array.tryPick (withoco "nhsIndicativeInfo") >>= (NhsIndicativeInfo.from >> Some)
      let dti = x.Ps |> Array.tryPick (withoco "drugTariffInfo") >>= (DrugTariffInfo.from >> Some)
      Pack(pi,nio,dti)

  type MedicinalProduct with
    static member from (x:mfProvider.Section) =
      let t = MedicinalProductTitle.from x.Title
      let str = x.Ps |> Array.tryPick (withoco "strengthOfActiveIngredient") >>= (StrengthOfActiveIngredient >> Some)
      let ps = match x.Ul with
               | Some x -> Pack.from x
               | _ -> List.empty<Pack>
      let a = x.Data >>= (Ampid.from >> Some)
      {title = t; ampid = a; strengthOfActiveIngredient = str; packs = ps;}

  type MedicinalForm with
    static member parse (x:mfProvider.Topic) =
      let t = match x.Title.Value with
              | Some(v) -> Some(Title v)
              | None -> None
      let cals = x.Body.Sections
               |> Array.choose (withoc "cautionaryAndAdvisoryLabels")
               |> Array.map (CautionaryAdvisoryLabels.from >> Some)
               |> Array.tryPick id
      let mps = x.Body.Sections
               |> Array.choose (withoc "medicinalProduct")
               |> Array.map (MedicinalProduct.from)
               |> Array.toList

      {id = Id(x.Id); title = t; cautionaryAdvisoryLabels = cals; medicinalProducts = mps}
