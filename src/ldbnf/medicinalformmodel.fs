namespace Bnf
open FSharp.Data

module MedicinalForm =
  type mfProvider = XmlProvider<"supermedicinalform.xml", Global=true>

  type Title = | Title of string

  type LabelNumber = | LabelNumber of decimal

  type CautionaryAndAdvisoryLabelsTitle = | CautionaryAndAdvisoryLabelsTitle of string

  type CautionaryAdvisoryLabel = | CautionaryAdvisoryLabel of Option<LabelNumber> * mfProvider.P

  type CautionaryAdvisoryLabels = | CautionaryAdvisoryLabels of Option<CautionaryAndAdvisoryLabelsTitle> * CautionaryAdvisoryLabel []

  type Manufacturer = | Manufacturer of string

  type MedicinalProductTitle = | MedicinalProductTitle of Manufacturer * mfProvider.Title

  type Ampid = | Ampid of int

  type StrengthOfActiveIngredient = | StrengthOfActiveIngredient of mfProvider.P

  //To be extended in the future
  type UnitOfMeasure =
    | Tablet
    | Dunno

  type PackSize = | PackSize of decimal

  //I'm pretty sure that there will be more of these
  type LegalCategory =
    | POM
    | Dunno

  type PackInfo = | PackInfo of Option<PackSize> * Option<UnitOfMeasure> * Option<LegalCategory>

  type PriceText = | PriceText of string


  type NhsIndicative = | NhsIndicative of string

  type NhsIndicativePrice = | NhsIndicativePrice of decimal

  type NhsIndicativeInfo = | NhsIndicativeInfo of NhsIndicative * PriceText * NhsIndicativePrice


  type DrugTarrif = | DrugTarrif of string

  type DrugTariffPrice = | DrugTariffPrice of decimal

  type DrugTariffInfo = | DrugTariffInfo of DrugTarrif * PriceText * DrugTariffPrice



  type Pack = | Pack of PackInfo * NhsIndicativeInfo * DrugTariffInfo

  type MedicinalProduct = {
    title:MedicinalProductTitle;
    ampid:Ampid;
    strengthOfActiveIngredient:StrengthOfActiveIngredient;
    packs: Pack list}

  type MedicinalForm = { title : Option<Title>;
    cautionaryAdvisoryLabels : Option<CautionaryAdvisoryLabels>; }
    //medicinalProducts : MedicinalProduct list;
  //}

module MedicinalFormParser =
  open prelude
  open MedicinalForm

  let inline withoc n x =
    let oc = (^a : (member Outputclass : string) x)
    if (oc = n) then Some (x)
    else None

  type CautionaryAdvisoryLabel with
    static member from (x:mfProvider.P) =
      let ln = x.Phs.[0].Number >>= (LabelNumber >> Some)
      CautionaryAdvisoryLabel(ln,x)

  type CautionaryAdvisoryLabels with
    static member from (x:mfProvider.Section) =
      let ls = x.Ps |> Array.map CautionaryAdvisoryLabel.from
      let t = x.Title.Value >>= (CautionaryAndAdvisoryLabelsTitle >> Some)
      CautionaryAdvisoryLabels(t,ls)

  type PackSize with
    static member from (x:mfProvider.Ph) = 
      x.Number >>= (PackSize >> Some)

  type UnitOfMeasure with
    static member from (x:mfProvider.Ph) =
      match x.String with
        | Some(s) -> match s with
                     | "tablet" -> Tablet
                     | _ -> Dunno
        | None -> Dunno

  type LegalCategory with
    static member from (x:mfProvider.Ph) =
      match x.String with
        | Some(s) -> match s with
                     | "POM" -> POM
                     | _ -> Dunno
        | None -> Dunno

  type PackInfo with
    static member from (x:mfProvider.P) =
      let ps = x.Phs |> Array.tryPick (withoc "packSize") >>= PackSize.from
      let uom = x.Phs |> Array.tryPick (withoc "unitOfMeasure") >>= UnitOfMeasure.from
      let lc = x.Phs |> Array.tryPick (withoc "legalCategory") >>= LegalCategory.from
      PackInfo(ps,uom,lc)

  let parse (x:mfProvider.Topic) =
    let t = match x.Title.Value with
              | Some(v) -> Some(Title v)
              | None -> None
    let cals = x.Body.Sections
               |> Array.choose (withoc "cautionaryAndAdvisoryLabels")
               |> Array.map (CautionaryAdvisoryLabels.from >> Some)
               |> Array.tryPick id

    {title = t; cautionaryAdvisoryLabels = cals;}
