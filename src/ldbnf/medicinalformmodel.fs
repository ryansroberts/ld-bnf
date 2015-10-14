namespace Bnf
open FSharp.Data

module MedcinalForm =
  type mfProvider = XmlProvider<"supermedicinalform.xml", Global=true>

  type Title = | Title of string

  type LabelNumber = | LabelNumber of int

  type CautionaryAndAdvisoryLabelsTitle = | CautionaryAndAdvisoryLabelsTitle of string

  type CautionaryAdvisoryLabel = | CautionaryAdvisoryLabel of LabelNumber * mfProvider.P

  type CautionaryAdvisoryLabels = | CautionaryAdvisoryLabels of CautionaryAndAdvisoryLabelsTitle * CautionaryAdvisoryLabel list

  type Manufacturer = | Manufacturer of string

  type MedicinalProductTitle = | MedicinalProductTitle of Manufacturer * mfProvider.Title

  type Ampid = | Ampid of int

  type StrengthOfActiveIngredient = | StrengthOfActiveIngredient of mfProvider.P

  type UnitOfMeasure = | Tablet

  type PackSize = | PackSize of int * UnitOfMeasure

  type LegalCategory = | POM

  type PackInfo = | PackInfo of PackSize * LegalCategory

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

  type MedicinalForm = {
    title : Title;
    cautionaryAdvisoryLabels : CautionaryAdvisoryLabels;
    medicinalProduct : MedicinalProduct; }
