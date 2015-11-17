namespace Bnf
open FSharp.Data
open Shared


module BorderlineSubstances =

  type bsProvider = XmlProvider<"borderlinesubstances.xml", Global=true>

  type Title =
    | Title of string
    override __.ToString() = match __ with | Title x -> x

  type Link = {Uri:string;Label:string;}

  type Category = | Category of string

  type IntroductionNote = | IntroductionNote of string

  type Formulation = | Formulation of string

  [<Measure>] type Kj
  [<Measure>] type Kcal
  [<Measure>] type g

  type EnergyKj = | EnergyKj of int<Kj>

  type EnergyKcal = | EnergyKcal of int<Kcal>

  type ProteinGrams = | ProteinGrams of int<g>

  type ProteinConstituents = | ProteinConstituents of string

  type CarbohydrateGrams = | CarbohydrateGrams of int<g>

  type CarbohydrateConstituents = | CarbohydrateConstituents of string

  type FatGrams = | FatGrams of int<g>

  type FatConstituents = | FatConstituents of string

  type FibreGrams = | FibreGrams of int<g>

  type SpecialCharacteristics = | SpecialCharacteristics of string

  type Acbs = | Acbs of Link

  type Presentation = | Presentation of string

  type Details = {
    formulation: Formulation option;
    energyKj: EnergyKj option;
    energyKcal: EnergyKcal option;
    proteinGrams: ProteinGrams option;
    proteinConstituents: ProteinConstituents option;
    carbohydrateGrams: CarbohydrateGrams option;
    carbohydrateConstituents: CarbohydrateConstituents option;
    fatGrams: FatGrams option;
    fatConstituents: FatConstituents option;
    fibreGrams: FibreGrams option;
    specialCharacteristics: SpecialCharacteristics option;
    acbs: Acbs option;
    presentation: Presentation option;
  }

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
    title:Title;
    category:Category;
    intro:IntroductionNote;
    detals:Details;
    preparations:BorderlineSubstancePrep list;
  }
