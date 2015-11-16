namespace Bnf
open FSharp.Data
open Shared

module BorderlineSubstances =
  type bsProvider = XmlProvider<"borderlinesubstances.xml", Global=true>

  type Title =
    | Title of string
    override __.ToString() = match __ with | Title x -> x

  type Category = | Category of string

  type IntroductionNote = | IntroductionNote of string


  type Formulation = | Formulation of string

  type Details = | Details



  type BorderlineSubstancePrep = | BorderlineSubstancePrep

  type BorderlinSubstance = {
    title:Title;
    category:Category;
    intro:IntroductionNote;
    detals:Details;
    preparations:BorderlineSubstancePrep list;
    }
