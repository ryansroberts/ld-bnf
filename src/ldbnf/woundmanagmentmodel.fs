namespace Bnf
open FSharp.Data

module WoundManagement =
  open Shared

  type wmProvider = XmlProvider<"superwoundmanagment.xml", Global=true, SampleIsList=true>

  type Title = | Title of string

  type General = | General of wmProvider.Section

  type TypeOfWound = | TypeOfWound of string

  type Description = | Description of wmProvider.Sectiondiv

  type WoundManagementLink = {uri:string;label:string;}

  type WoundExudate = | WoundExudate of string * WoundManagementLink list

  type WoundType = | WoundType of TypeOfWound * Description * WoundExudate list

  type DressingChoices = | DressingChoices of WoundType list

  type Product = {
    ampid: Int64;
    name: string;
    price: string;
    manufacturer: string;
  }

  type ProductGroup = | ProductGroup of Title * Description * Product list

  type WoundManagement = {
    title: Title;
    dressings: DressingChoices;
    links: WoundManagementLink list;
    products: Product list;
    productGroups: ProductGroup list;
  }
