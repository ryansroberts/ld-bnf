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

  type WoundType = | WoundType of TypeOfWound * Description option * WoundExudate list

  type Product = {
    ampid: int64;
    name: string;
    price: string;
    manufacturer: string;
  }

  type ProductGroup = | ProductGroup of Title * Description option * Product list

  type WoundManagement = {
    title: Title;
    general: General option;
    dressingChoices: WoundType list;
    links: WoundManagementLink list;
    //products: Product list; //need to check if they exist in isolation
    productGroups: ProductGroup list;
  }

module WoundManagementParser =
  open WoundManagement
  open prelude

  let inline withoc n x =
    let oc =  ( ^a : (member Outputclass : string) x)
    if n = oc then Some(x)
    else None

  let inline withoco n x =
    let oc =  ( ^a : (member Outputclass : Option<string>) x)
    match oc with
    | Some s -> if n = s then Some(x)
                else None
    | _ -> None

  let inline (|HasOutputClass|_|) n x = withoc n x
  let inline (|HasOutputClasso|_|) n x = withoco n x

  let desc = Array.tryPick (withoco "description" >> Option.map Description)

  type Title with
    static member from (x:wmProvider.P) = x.Value >>= (Title >> Some)

  type WoundManagementLink with
    static member from (x:wmProvider.Xref) =
      {uri=x.Href;label=x.Value}

  type WoundExudate with
    static member list (x:wmProvider.Sectiondiv[]) =
      x |> Array.choose (withoco "woundExudate" >> Option.map WoundExudate.from) |> Array.toList
    static member from (x:wmProvider.Sectiondiv) =
      let r = x.Ps.[0].Value.Value
      let ls = x.Xrefs |> Array.map WoundManagementLink.from |> Array.toList
      WoundExudate(r,ls)

  type TypeOfWound with
    static member from (x:wmProvider.P) = x.Value >>= (TypeOfWound >> Some)

  type WoundType with
    static member list (x:wmProvider.Section) =
      x.Sectiondivs |> Array.choose (withoco "woundType" >> Option.map WoundType.from)
    static member from (x:wmProvider.Sectiondiv) =
      let tow = x.Ps |> Array.pick (withoco "typeOfWound" >> Option.bind TypeOfWound.from)
      let d = x.Sectiondivs |> desc
      let wes = x.Sectiondivs |> WoundExudate.list
      WoundType(tow,d,wes)

  type Product with
    static member from (x:wmProvider.P) =
      let n = x.Phs |> Array.tryPick (withoc "name")
      let p = x.Phs |> Array.tryPick (withoc "price")
      let m = x.Phs |> Array.tryPick (withoc "manufacturer")
      match (x.Data,n,p,m) with
        | (Some d, Some name, Some price, Some man) ->
           {ampid=d.Value;name=name.String.Value;price=price.String.Value;manufacturer=man.String.Value}
        | _ -> failwith "missing part of the product"

    static member from (x:wmProvider.Sectiondiv) =
      let p = x.Ps |> Array.tryPick (withoco "product")
      match p with
        | Some p -> p |> Product.from |> Some
        | None -> None

    static member list (x:wmProvider.Sectiondiv[]) =
      x |> Array.choose Product.from |> Array.toList

  type ProductGroup with
    static member list (x:wmProvider.Section) =
      x.Sectiondivs |> Array.choose (withoco "productGroup" >> Option.map ProductGroup.from)
    static member from (x:wmProvider.Sectiondiv) =
      let t = x.Ps |> Array.pick (withoco "title" >> Option.bind Title.from)
      let d = x.Sectiondivs |> desc
      let ps = x.Sectiondivs |> Product.list
      ProductGroup(t,d,ps)

  type WoundManagement with
    static member parse (x:wmProvider.Topic) =
      let t = Title x.Title
      let gen = x.Body.Sections |> Array.tryPick (withoc "general" >> Option.map General)
      let dcs = x.Body.Sections
                |> Array.choose (withoc "dressingChoices")
                |> Array.collect WoundType.list
                |> Array.toList
      let ls = x.Xrefs |> Array.map WoundManagementLink.from |> Array.toList
      let pgs = x.Body.Sections
                |> Array.choose (withoc "productGroups")
                |> Array.collect ProductGroup.list
                |> Array.toList
      {title=t;general=gen;dressingChoices=dcs;links=ls;productGroups=pgs}
