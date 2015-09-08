#I "../../packages/FSharp.Data/lib/net40/"
#I "../../packages/FParsec/lib/net40-client/"
#r "../../packages/FParsec/lib/net40-client/FParsec.dll"
#r "../../packages/FParsec/lib/net40-client/FParsecCS.dll"
#r "../../packages/FSharp.Data/lib/net40/FSharp.Data.dll"
#r "../../packages/Arachne.Core/lib/net45/Arachne.Core.dll"


#r "../../packages/Arachne.Uri/lib/net45/Arachne.Uri.dll"
#r "System.Xml.Linq.dll"
open FSharp.Data
open Arachne.Uri

type Html = | Html of string
type Content = | Content of Html //content as a string is dumb
type Id = | Id of string
type Link = {Title:string; Url:string}

type ReferenceableContent = | ReferenceableContent of Content * Id * string

type InteractionLink = | InteractionLink of Link

type InteractionLinks = | InteractionLinks of InteractionLink seq


type Classification =
  | DrugClassification of string 
  | InheritsFromClass of string 

type Classifications = | Classifications of Classification seq

type Vtmid = | Vtmid of int

type MedicinalFormLink = | MedicinalFormLink of Link

type TheraputicIndication = | TheraputicIndication of string

type PatientGroup = {Group:string; Dosage:string;}

type Route = | Route of string

type RouteOfAdministration = | RouteOfAdministration of Route * PatientGroup seq 

type IndicationsAndDose = | IndicationsAndDose  of TheraputicIndication seq * RouteOfAdministration seq

type GeneralInformation = | GeneralInformation of string seq

type AdditionalMonitoringInRenalImpairment = | AdditionalMonitoringInRenalImpairment of string seq

type PatientResources = | PatientResources of string seq

type MonographSection =
  | IndicationsAndDose of IndicationsAndDose
  | Pregnancy of Id * GeneralInformation
  | BreastFeeding of Id * GeneralInformation
  | HepaticImpairment of Id * GeneralInformation
  | RenalImpairment of Id * GeneralInformation * AdditionalMonitoringInRenalImpairment
  | PatientAndCarerAdvice of Id * PatientResources
  | MedicinalForms of ReferenceableContent * MedicinalFormLink seq

type Drug = | Drug of InteractionLinks *
                      Classifications *
                      Vtmid *
                      MonographSection seq

let inline name arg =
  ( ^a : (member Name : string) arg)

let inline value arg =
  ( ^a : (member Value : ^b) arg)

let inline outputclass arg =
  ( ^a : (member Outputclass : string) arg)

let inline (|HasName|_|) n x =
  if (name x) = n then Some(HasName)
  else None

let inline hasName s x = name x = s

let inline (|HasOutputClass|_|) n x =
  if (outputclass x) = n then Some(HasOutputClass)
  else None

type drugProvider = XmlProvider<"PHP1494.xml">

type InteractionLink with
  static member from (x:drugProvider.Xref) = InteractionLink {Url = x.Href ; Title = x.Value}

type MedicinalFormLink with
  static member from (x:drugProvider.Xref2) = MedicinalFormLink {Url = x.Href ; Title = x.Value}

type Content with
  static member from x = Content(Html(x.ToString()))

type ReferenceableContent with
  static member from (x:drugProvider.Topic2) = ReferenceableContent(Content.from x,Id(x.Id),x.Title)

type PatientGroup with
  static member from (x:drugProvider.Li) = {Group = x.Ps.[0]; Dosage = x.Ps.[1];}

type TheraputicIndication with
  static member from x = Array.map TheraputicIndication

let paragraphs (x:Option<drugProvider.Sectiondiv>)  =
  match x with
    | Some(x) -> x.Ps |> Array.map value |> Array.choose id
    | None -> Array.empty<string>

type IndicationsAndDose with
  static member from (x:drugProvider.Topic2) =
    let theraputicIndications = x.Body.Sections.[0].Sectiondiv |> paragraphs |> Array.map TheraputicIndication 
    let routes = x.Body.Sections.[0].Ps |> Array.map (value >> Route)
    let groups = x.Body.Sections.[0].Uls |> Array.map (fun u -> u.Lis |> Seq.map PatientGroup.from)
    let routesOfAdministration = Array.zip routes groups |> Array.map RouteOfAdministration
    IndicationsAndDose(theraputicIndications,routesOfAdministration)

    
let medicinalForms (x:drugProvider.Topic2) =
    let content = ReferenceableContent.from x
    let links = x.Xrefs |> Array.map MedicinalFormLink.from
    MedicinalForms(content,links)

let inline optn t f x = Some (t (f x))

let inline classFn x =
    match x with 
    | HasName "drugClassification" ->  optn DrugClassification name x 
    | HasName "inheritsFromClass" ->  optn InheritsFromClass name x
    | _ -> None

let generalInformation (c,(x:drugProvider.Topic2)) = 
    let gi = x.Body.Sections.[0].Sectiondiv |> (paragraphs >> Array.toSeq >> GeneralInformation)
    c(Id(x.Id),gi)

let drugXml = drugProvider.GetSample()

let interactionLinks = InteractionLinks(drugXml.Body.P.Xrefs |> Array.map InteractionLink.from)

let classifications = Classifications(drugXml.Body.Datas
                      |> Array.filter (hasName "classifications")
                      |> Array.collect (fun c -> c.Datas)
                      |> Array.collect (fun c -> c.Datas)
                      |> Array.map classFn
                      |> Array.choose id)

let vtmid =
  (drugXml.Body.Datas
   |> Array.filter ( hasName "vtmid" )
   |> Array.map (value >> Option.map Vtmid)
   |> Array.choose id).[0]

let inline sectionFn x =
    match x with
    | HasOutputClass "pregnancy" -> Some(generalInformation(MonographSection.Pregnancy,x))
    | HasOutputClass "breastFeeding" -> Some(generalInformation(MonographSection.BreastFeeding,x))
    | HasOutputClass "hepaticImpairment" -> Some(generalInformation(MonographSection.HepaticImpairment,x))
    | HasOutputClass "renalImpairment" -> Some(RenalImpairment(ReferenceableContent.from x))
    | HasOutputClass "patientAndCarerAdvice" -> Some(PatientAndCarerAdvice(ReferenceableContent.from x))
    | HasOutputClass "medicinalForms" -> Some(medicinalForms x)
    | HasOutputClass "indicationsAndDose" -> Some(IndicationsAndDose(IndicationsAndDose.from x))
    | _ -> None

let sections =
  drugXml.Topics |> Array.map sectionFn |> Array.choose id

let drug = Drug(interactionLinks,classifications,vtmid,sections)
