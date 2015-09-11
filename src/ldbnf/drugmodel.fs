namespace Bnf
open FSharp.Data

module Drug =
    type Paragraph = | Paragraph of string
    type Paragraphs = | Paragraphs of Paragraph seq 
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

    type LicensingVariationStatement = | LicensingVariationStatement of string seq

    type PatientResources = | PatientResources of string seq

    type MonographSection =
    | IndicationsAndDoseGroup of IndicationsAndDose seq
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

module DrugParser =
    open Drug

    let (|?) = defaultArg

    let inline name arg =
      ( ^a : (member Name : string) arg)

    let inline value arg =
      ( ^a : (member Value : ^b) arg)

    type OutputClass = | OutputClass of Option<string> with
       static member lift (x:Option<string>) = OutputClass(x)
       static member lift (x:string) = OutputClass(Some(x))
       

    let inline outputclasso  arg = 
       OutputClass.lift(( ^a : (member Outputclass : Option<string>) arg))

    let inline outputclass arg =
       OutputClass.lift(( ^a : (member Outputclass : string) arg))

    let inline (|HasName|_|) n x =
        if (name x) = n then Some(HasName)
        else None

    let inline hasName s x = name x = s

    let inline (|HasOutputClass|_|) n x =
        if (outputclass x) = n then Some(HasOutputClass)
        else None

    let inline hasOutputclass s x = outputclass x = s

    type drugProvider = XmlProvider<"SuperDrug.xml">

    type Paragraphs with
        static member from (x:Option<drugProvider.Sectiondiv>) =
          match x with
            | Some(x) -> Paragraphs.from x
            | None -> Paragraphs Array.empty<Paragraph>
        static member from (x:drugProvider.Sectiondiv) =
          x.Ps |> Array.map value |> Array.choose id |> Seq.map Paragraph |> Paragraphs
        static member from (x:drugProvider.Section) =
          x.Ps |> Array.map value |> Array.choose id |> Seq.map Paragraph |> Paragraphs
          
    type Route with       
      static member from (Paragraph x) = Route x
      static member from (Paragraphs xs) = Seq.map Route.from xs
 
    type InteractionLink with
        static member from (x:drugProvider.Xref) = InteractionLink {Url = x.Href ; Title = x.Value}

    type MedicinalFormLink with
        static member from (x:drugProvider.Xref6) = MedicinalFormLink {Url = x.Href ; Title = x.Value}

    type Content with
        static member from x = Content(Html(x.ToString()))

    type ReferenceableContent with
        static member from (x:drugProvider.Topic2) = ReferenceableContent(Content.from x,Id(x.Id),x.Title)

    type PatientGroup with
        static member from (x:drugProvider.Li) = {Group = x.Ps.[0].Value |? ""; Dosage = x.Ps.[1].Value |? "";}

    type TheraputicIndication with
      static member from (Paragraph x) = TheraputicIndication x
      static member from (Paragraphs xs) = Seq.map TheraputicIndication.from xs


    let sections (n:string) (x:drugProvider.Topic2) = x.Body.Sections |> Array.filter (fun s -> outputclasso s = OutputClass.lift n)


    type IndicationsAndDose with
      static member from (x:drugProvider.Section) =
            let theraputicIndications = x.Sectiondivs.[0] |> ( Paragraphs.from >> TheraputicIndication.from )
            let routes = x |> (Paragraphs.from >> Route.from >> Seq.toArray)
            let groups = x.Uls |> Array.map (fun u -> u.Lis |> Seq.map PatientGroup.from)
            let routesOfAdministration = Array.zip routes groups |> Array.map RouteOfAdministration
            IndicationsAndDose.IndicationsAndDose(theraputicIndications,routesOfAdministration)

    let indicationGroups (x:drugProvider.Topic2) =
        sections "indicationAndDoseGroup" x |> Array.map IndicationsAndDose.from


    let medicinalForms (x:drugProvider.Topic2) =
        let content = ReferenceableContent.from x
        let links = x.Xrefs |> Array.map MedicinalFormLink.from
        MedicinalForms(content,links)

    let inline optn t f x = Some (t (f x))

    let inline opt t f x =
        let y = f x
        match y with
          | Some(y) -> Some (t (y))
          | None -> None
        
    let inline classFn x =
        match x with 
        | HasName "drugClassification" ->  opt DrugClassification value x
        | HasName "inheritsFromClass" ->  opt InheritsFromClass value x
        | _ -> None

    let generalInformation c (x:drugProvider.Topic2) = 
        let gi = sections "generalInformation" x |> Array.map (Paragraphs.from >> GeneralInformation)
        c(Id(x.Id),gi)

    let sectionParagraphs c (x:drugProvider.Section []) =
        x |> Array.filter (hasOutputclass c) |> Array.map (fun s -> s.Sectiondiv) |> Seq.collect paragraphs

    let renalImpairment (x:drugProvider.Topic2) =
        let gi = sectionParagraphs "generalInformation" x.Body.Sections
                |> GeneralInformation
        let am = sectionParagraphs "additionalMonitoringInRenalImpairment" x.Body.Sections
                |> AdditionalMonitoringInRenalImpairment
        RenalImpairment(Id(x.Id),gi,am)

    let patientAndCarerAdvice (x:drugProvider.Topic2) =
        let r = sectionParagraphs "patientResources" x.Body.Sections
                |> PatientResources
        PatientAndCarerAdvice(Id(x.Id),r)
    
    let parse (x:drugProvider.Topic) =
        let interactionLinks = InteractionLinks(x.Body.P.Xrefs |> Array.map InteractionLink.from)

        let classifications = Classifications(x.Body.Datas
                            |> Array.filter (hasName "classifications")
                            |> Array.collect (fun c -> c.Datas)
                            |> Array.collect (fun c -> c.Datas)
                            |> Array.map classFn
                            |> Array.choose id)

        let vtmid =
            (x.Body.Datas
            |> Array.filter ( hasName "vtmid" )
            |> Array.map (value >> Option.map Vtmid)
            |> Array.choose id).[0]

        let inline sectionFn x =
            match x with
                | HasOutputClass "indicationsAndDose" -> Some(IndicationsAndDose (IndicationsAndDose.from x))
                | HasOutputClass "pregnancy" -> Some(generalInformation MonographSection.Pregnancy x)
                | HasOutputClass "breastFeeding" -> Some(generalInformation MonographSection.BreastFeeding x)
                | HasOutputClass "hepaticImpairment" -> Some(generalInformation MonographSection.HepaticImpairment x)
                | HasOutputClass "renalImpairment" -> Some(renalImpairment x)
                | HasOutputClass "patientAndCarerAdvice" -> Some(patientAndCarerAdvice x)
                | HasOutputClass "medicinalForms" -> Some(medicinalForms x)
                | _ -> None

        let sections =
            x.Topics |> Array.map sectionFn |> Array.choose id

        Drug(interactionLinks,classifications,vtmid,sections)
 
