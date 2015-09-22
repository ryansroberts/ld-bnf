namespace Bnf
open FSharp.Data

module Drug =
    //sensible compromise to reference the types provided to avoid replication
    type drugProvider = XmlProvider<"SuperDrug.xml", Global=true>

    type Paragraph = | Paragraph of string
    type Paragraphs = | Paragraphs of Paragraph seq
    type Title = | Title of Paragraph
    type Html = | Html of string
    type Content = | Content of Html
    type Id = | Id of string
    type Link = {Title:string; Url:string}

    type ReferenceableContent = | ReferenceableContent of Content * Id * string

    type InteractionLink = | InteractionLink of Link

    type InheritsFromClass = | InheritsFromClass of Link

    type Classification = | Classification of Link * InheritsFromClass seq


    type Vtmid = | Vtmid of int

    type MedicinalFormLink = | MedicinalFormLink of Link

    type TheraputicIndication = | TheraputicIndication of string

    type PatientGroup = {Group:string; Dosage:string;}

    type Route = | Route of string
    type Indication = | Indication of string

    type RouteOfAdministration = | RouteOfAdministration of Route * PatientGroup seq 

    type IndicationsAndDose = | IndicationsAndDose  of TheraputicIndication seq * RouteOfAdministration seq

    type Specificity = | Specificity of Paragraph * Option<Route> * Option<Indication>

    type GeneralInformation = | GeneralInformation of drugProvider.Sectiondiv * Option<Specificity>

    type DoseAdjustment = | DoseAdjustment of drugProvider.Sectiondiv * Option<Specificity>

    type AdditionalMonitoringInRenalImpairment = | AdditionalMonitoringInRenalImpairment of Paragraphs

    type LicensingVariationStatement = | LicensingVariationStatement of Paragraphs

    type PatientResources = | PatientResources of Paragraphs

    type AdviceAroundMissedDoses = | AdviceAroundMissedDoses of drugProvider.Sectiondiv

    type GeneralPatientAdvice = | GeneralPatientAdvice of drugProvider.Sectiondiv * Option<Title> * Option<Specificity> 


    type TheraputicUse = | TheraputicUse of string * Option<TheraputicUse>

    type PrimaryTheraputicUse = | PrimaryTheraputicUse of Option<TheraputicUse>

    type SecondaryTheraputicUses = | SecondaryTheraputicUses of Option<TheraputicUse>

    type DomainOfEffect = | DomainOfEffect of string * Option<PrimaryTheraputicUse> * Option<SecondaryTheraputicUses>

    type PrimaryDomainOfEffect = | PrimaryDomainOfEffect of Option<DomainOfEffect>

    type SecondaryDomainsOfEffect = | SecondaryDomainsOfEffect of DomainOfEffect seq

    type AllergyAndCrossSensitivityContraindications =
      | AllergyAndCrossSensitivityContraindications of drugProvider.Sectiondiv

    type AllergyAndCrossSensitivityCrossSensitivity =
        | AllergyAndCrossSensitivityCrossSensitivity of Option<Link> * drugProvider.Sectiondiv

    type ExceptionToLegalCategory = | ExceptionToLegalCategory of Option<Specificity> * drugProvider.Sectiondiv

    type DentalPractitionersFormulary = | DentalPractitionersFormulary of Option<Specificity> * drugProvider.Sectiondiv

    type MonographSection =
        | IndicationsAndDoseGroup of IndicationsAndDose seq
        | Pregnancy of Id * GeneralInformation seq
        | BreastFeeding of Id * GeneralInformation seq
        | HepaticImpairment of Id * GeneralInformation seq * DoseAdjustment seq
        | RenalImpairment of Id * GeneralInformation seq * AdditionalMonitoringInRenalImpairment seq * DoseAdjustment seq
        | PatientAndCarerAdvice of Id * AdviceAroundMissedDoses seq * GeneralPatientAdvice seq
        | MedicinalForms of Id * Option<LicensingVariationStatement> * Paragraphs * MedicinalFormLink seq
        | AllergyAndCrossSensitivity of Id * Option<AllergyAndCrossSensitivityContraindications> * Option<AllergyAndCrossSensitivityCrossSensitivity>
        | ExceptionsToLegalCategory of Id * ExceptionToLegalCategory seq
        | ProfessionSpecificInformation of Id *DentalPractitionersFormulary seq

    type Drug = | Drug of InteractionLink seq *
                        Classification seq *
                        Vtmid *
                        MonographSection seq *
                        PrimaryDomainOfEffect *
                        SecondaryDomainsOfEffect

module DrugParser =
    open Drug

    let (|?) = defaultArg

    //all of this needs a refactor

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
        if (name x) = n then Some(x)
        else None

    let inline hasName s x = name x = s

    let inline (|HasOutputClass|_|) (n:string) x =
        if (outputclass x) = OutputClass.lift n then Some(x)
        else None

    let inline (|HasOutputClasso|_|) (n:string) x =
        if (outputclasso x) = OutputClass.lift n then Some(x)
        else None

    let inline hasOutputclass (s:string) x = outputclass x = OutputClass.lift s
    let inline hasOutputclasso (s:string) x = outputclasso x = OutputClass.lift s

    type Paragraphs with
        static member from (x:Option<drugProvider.Sectiondiv>) =
          match x with
            | Some(x) -> Paragraphs.fromsd x
            | None -> Paragraphs Array.empty<Paragraph>
        static member fromsd (x:drugProvider.Sectiondiv) =
          x.Ps |> Array.map value |> Array.choose id |> Seq.map Paragraph |> Paragraphs
        static member froms (x:drugProvider.Section) =
          x.Ps |> Array.map value |> Array.choose id |> Seq.map Paragraph |> Paragraphs

    type Paragraph with
      static member from (x:drugProvider.P) =
        Paragraph(x.Value |? "")

    type Route with
      static member from (Paragraph x) = Route x
      static member from (Paragraphs xs) = Seq.map Route.from xs
      static member from (x:drugProvider.Ph) = Route(x.Value.Value)

    type Link with
      static member from (r:drugProvider.Xref) =
        {Url = r.Href; Title = r.Value}
      static member from (x:drugProvider.Data) =
        match x.Xref with
          |Some(r) -> Some(Link.from r)
          |None -> None
      static member from (x:drugProvider.P) =
        x.Xrefs |> Array.map Link.from |> Array.tryPick Some
      static member from (x:drugProvider.Sectiondiv) =
        x.Ps |> Array.choose Link.from

    type Indication with
      static member from (x:drugProvider.Ph) = Indication(x.Value.Value)

    type InteractionLink with
        static member from (x:drugProvider.Xref) = InteractionLink {Url = x.Href ; Title = x.Value}

    type MedicinalFormLink with
        static member from (x:drugProvider.Xref) = MedicinalFormLink {Url = x.Href ; Title = x.Value}

    type Content with
        static member from x = Content(Html(x.ToString()))

    type ReferenceableContent with
        static member from (x:drugProvider.Topic) = ReferenceableContent(Content.from x,Id(x.Id),x.Title.Value)

    type PatientGroup with
        static member from (x:drugProvider.Li) = {Group = x.Ps.[0].Value |? ""; Dosage = x.Ps.[1].Value |? "";}

    type TheraputicIndication with
      static member from (Paragraph x) = TheraputicIndication x
      static member from (Paragraphs xs) = Seq.map TheraputicIndication.from xs


    let sections (n:string) (x:drugProvider.Topic) = x.Body.Sections
                                                     |> Array.filter (fun s -> outputclasso s = OutputClass.lift n)

    type IndicationsAndDose with
      static member from (x:drugProvider.Section) =
         let theraputicIndications = x.Sectiondivs.[0] |> ( Paragraphs.fromsd >> TheraputicIndication.from )
         let routes = x |> (Paragraphs.froms >> Route.from >> Seq.toArray)
         let groups = x.Uls |> Array.map (fun u -> u.Lis |> Seq.map PatientGroup.from)
         let routesOfAdministration = Array.zip routes groups |> Array.map RouteOfAdministration
         IndicationsAndDose.IndicationsAndDose(theraputicIndications,routesOfAdministration)

    type MonographSection with
      static member indicationsAndDoseGroup (x:drugProvider.Topic) =
         IndicationsAndDoseGroup(x.Body.Sections |> Array.filter (hasOutputclasso "indicationAndDoseGroup") |> Array.map IndicationsAndDose.from)

    let medicinalForms (x:drugProvider.Topic) =
      let lvs = x.Body.Sections
                |> Array.filter (hasOutputclasso "licensingVariationStatement") 
                |> Array.map (Paragraphs.froms >> LicensingVariationStatement)
                |> Array.tryPick Some
      let ps = Paragraphs(x.Body.Ps |> Array.map Paragraph.from)
      let content = ReferenceableContent.from x
      let links = x.Xrefs |> Array.map MedicinalFormLink.from
      MedicinalForms(Id(x.Id),lvs,ps,links)

    let inline optn t f x = Some (t (f x))

    let inline opt t f x =
        let y = f x
        match y with
          | Some(y) -> Some (t (y))
          | None -> None

    type Specificity with
      static member from (x:drugProvider.P) =
        let r = x.Phs |> Array.filter (hasOutputclass "route") |> Array.map Route.from |> Array.tryPick Some 
        let i = x.Phs |> Array.filter (hasOutputclass "indication") |> Array.map Indication.from |> Array.tryPick Some
        Specificity(Paragraph.from x,r,i)

    let extractSpecificity (x:drugProvider.Sectiondiv) =
      x.Ps |> Array.filter (hasOutputclasso "specificity") |> Array.map Specificity.from |> Array.tryPick Some

    type Title with
      static member from (x:drugProvider.P) =
        Title(Paragraph.from x) 

    let extractTitle (x:drugProvider.Sectiondiv) =
      x.Ps |> Array.filter (hasOutputclasso "title") |> Array.map Title.from |> Array.tryPick Some

    type GeneralInformation with
      static member from (x:drugProvider.Sectiondiv) =
        GeneralInformation(x,extractSpecificity x)
      static member from (x:drugProvider.Section) =
        x.Sectiondivs |> Array.map GeneralInformation.from

    type DoseAdjustment with
      static member from (x:drugProvider.Sectiondiv) =
        DoseAdjustment(x,extractSpecificity x)
      static member from (x:drugProvider.Section) =
        x.Sectiondivs |> Array.map DoseAdjustment.from

    let subsections cl c s =
      s |> Array.filter (hasOutputclasso cl) |> Array.collect c

    let renalImpairment (x:drugProvider.Topic) =
      let gi = x.Body.Sections |> subsections "generalInformation" GeneralInformation.from
      let am = x.Body.Sections
               |> Array.filter (hasOutputclasso "additionalMonitoringInRenalImpairment")
               |> Array.map (Paragraphs.froms >> AdditionalMonitoringInRenalImpairment)
      let da = x.Body.Sections |> subsections "doseAdjustments" DoseAdjustment.from
      RenalImpairment(Id(x.Id),gi,am,da)

    type GeneralPatientAdvice with
      static member from (x:drugProvider.Sectiondiv) =
        GeneralPatientAdvice(x,extractTitle x, extractSpecificity x)
      static member from (x:drugProvider.Section) =
        x.Sectiondivs |> Array.map GeneralPatientAdvice.from

    let patientAndCarerAdvice (x:drugProvider.Topic) =
      let ga = x.Body.Sections |> subsections "generalPatientAdvice" GeneralPatientAdvice.from
      let am = x.Body.Sections |> subsections "adviceAroundMissedDoses" (fun s -> s.Sectiondivs |> Array.map AdviceAroundMissedDoses) 
      PatientAndCarerAdvice(Id(x.Id),am,ga)

    let withname = (|HasName|_|)
    let withclass = (|HasOutputClasso|_|)

    let (>=>) a b x =
      match (a x) with
        | Some x -> b x
        | None -> None

    let (>>|) a b x =
      match (a x) with
        | Some x -> Some(b x)
        | None -> None

    type Classification with
      static member from (x:drugProvider.Data) =
        let l = x.Datas |> Array.tryPick (Some >=> withname "drugClassification" >=> Link.from)
        let i = x.Datas |> Array.choose (Some >=> withname "inheritsFromClass" >=> Link.from >>| InheritsFromClass)
        Classification(l.Value,i)
      static member fromlist (x:drugProvider.Data) =
        x.Datas |> Array.filter (hasName "classification") |> Array.map Classification.from

    type TheraputicUse with
      static member from (x:drugProvider.Data) =
        TheraputicUse(x.String.Value,TheraputicUse.from x.Datas)
      static member from (x:drugProvider.Data []) =
        x |> Array.tryPick (Some >=> withname "therapeuticUse" >>| TheraputicUse.from)

    type SecondaryTheraputicUses with
      static member from (x:drugProvider.Data) =
        SecondaryTheraputicUses(TheraputicUse.from x.Datas)

    type PrimaryTheraputicUse with
      static member from (x:drugProvider.Data) =
        PrimaryTheraputicUse(TheraputicUse.from x.Datas)

    type DomainOfEffect with
      static member from (x:drugProvider.Data) =
        let p = x.Datas |> Array.tryPick (Some >=> withname "primaryTherapeuticUse" >>| PrimaryTheraputicUse.from)
        let s = x.Datas |> Array.tryPick (Some >=> withname "secondaryTherapeuticUses" >>| SecondaryTheraputicUses.from)
        DomainOfEffect(x.String.Value,p,s)

    type PrimaryDomainOfEffect with
      static member from (x:drugProvider.Body) =
        let d = x.Datas |> Array.tryPick (Some >=> withname "primaryDomainOfEffect" >>| PrimaryDomainOfEffect.from)
        PrimaryDomainOfEffect(d)
      static member from (x:drugProvider.Data) =
        x.Datas |> Array.pick (Some >=> withname "domainOfEffect" >>| DomainOfEffect.from)

    type SecondaryDomainsOfEffect with
      static member from (x:drugProvider.Body) =
        let ds = x.Datas
                 |> Array.choose (Some >=> withname "secondaryDomainsOfEffect" >>| SecondaryDomainsOfEffect.from)
                 |> Array.collect id
        SecondaryDomainsOfEffect(ds)
      static member from (x:drugProvider.Data) =
        x.Datas |> Array.choose (Some >=> withname "domainOfEffect" >>| DomainOfEffect.from)

    type Vtmid with
      static member from (x:drugProvider.Data) =
        match x.Number with
          | Some(n) -> Some(Vtmid(n))
          | None -> None

    type MonographSection with
      static member buidgi c (x:drugProvider.Topic) =
        let gi = x.Body.Sections |> subsections "generalInformation" GeneralInformation.from |> Array.toSeq
        c(Id(x.Id),gi)
      static member pregnancyfrom = MonographSection.buidgi Pregnancy
      static member breastFeedingFrom = MonographSection.buidgi BreastFeeding
      static member hepaticImparmentFrom (x:drugProvider.Topic) =
        let hi = x.Body.Sections |> subsections "doseAdjustments" DoseAdjustment.from |> Array.toSeq
        let c (i,gi) = HepaticImpairment(i, gi, hi) //build a partial constructor
        MonographSection.buidgi c x

    type AllergyAndCrossSensitivityContraindications with
      static member from (x:drugProvider.Section) = x.Sectiondivs |> Array.map AllergyAndCrossSensitivityContraindications |> Array.tryPick Some

    type AllergyAndCrossSensitivityCrossSensitivity with
      static member from (x:drugProvider.Section) = 
        let l = x.Sectiondivs |> Array.collect Link.from |> Array.tryPick Some
        AllergyAndCrossSensitivityCrossSensitivity(l,x.Sectiondivs.[0])

    type MonographSection with
      static member allergyAndCrossSensitivity (x:drugProvider.Topic) =
        let ac = x.Body.Sections
                 |> Array.tryPick (Some >=> withclass "allergyAndCrossSensitivityContraindications" >=> AllergyAndCrossSensitivityContraindications.from)
        let acss = x.Body.Sections
                 |> Array.tryPick (Some >=> withclass "allergyAndCrossSensitivityCrossSensitivity" >>| AllergyAndCrossSensitivityCrossSensitivity.from)
        AllergyAndCrossSensitivity(Id(x.Id),ac,acss)

    type ExceptionToLegalCategory with
      static member from (x:drugProvider.Sectiondiv) =
        ExceptionToLegalCategory(extractSpecificity x,x)

    type MonographSection with
      static member exceptionsToLegalCategory (x:drugProvider.Topic) =
        ExceptionsToLegalCategory(Id(x.id),x.Body.Sections |> Array.collect (fun s -> s.Sectiondivs) |> Array.map ExceptionToLegalCategory.from)

    type DentalPractitionersFormulary with
      static member from (x:drugProvider.Sectiondiv) =
        DentalPractitionersFormulary(extractSpecificity x,x)
      static member from (x:drugProvider.Section) =
        x.Sectiondivs |> DentalPractitionersFormulary.from

    type MonographSection with
      static member professionSpecificInformation (x:drugProvider.Topic) =
        ProfessionSpecificInformation(Id(x.Id),x.Body.Sections |> Array.filter (hasOutputclasso "dentalPractitionersFormulary") |> Array.collect DentalPractitionersFormulary.from)

    let parse (x:drugProvider.Topic) =
        let interactionLinks = x.Body.Ps
                               |> Array.filter (hasOutputclasso "interactionsLinks")
                               |> Array.collect (fun p -> p.Xrefs |> Array.map InteractionLink.from)

        let classifications = x.Body.Datas
                              |> Array.filter (hasName "classifications")
                              |> Array.collect (fun cs -> Classification.fromlist cs)

        let vtmid = x.Body.Datas |> Array.pick (Some >=> withname "vtmid" >=> Vtmid.from)

        let inline sectionFn x =
            match x with
                | HasOutputClass "indicationsAndDose" _ -> Some(MonographSection.indicationsAndDoseGroup x)
                | HasOutputClass "pregnancy" _ -> Some(MonographSection.pregnancyfrom x)
                | HasOutputClass "breastFeeding" _ -> Some(MonographSection.breastFeedingFrom x)
                | HasOutputClass "hepaticImpairment" _ -> Some(MonographSection.hepaticImparmentFrom x)
                | HasOutputClass "renalImpairment" _ -> Some(renalImpairment x)
                | HasOutputClass "patientAndCarerAdvice" _ -> Some(patientAndCarerAdvice x)
                | HasOutputClass "medicinalForms" _ -> Some(medicinalForms x)
                | HasOutputClass "exceptionsToLegalCategory" _ -> Some(MonographSection.exceptionsToLegalCategory x)
                | HasOutputClass "professionSpecificInformation" _ -> Some(MonographSection.professionSpecificInformation x)
                | _ -> None

        let sections =
          x.Topics |> Array.map sectionFn |> Array.choose id

        let primaryDomainOfEffect = PrimaryDomainOfEffect.from x.Body
        let secondaryDomainsOfEffect = SecondaryDomainsOfEffect.from x.Body

        Drug(interactionLinks,classifications,vtmid,sections,primaryDomainOfEffect, secondaryDomainsOfEffect)
 