namespace Bnf
open FSharp.Data

module Drug =
    //sensible compromise to reference the types provided to avoid replication
    type drugProvider = XmlProvider<"SuperDrug.xml", Global=true, SampleIsList=true>

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

    type Classification = | Classification of Option<Link> * InheritsFromClass seq

    type DrugName = | DrugName of string

    type Vtmid = | Vtmid of int

    type MedicinalFormLink = | MedicinalFormLink of Link

    type TheraputicIndication = | TheraputicIndication of string

    type PatientGroup = {Group:string; Dosage:string;}

    type Route = | Route of string
    type Indication = | Indication of string

    type RouteOfAdministration = | RouteOfAdministration of Option<Route> * PatientGroup seq 

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

    type PrimaryDomainOfEffect = | PrimaryDomainOfEffect of DomainOfEffect

    type SecondaryDomainsOfEffect = | SecondaryDomainsOfEffect of DomainOfEffect seq

    type AllergyAndCrossSensitivityContraindications =
      | AllergyAndCrossSensitivityContraindications of drugProvider.Sectiondiv

    type AllergyAndCrossSensitivityCrossSensitivity =
        | AllergyAndCrossSensitivityCrossSensitivity of Option<Link> * drugProvider.Sectiondiv

    type ExceptionToLegalCategory = | ExceptionToLegalCategory of Option<Specificity> * drugProvider.Sectiondiv

    type DentalPractitionersFormulary = | DentalPractitionersFormulary of Option<Specificity> * drugProvider.Sectiondiv

    type EffectOnLaboratoryTest = | EffectOnLaboratoryTest of drugProvider.Sectiondiv

    type PreTreatmentScreening = | PreTreatmentScreening of drugProvider.Sectiondiv

    type LessSuitableForPrescribing = | LessSuitableForPrescribing of Option<Specificity> * drugProvider.Sectiondiv
 
    type HandlingAndStorage = | HandlingAndStorage of Option<Specificity> * drugProvider.Sectiondiv

    type TreatmentCessation = | TreatmentCessation of drugProvider.Sectiondiv

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
        | EffectOnLaboratoryTests of Id * EffectOnLaboratoryTest seq
        | PreTreatmentScreenings of Id * PreTreatmentScreening seq
        | LessSuitableForPrescribings of Id * LessSuitableForPrescribing seq
        | HandlingAndStorages of Id * HandlingAndStorage seq
        | TreatmentCessations of Id * TreatmentCessation seq

    type Drug = {id : Id;
                 name : DrugName;
                 interactionLinks : InteractionLink seq;
                 classifications : Classification seq;
                 vtmid : Option<Vtmid>;
                 sections : MonographSection seq;
                 primaryDomainOfEffect : Option<PrimaryDomainOfEffect>;
                 secondaryDomainsOfEffect : Option<SecondaryDomainsOfEffect>;}

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
      static member from (x:drugProvider.P) =
        x.Xrefs |> Array.map Link.from |> Array.tryPick Some
      static member from (x:drugProvider.Sectiondiv) =
        x.Ps |> Array.choose Link.from
      static member fromd (x:drugProvider.Data) =
        match x.Xref with
          |Some(r) -> Some(Link.from r)
          |None -> None

    type Indication with
      static member from (x:drugProvider.Ph) = Indication(x.Value.Value)

    type InteractionLink with
        static member from (x:drugProvider.Xref) = InteractionLink {Url = x.Href ; Title = x.Value}

    type MedicinalFormLink with
        static member from (x:drugProvider.Xref) = MedicinalFormLink {Url = x.Href ; Title = x.Value}

    type Content with
        static member from x = Content(Html(x.ToString()))

    type ReferenceableContent with
        static member from (x:drugProvider.Topic) = ReferenceableContent(Content.from x,Id(x.Id),x.Title)

    type PatientGroup with
        static member from (x:drugProvider.Li) = {Group = x.Ps.[0].Value |? ""; Dosage = x.Ps.[1].Value |? "";}

    type TheraputicIndication with
      static member from (Paragraph x) = TheraputicIndication x
      static member from (Paragraphs xs) = Seq.map TheraputicIndication.from xs


    let sections (n:string) (x:drugProvider.Topic) =
      match x.Body with
        | Some(b) -> b.Sections |> Array.filter (fun s -> outputclasso s = OutputClass.lift n)
        | None -> Array.empty<drugProvider.Section>

    type IndicationsAndDose with
      static member from (x:drugProvider.Section) =
         let theraputicIndications = x.Sectiondivs.[0] |> ( Paragraphs.fromsd >> TheraputicIndication.from )
         let routes = x |> (Paragraphs.froms >> Route.from >> Seq.toArray) |> Array.map Some
         let groups = x.Uls |> Array.map (fun u -> u.Lis |> Seq.map PatientGroup.from)
         //if there are no routes then return something else
         let routesOfAdministration =
            match routes with
            | [||] -> [|RouteOfAdministration(None,groups.[0]) |]
            | _ -> Array.zip routes groups |> Array.map RouteOfAdministration
         IndicationsAndDose.IndicationsAndDose(theraputicIndications,routesOfAdministration)

    type MonographSection with
      static member indicationsAndDoseGroup (x:drugProvider.Topic) =
        match x.Body with
          | Some(b) -> Some(IndicationsAndDoseGroup(b.Sections |> Array.filter (hasOutputclasso "indicationAndDoseGroup") |> Array.map IndicationsAndDose.from))
          | None -> None

    let (>>=) a b = Option.bind b a

    let lvs (b:drugProvider.Body) =
      b.Sections
      |> Array.filter (hasOutputclasso "licensingVariationStatement")
      |> Array.map (Paragraphs.froms >> LicensingVariationStatement)
      |> Array.tryPick Some

    let medicinalForms (x:drugProvider.Topic) =
      let lvs = x.Body >>= lvs
      let ps = Paragraphs(match x.Body with
                          | Some(b) -> b.Ps |> Array.map Paragraph.from
                          | None -> Array.empty<Paragraph>)
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

    let addSpecificity x =
      extractSpecificity x, x

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
      match x.Body with
        | Some(b) -> 
           let gi = b.Sections |> subsections "generalInformation" GeneralInformation.from
           let am = b.Sections
                    |> Array.filter (hasOutputclasso "additionalMonitoringInRenalImpairment")
                    |> Array.map (Paragraphs.froms >> AdditionalMonitoringInRenalImpairment)
           let da = b.Sections |> subsections "doseAdjustments" DoseAdjustment.from
           Some(RenalImpairment(Id(x.Id),gi,am,da))
        | None -> None

    type GeneralPatientAdvice with
      static member from (x:drugProvider.Sectiondiv) =
        GeneralPatientAdvice(x,extractTitle x, extractSpecificity x)
      static member from (x:drugProvider.Section) =
        x.Sectiondivs |> Array.map GeneralPatientAdvice.from

    let patientAndCarerAdvice (x:drugProvider.Topic) =
      match x.Body with
        | Some(b) -> 
          let ga = b.Sections |> subsections "generalPatientAdvice" GeneralPatientAdvice.from
          let am = b.Sections |> subsections "adviceAroundMissedDoses" (fun s -> s.Sectiondivs |> Array.map AdviceAroundMissedDoses) 
          Some(PatientAndCarerAdvice(Id(x.Id),am,ga))
        | None -> None

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
        let l = x.Datas |> Array.tryPick (Some >=> withname "drugClassification" >=> Link.fromd)
        let i = x.Datas |> Array.choose (Some >=> withname "inheritsFromClass" >=> Link.fromd >>| InheritsFromClass)
        Classification(l,i)
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
        match d with
          | Some(d) -> Some(PrimaryDomainOfEffect(d))
          | None -> None
      static member from (x:drugProvider.Data) =
        x.Datas |> Array.pick (Some >=> withname "domainOfEffect" >>| DomainOfEffect.from)

    type SecondaryDomainsOfEffect with
      static member from (x:drugProvider.Body) =
        let ds = x.Datas
                 |> Array.choose (Some >=> withname "secondaryDomainsOfEffect" >>| SecondaryDomainsOfEffect.from)
                 |> Array.collect id
        match ds with
          | [||] -> None
          | _ -> Some(SecondaryDomainsOfEffect(ds))
      static member from (x:drugProvider.Data) =
        x.Datas |> Array.choose (Some >=> withname "domainOfEffect" >>| DomainOfEffect.from)

    type Vtmid with
      static member from (x:drugProvider.Data) =
        match x.Number with
          | Some(n) -> Some(Vtmid(n))
          | None -> None

    type MonographSection with
      static member buidgi c (x:drugProvider.Topic) =
        match x.Body with
          | Some(b) ->
              let gi =b.Sections |> subsections "generalInformation" GeneralInformation.from |> Array.toSeq
              Some(c(Id(x.Id),gi))
          | None -> None
      static member pregnancyfrom = MonographSection.buidgi Pregnancy
      static member breastFeedingFrom = MonographSection.buidgi BreastFeeding
      static member hepaticImparmentFrom (x:drugProvider.Topic) =
        match x.Body with
          | Some(b) ->
                    let hi = b.Sections |> subsections "doseAdjustments" DoseAdjustment.from |> Array.toSeq
                    let c (i,gi) = HepaticImpairment(i, gi, hi) //build a partial constructor
                    MonographSection.buidgi c x
          | None -> None

    type AllergyAndCrossSensitivityContraindications with
      static member from (x:drugProvider.Section) = x.Sectiondivs |> Array.map AllergyAndCrossSensitivityContraindications |> Array.tryPick Some

    type AllergyAndCrossSensitivityCrossSensitivity with
      static member from (x:drugProvider.Section) = 
        let l = x.Sectiondivs |> Array.collect Link.from |> Array.tryPick Some
        AllergyAndCrossSensitivityCrossSensitivity(l,x.Sectiondivs.[0])

    type MonographSection with
      static member allergyAndCrossSensitivity (x:drugProvider.Topic) =
        match x.Body with
          | Some(b) -> 
            let ac = b.Sections
                     |> Array.tryPick (Some >=> withclass "allergyAndCrossSensitivityContraindications" >=> AllergyAndCrossSensitivityContraindications.from)
            let acss = b.Sections
                       |> Array.tryPick (Some >=> withclass "allergyAndCrossSensitivityCrossSensitivity" >>| AllergyAndCrossSensitivityCrossSensitivity.from)
            Some(AllergyAndCrossSensitivity(Id(x.Id),ac,acss))
          | None -> None

    type ExceptionToLegalCategory with
      static member from (x:drugProvider.Sectiondiv) =
        ExceptionToLegalCategory(extractSpecificity x,x)

    type MonographSection with
      static member exceptionsToLegalCategory (x:drugProvider.Topic) =
        let es = match x.Body with
                       | Some(b) -> b.Sections |> Array.collect (fun s -> s.Sectiondivs) |> Array.map ExceptionToLegalCategory.from
                       | None -> Array.empty<ExceptionToLegalCategory>
        ExceptionsToLegalCategory(Id(x.Id),es)

    type DentalPractitionersFormulary with
      static member from (x:drugProvider.Sectiondiv) =
        DentalPractitionersFormulary(extractSpecificity x,x)
      static member from (x:drugProvider.Section) =
        x.Sectiondivs |> Array.map DentalPractitionersFormulary.from

    type MonographSection with
      static member professionSpecificInformation (x:drugProvider.Topic) =
        let psi = match x.Body with
          | Some(b) -> b.Sections |> Array.filter (hasOutputclasso "dentalPractitionersFormulary") |> Array.collect DentalPractitionersFormulary.from
          | None -> Array.empty<DentalPractitionersFormulary>
        ProfessionSpecificInformation(Id(x.Id),psi)

    let allsections (x:drugProvider.Topic) =
      match x.Body with
        | Some(b) -> b.Sections |> Array.collect (fun s -> s.Sectiondivs)
        | None -> Array.empty<drugProvider.Sectiondiv>

    type MonographSection with
      static member effectOnLaboratoryTests (x:drugProvider.Topic) =
        EffectOnLaboratoryTests(Id(x.Id),allsections x |> Array.map EffectOnLaboratoryTest)
      static member preTreatmentScreenings (x:drugProvider.Topic) =
        PreTreatmentScreenings(Id(x.Id), allsections x |> Array.map PreTreatmentScreening)
      static member lessSuitableForPrescribings (x:drugProvider.Topic) =
        LessSuitableForPrescribings(Id(x.Id), allsections x |> Array.map (addSpecificity >> LessSuitableForPrescribing))
      static member handlingAndStorages (x:drugProvider.Topic) =
        HandlingAndStorages(Id(x.Id), allsections x |> Array.map (addSpecificity >> HandlingAndStorage))
      static member treatmentCessations (x:drugProvider.Topic) =
        TreatmentCessations(Id(x.Id), allsections x |> Array.map TreatmentCessation)

    let parse (x:drugProvider.Topic) =
        let name = DrugName(x.Title)

        let interactionLinks = match x.Body with
                               | Some(b) -> b.Ps |> Array.filter (hasOutputclasso "interactionsLinks")
                                                 |> Array.collect (fun p -> p.Xrefs |> Array.map InteractionLink.from)
                               | None -> Array.empty<InteractionLink>

        let classifications = match x.Body with
                              | Some(b) -> b.Datas
                                           |> Array.filter (hasName "classifications")
                                           |> Array.collect (fun cs -> Classification.fromlist cs)
                              | None -> Array.empty<Classification>

        let vtmid = x.Body >>= (fun b ->  b.Datas |> Array.tryPick (Some >=> withname "vtmid" >=> Vtmid.from))

        let inline sectionFn x =
            match x with
                | HasOutputClass "indicationsAndDose" _ -> MonographSection.indicationsAndDoseGroup x
                | HasOutputClass "pregnancy" _ -> MonographSection.pregnancyfrom x
                | HasOutputClass "breastFeeding" _ -> MonographSection.breastFeedingFrom x
                | HasOutputClass "hepaticImpairment" _ -> MonographSection.hepaticImparmentFrom x
                | HasOutputClass "renalImpairment" _ -> renalImpairment x
                | HasOutputClass "patientAndCarerAdvice" _ -> patientAndCarerAdvice x
                | HasOutputClass "medicinalForms" _ -> Some(medicinalForms x)
                | HasOutputClass "exceptionsToLegalCategory" _ -> Some(MonographSection.exceptionsToLegalCategory x)
                | HasOutputClass "professionSpecificInformation" _ -> Some(MonographSection.professionSpecificInformation x)
                | HasOutputClass "effectOnLaboratoryTests" _ -> Some(MonographSection.effectOnLaboratoryTests x)
                | HasOutputClass "preTreatmentScreening" _ -> Some(MonographSection.preTreatmentScreenings x)
                | HasOutputClass "lessSuitableForPrescribing" _ -> Some(MonographSection.lessSuitableForPrescribings x)
                | HasOutputClass "handlingAndStorage" _ -> Some(MonographSection.handlingAndStorages x)
                | HasOutputClass "treatmentCessation" _ -> Some(MonographSection.treatmentCessations x)
                | HasOutputClass "allergyAndCrossSensitivity" _ -> MonographSection.allergyAndCrossSensitivity x
                | _ -> None

        let sections =
          x.Topics |> Array.map sectionFn |> Array.choose id

        let primaryDomainOfEffect = x.Body >>= PrimaryDomainOfEffect.from
        let secondaryDomainsOfEffect = x.Body >>= SecondaryDomainsOfEffect.from

        {id = Id(x.Id); name = name; interactionLinks = interactionLinks; classifications = classifications; vtmid = vtmid; sections = sections; primaryDomainOfEffect = primaryDomainOfEffect; secondaryDomainsOfEffect = secondaryDomainsOfEffect}
