namespace Bnf
open FSharp.Data

module Shared =
  type Id =
    | Id of string
    override __.ToString () = match __ with | Id x -> x


module Drug =
    open Shared
    //sensible compromise to reference the types provided to avoid replication
    type drugProvider = XmlProvider<"SuperDrug.xml", Global=true, SampleIsList=true>

    type Paragraph = | Paragraph of string
    type Paragraphs = | Paragraphs of Paragraph seq
    type Title = | Title of Paragraph
    type Html = | Html of string
    type Content = | Content of Html

    type Link = {Title:string; Url:string}

    type ReferenceableContent = | ReferenceableContent of Content * Id * string

    type InteractionLink = | InteractionLink of Link

    type InheritsFromClass = | InheritsFromClass of string

    type Classification = | Classification of Id * InheritsFromClass seq

    type DrugName = | DrugName of string

    type Vtmid = | Vtmid of int64

    type MedicinalFormLink = | MedicinalFormLink of Link

    type TheraputicIndication = | TheraputicIndication of string

    type PatientGroup = {Group:string; Dosage:string;}

    type Route = | Route of string
    type Indication = | Indication of string

    type RouteOfAdministration = | RouteOfAdministration of Option<Route> * PatientGroup seq 

    type IndicationsAndDose = | IndicationsAndDose  of TheraputicIndication seq * RouteOfAdministration seq

    type IndicationsAndDoseSection =
      | Pharmacokinetics of drugProvider.Section
      | DoseEquivalence of drugProvider.Section
      | DoseAdjustments of drugProvider.Section
      | ExtremesOfBodyWeight of drugProvider.Section
      | Potency of drugProvider.Section

    type Specificity = | Specificity of Paragraph * Option<Route> * Option<Indication>

    type GeneralInformation = | GeneralInformation of drugProvider.Sectiondiv * Option<Specificity>

    type DoseAdjustment = | DoseAdjustment of drugProvider.Sectiondiv * Option<Specificity>

    type AdditionalMonitoringInRenalImpairment = | AdditionalMonitoringInRenalImpairment of string

    type LicensingVariationStatement = | LicensingVariationStatement of Html

    type PatientResources = | PatientResources of Paragraphs

    type AdviceAroundMissedDoses = | AdviceAroundMissedDoses of drugProvider.Sectiondiv

    type GeneralPatientAdvice = | GeneralPatientAdvice of drugProvider.Sectiondiv * Option<Title> * Option<Specificity> 

    type TheraputicUse =
      | TheraputicUse of string * Option<TheraputicUse> 
    type PrimaryTheraputicUse = | PrimaryTheraputicUse of Option<TheraputicUse>

    type SecondaryTheraputicUses = | SecondaryTheraputicUses of Option<TheraputicUse>

    type DomainOfEffect = | DomainOfEffect of
                            Option<string> * Option<PrimaryTheraputicUse> * Option<SecondaryTheraputicUses>

    type PrimaryDomainOfEffect = | PrimaryDomainOfEffect of DomainOfEffect

    type SecondaryDomainsOfEffect = | SecondaryDomainsOfEffect of DomainOfEffect seq

    type AllergyAndCrossSensitivityContraindications =
      | AllergyAndCrossSensitivityContraindications of drugProvider.Sectiondiv

    type AllergyAndCrossSensitivityCrossSensitivity =
        | AllergyAndCrossSensitivityCrossSensitivity of drugProvider.Sectiondiv

    type ExceptionToLegalCategory = | ExceptionToLegalCategory of Option<Specificity> * drugProvider.Sectiondiv

    type DentalPractitionersFormulary = | DentalPractitionersFormulary of Option<Specificity> * drugProvider.Sectiondiv

    type EffectOnLaboratoryTest = | EffectOnLaboratoryTest of drugProvider.Sectiondiv

    type PreTreatmentScreening = | PreTreatmentScreening of drugProvider.Sectiondiv

    type LessSuitableForPrescribing = | LessSuitableForPrescribing of Option<Specificity> * drugProvider.Sectiondiv
 
    type HandlingAndStorage = | HandlingAndStorage of Option<Specificity> * drugProvider.Sectiondiv

    type TreatmentCessation = | TreatmentCessation of drugProvider.Sectiondiv

    type DrugAction = | DrugAction of drugProvider.Sectiondiv

    type SideEffectAdvice = | SideEffectAdvice of Option<Specificity> * drugProvider.Sectiondiv

    type SideEffect = | SideEffect of string

    type Frequency =
      | GeneralFrequency of string * SideEffect seq
      | SpecificFrequency of string * SideEffect seq * Title option

    type Contraindication = | Contraindication of drugProvider.Ph

    type ImportantAdvice = | ImportantAdvice of Title option * Specificity option * drugProvider.Sectiondiv

    type Caution = Caution of drugProvider.Ph

    type CautionsGroup =
      | GeneralCautions of drugProvider.P * Caution list
      | CautionsWithRoutes of string * drugProvider.P * Caution list
      | CautionsWithIndications of string * drugProvider.P * Caution list

    type PrescribingAndDispensingInformation =
      | PrescribingAndDispensingInformation of Option<Specificity> * drugProvider.Sectiondiv

    type UnlicencedUse =
      | UnlicencedUse of Option<Specificity> * drugProvider.Sectiondiv

    type MonitoringRequirement =
      | PatientMonitoringProgrammes of Option<Specificity> * drugProvider.Sectiondiv
      | TheraputicDrugMonitoring of Option<Specificity> * drugProvider.Sectiondiv
      | MonitoringOfPatientParameters of Option<Specificity> * drugProvider.Sectiondiv

    type ConceptionAndContraception =
      | ConceptionAndContraception of Option<Specificity> * drugProvider.Sectiondiv

    type ImportantSafetyInformation =
      | ImportantSafetyInformation of Option<Title> * Option<Specificity> * drugProvider.Sectiondiv

    type DirectionsForAdministration =
      | DirectionsForAdministration of Option<Specificity> * drugProvider.Sectiondiv

    type FundingIdentifier = | FundingIdentifier of string

    type FundingDecision =
      | NonNHS of Specificity option * drugProvider.Sectiondiv
      | NiceTechnologyAppraisals of FundingIdentifier option * Title option * Specificity option * drugProvider.Sectiondiv
      | SmcDecisions of Specificity option * drugProvider.Sectiondiv

    type Interaction = | Interaction of Specificity option * drugProvider.Sectiondiv

    type MonographSection =
        | IndicationsAndDoseGroup of Id * IndicationsAndDose seq * IndicationsAndDoseSection seq
        | Pregnancy of Id * GeneralInformation seq
        | BreastFeeding of Id * GeneralInformation seq
        | HepaticImpairment of Id * GeneralInformation seq * DoseAdjustment seq
        | RenalImpairment of Id * GeneralInformation seq * AdditionalMonitoringInRenalImpairment seq * DoseAdjustment seq
        | PatientAndCarerAdvice of Id * AdviceAroundMissedDoses seq * GeneralPatientAdvice seq
        | MedicinalForms of Id * Option<LicensingVariationStatement> * Option<Html> * MedicinalFormLink seq
        | AllergyAndCrossSensitivity of Id * Option<AllergyAndCrossSensitivityContraindications> * Option<AllergyAndCrossSensitivityCrossSensitivity>
        | ExceptionsToLegalCategory of Id * ExceptionToLegalCategory seq
        | ProfessionSpecificInformation of Id * DentalPractitionersFormulary seq
        | EffectOnLaboratoryTests of Id * EffectOnLaboratoryTest seq
        | PreTreatmentScreenings of Id * PreTreatmentScreening seq
        | LessSuitableForPrescribings of Id * LessSuitableForPrescribing seq
        | HandlingAndStorages of Id * HandlingAndStorage seq
        | TreatmentCessations of Id * TreatmentCessation seq
        | DrugActions of Id * DrugAction seq
        | SideEffects of Id * Frequency seq * SideEffectAdvice seq
        | Contraindications of Id * Contraindication seq * drugProvider.P seq * ImportantAdvice seq
        | Cautions of Id * CautionsGroup list * ImportantAdvice seq
        | PrescribingAndDispensingInformations of Id * PrescribingAndDispensingInformation seq
        | UnlicencedUses of Id * UnlicencedUse seq
        | MonitoringRequirements of Id * MonitoringRequirement seq
        | ConceptionAndContraceptions of Id * ConceptionAndContraception seq
        | ImportantSafetyInformations of Id * ImportantSafetyInformation seq
        | DirectionsForAdministrations of Id * DirectionsForAdministration seq
        | NationalFunding of Id * FundingDecision seq
        | Interactions of Id * Interaction seq

//re look at indications and dose group : doseAdjustments,doseEquivalence,extremesOfBodyWeight,pharmacokineticspotency
//add revision numbers

    type Drug = {id : Id;
                 name : DrugName;
                 interactionLinks : InteractionLink seq;
                 classifications : Classification seq;
                 vtmid : Option<Vtmid>;
                 sections : MonographSection seq;
                 primaryDomainOfEffect : Option<PrimaryDomainOfEffect>;
                 secondaryDomainsOfEffect : Option<SecondaryDomainsOfEffect>;}

module DrugParser =
    open prelude
    open Drug
    open Shared

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

    type IndicationsAndDoseSection with
      static member from (x:drugProvider.Section) =
        match x with
          | HasOutputClasso "pharmacokinetics" _ -> Pharmacokinetics x |> Some
          | HasOutputClasso "doseEquivalence" _ -> DoseEquivalence x |> Some
          | HasOutputClasso "doseAdjustments" _ -> DoseAdjustments x |> Some
          | HasOutputClasso "extremesOfBodyWeight" _ -> ExtremesOfBodyWeight x |> Some
          | HasOutputClasso "potency" _ -> Potency x |> Some
          | _ -> None

    type MonographSection with
      static member indicationsAndDoseGroup (x:drugProvider.Topic) =
        match x.Body with
          | Some(b) ->
             let grps = b.Sections |> Array.filter (hasOutputclasso "indicationAndDoseGroup") |> Array.map IndicationsAndDose.from
             let idgss = b.Sections |> Array.choose IndicationsAndDoseSection.from
             Some(IndicationsAndDoseGroup(Id(x.Id),grps,idgss))
          | None -> None

    let (>>=) a b = Option.bind b a

    let lvs (b:drugProvider.Body) =
      b.Sections
      |> Array.filter (hasOutputclasso "licensingVariationStatement")
      |> Array.collect (fun s -> s.Ps)
      |> Array.map (string >> Html >> LicensingVariationStatement)
      |> Array.tryPick Some

    let medicinalForms (x:drugProvider.Topic) =
      let lvs = x.Body >>= lvs
      let ps = match x.Body with
                          | Some(b) -> Some(Html(b.ToString()))
                          | None -> None
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

    let addTitle (sp,s) =
      extractTitle s,sp,s

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
                    |> Array.map (string >> AdditionalMonitoringInRenalImpairment)
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


    type InheritsFromClass with
      static member from (x:drugProvider.Data) =
        match x.String with
          | Some(s) -> InheritsFromClass s |> Some
          | None -> None

    type Classification with
      static member from (x:drugProvider.Data) =
        let l = x.Datas |> Array.tryPick (Some >=> withname "drugClassification" >=> (fun d -> d.String >>= (Id >> Some)))
        let i = x.Datas |> Array.choose (Some >=> withname "inheritsFromClass" >=> InheritsFromClass.from)
        match l with
          | Some(l) -> Some( Classification(l,i))
          | None -> None
      static member fromlist (x:drugProvider.Data) =
        x.Datas |> Array.filter (hasName "classification") |> Array.choose Classification.from

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
        DomainOfEffect(x.String,p,s)

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
        match x.String with
          | Some(n) -> Some(Vtmid(int64 n))
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
      static member from (x:drugProvider.Section) =
        x.Sectiondivs |> Array.map AllergyAndCrossSensitivityContraindications |> Array.tryPick Some

    type AllergyAndCrossSensitivityCrossSensitivity with
      static member from (x:drugProvider.Section) =
        x.Sectiondivs |> Array.map AllergyAndCrossSensitivityCrossSensitivity |> Array.tryPick Some

    type MonographSection with
      static member allergyAndCrossSensitivity (x:drugProvider.Topic) =
        match x.Body with
          | Some(b) -> 
            let ac = b.Sections
                     |> Array.tryPick (Some >=> withclass "allergyAndCrossSensitivityContraindications" >=> AllergyAndCrossSensitivityContraindications.from)
            let acss = b.Sections
                       |> Array.tryPick (Some >=> withclass "allergyAndCrossSensitivityCrossSensitivity" >=> AllergyAndCrossSensitivityCrossSensitivity.from)
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

    //these three really need to be recfactored into something more sensible

    let allsections (x:drugProvider.Topic) =
      match x.Body with
        | Some b -> b.Sections |> Array.collect (fun s -> s.Sectiondivs)
        | None -> Array.empty<drugProvider.Sectiondiv>

    let sectiondivs cl (s:drugProvider.Section[]) =
      s |> Array.filter (hasOutputclasso cl) |> Array.collect (fun sec -> sec.Sectiondivs)

    let somesections cl  (x:drugProvider.Topic) =
        match x.Body with
          | Some b -> b.Sections |> (sectiondivs cl)
          | None -> Array.empty<drugProvider.Sectiondiv>

    type Frequency with
      static member fromge (x:drugProvider.Sectiondiv) =
        let c = x.Outputclass.Value
        let s = x.Sectiondivs.[0].Ps.[0].Phs |> Array.map (fun ph -> SideEffect ph.Value.Value)
        GeneralFrequency(c,s)
      static member fromsp (x:drugProvider.Sectiondiv) =
        let c = x.Outputclass.Value
        let s = x.Sectiondivs.[0].Ps.[0].Phs |> Array.map (fun ph -> SideEffect ph.Value.Value)
        let t = extractTitle x
        SpecificFrequency(c,s,t)

    type SideEffectAdvice with
      static member from (x:drugProvider.Sectiondiv) =
        addSpecificity x |> SideEffectAdvice

    type Contraindication with
      static member from (x:drugProvider.Section) =
        let phs = x.Ps |> Array.collect (fun p -> p.Phs)
        phs |> Array.filter (hasOutputclass "contraindication") |> Array.map Contraindication
      static member content (x:drugProvider.Section) =
        x.Ps

    type Caution with
      static member from (x:drugProvider.Ph) = Caution x
    type CautionsGroup with
      static member from (x:drugProvider.Section) =
        let gen p = GeneralCautions(p, p.Phs |> Array.map Caution.from |> Array.toList)
        let ac (x:drugProvider.Sectiondiv) =
          match x with
            | HasOutputClasso "cautionsOrContraindicationsWithRoutes" s ->
                CautionsWithRoutes(s.Ps.[0].Value.Value, s.Ps.[1], s.Ps.[1].Phs |> Array.map Caution.from |> Array.toList)
            | HasOutputClasso "cautionsOrContraindicationsWithIndications" s ->
                CautionsWithIndications(s.Ps.[0].Value.Value, s.Ps.[1], s.Ps.[1].Phs |> Array.map Caution.from |> Array.toList)

        [|x.Ps |> Array.map gen
          x.Sectiondivs |> Array.filter (hasOutputclasso "additionalCautions") |> Array.collect (fun sd -> sd.Sectiondivs |> Array.map ac) |] |> Array.collect id

    let firstsection n (x:drugProvider.Topic) =
      match x.Body with
        | Some b -> b.Sections |> Array.tryPick n
        | None -> None

    type MonitoringRequirement with
      static member from (x:drugProvider.Section) =
        let build c = x.Sectiondivs |> Array.map (addSpecificity >> c)
        match x with
          | HasOutputClasso "patientMonitoringProgrammes" _ -> build PatientMonitoringProgrammes
          | HasOutputClasso "therapeuticDrugMonitoring" _ -> build TheraputicDrugMonitoring
          | HasOutputClasso "monitoringOfPatientParameters" _ -> build MonitoringOfPatientParameters

    type FundingIdentifier with
      static member from (x:drugProvider.P) =
        x.Value >>= (FundingIdentifier >> Some)

    type FundingDecision with
      static member from (x:drugProvider.Sectiondiv) =
        match x with
          | HasOutputClasso "niceTechnologyAppraisals" _ ->
             let fid = function
                          | HasOutputClasso "fundingIdentifier" p -> FundingIdentifier.from p
                          | _ -> None
             let s1 = x.Sectiondivs.[0]
             let s2 = s1.Sectiondivs.[0]
             let fi = s1.Ps |> Array.tryPick fid
             let (t,sp,s) = s2 |> (addSpecificity >> addTitle)
             NiceTechnologyAppraisals (fi,t,sp,s)
          | HasOutputClasso "smcDecisions" _ ->
             let s2 = x.Sectiondivs.[0].Sectiondivs.[0]
             s2 |> (addSpecificity >> SmcDecisions)
      static member from (x:drugProvider.Section) =
        match x with
          | HasOutputClasso "nonNHS" _ -> x.Sectiondivs |> Array.map (addSpecificity >> NonNHS)
          | _ -> x.Sectiondivs |> Array.map FundingDecision.from 

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
      static member drugActions (x:drugProvider.Topic) =
        DrugActions(Id(x.Id), allsections x |> Array.map DrugAction)
      static member sideEffects (x:drugProvider.Topic) =
        let gse = x |> (somesections "generalSideEffects")
                    |> Array.filter (hasOutputclasso "frequencies")
                    |> Array.map Frequency.fromge
        let sse = x |> (somesections "specificSideEffects")
                    |> Array.filter (hasOutputclasso "frequencies")
                    |> Array.map Frequency.fromsp
        let adv = x |> (somesections "sideEffectsAdvice")
                    |> Array.map SideEffectAdvice.from
        SideEffects(Id(x.Id), Array.concat [gse;sse] ,adv) 
      static member contraindications (x:drugProvider.Topic) =
        match x.Body with
          | Some b ->
             let cs = b.Sections |> Array.choose (withclass "contraindications")
             let ias = b.Sections
                      |> (sectiondivs "importantAdvice")
                      |> Array.map (addSpecificity >> addTitle >> ImportantAdvice) 
             Contraindications(
               Id(x.Id),
               cs |> Array.collect Contraindication.from,
               cs |> Array.collect Contraindication.content,
               ias)
          | None -> Contraindications(Id(x.Id), Array.empty<Contraindication>, Array.empty<drugProvider.P>,Array.empty<ImportantAdvice>)
      static member cautions (x:drugProvider.Topic) =
        let s = firstsection (withclass "cautions") x 
        let cgs = match s with 
                   | Some (s) -> CautionsGroup.from s |> Array.toList
                   | None -> List.empty<CautionsGroup>
        let ias = x |> (somesections "importantAdvice") |> Array.map (addSpecificity >> addTitle >> ImportantAdvice)
        Cautions(Id(x.Id), cgs, ias)
      static member prescribingAndDispensingInformation (x:drugProvider.Topic) =
        PrescribingAndDispensingInformations(Id(x.Id), allsections x |> Array.map (addSpecificity >> PrescribingAndDispensingInformation))
      static member unlicencedUse (x:drugProvider.Topic) =
        UnlicencedUses(Id(x.Id), allsections x |> Array.map (addSpecificity >> UnlicencedUse))
      static member monitoringRequirements (x:drugProvider.Topic) =
        match x.Body with
          | Some b -> MonitoringRequirements(Id(x.Id),b.Sections |> Array.collect MonitoringRequirement.from)
          | None -> MonitoringRequirements(Id(x.Id), Array.empty<MonitoringRequirement>)
      static member conceptionAndContraception (x:drugProvider.Topic) =
        ConceptionAndContraceptions(Id(x.Id), allsections x |> Array.map (addSpecificity >> ConceptionAndContraception))
      static member importantSafetyInformation (x:drugProvider.Topic) =
        ImportantSafetyInformations(Id(x.Id), allsections x |> Array.map (addSpecificity >> addTitle >> ImportantSafetyInformation))

      static member directionsForAdministration (x:drugProvider.Topic) =
        DirectionsForAdministrations(Id(x.Id), allsections x |> Array.map (addSpecificity >> DirectionsForAdministration))
      static member nationalFunding (x:drugProvider.Topic) =
        let fds =  match x.Body with
                         | Some b -> b.Sections |> Array.collect FundingDecision.from
                         | None -> [||]
        NationalFunding(Id(x.Id),fds)
      static member interactions (x:drugProvider.Topic) =
        let fs = x |> firstsection (withclass "general")
        let is = match fs with
                 | Some s -> s.Sectiondivs |> Array.map (addSpecificity >> Interaction)
                 | None -> [||]
        Interactions(Id(x.Id),is)

    type Drug with
      static member parse (x:drugProvider.Topic) =
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
                | HasOutputClass "drugAction" _ -> Some(MonographSection.drugActions x)
                | HasOutputClass "sideEffects" _ -> Some(MonographSection.sideEffects x)
                | HasOutputClass "contraindications" _ -> Some(MonographSection.contraindications x)
                | HasOutputClass "cautions" _ -> Some(MonographSection.cautions x)
                | HasOutputClass "prescribingAndDispensingInformation" _ -> Some(MonographSection.prescribingAndDispensingInformation x)
                | HasOutputClass "unlicencedUse" _ -> Some(MonographSection.unlicencedUse x)
                | HasOutputClass "monitoringRequirements" _ -> Some(MonographSection.monitoringRequirements x)
                | HasOutputClass "conceptionAndContraception" _ -> Some(MonographSection.conceptionAndContraception x)
                | HasOutputClass "importantSafetyInformation" _ -> Some(MonographSection.importantSafetyInformation x)
                | HasOutputClass "directionsForAdministration" _ -> Some(MonographSection.directionsForAdministration x)
                | HasOutputClass "nationalFunding" _ -> Some(MonographSection.nationalFunding x)
                | HasOutputClass "interactions" _ -> Some(MonographSection.interactions x)
                | _ -> None

        let sections =
          x.Topics |> Array.map sectionFn |> Array.choose id

        let primaryDomainOfEffect = x.Body >>= PrimaryDomainOfEffect.from
        let secondaryDomainsOfEffect = x.Body >>= SecondaryDomainsOfEffect.from

        {id = Id(x.Id); name = name; interactionLinks = interactionLinks; classifications = classifications; vtmid = vtmid; sections = sections; primaryDomainOfEffect = primaryDomainOfEffect; secondaryDomainsOfEffect = secondaryDomainsOfEffect}
