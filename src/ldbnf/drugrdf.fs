namespace Bnf
open FSharp.RDF
open FSharp.Data.Runtime

module DrugRdf =
  open prelude
  open resource
  open Bnf.Drug
  open Bnf.MedicinalForm
  open Assertion
  open rdf
  open Shared
  open Rdf
  open RdfUris

  //shoudl replace these with tostring
  let getvald (DrugName n) = n
  let getvaldc (DrugClassName n) = n
  let getvalcmpi (CMPIName n) = n
  let getvtmid (Vtmid i) = Some(string i)
  let tosys (Sys s) = s

  type Graph with
    static member setupGraph = Graph.ReallyEmpty ["nicebnf",!!Uri.nicebnf
                                                  "cnt",!!"http://www.w3.org/2011/content#"
                                                  "rdfs",!!"http://www.w3.org/2000/01/rdf-schema#"
                                                  "bnfsite",!!Uri.bnfsite]

    static member from (x:CMPI) =
      let s = [ Some(a Uri.CMPIEntity)
                Some(dataProperty !!"rdfs:label" ((getvalcmpi x.cmpiname)^^xsd.string))] |> List.choose id
      let dr r = resource (Uri.from x) r
      let sec = Graph.fromsec (Uri.fromseccmpi x)
      [dr s
       dr (x.sections |> Seq.map sec |> Seq.choose id |> Seq.toList)]
       |> Assert.graph Graph.setupGraph

    static member from (x:DrugClass) =

      let s = [ Some(a Uri.DrugClassEntity)
                Some(dataProperty !!"rdfs:label" ((getvaldc x.dcname)^^xsd.string))] |> List.choose id

      let dr r = resource (Uri.from x) r
      let sec = Graph.fromsec (Uri.fromsecdc x)

      [dr s
       dr (x.sections |> Seq.map sec |> Seq.choose id |> Seq.toList)]
       |> Assert.graph Graph.setupGraph

    static member from (x:Drug) =

      let s = [ Some(a Uri.DrugEntity)
                Some(dataProperty !!"rdfs:label" ((getvald x.name)^^xsd.string))
                x.vtmid >>= getvtmid >>= (xsd.string >> dataProperty !!"nicebnf:vtmid" >> Some)
                x.primaryDomainOfEffect >>= (Graph.frompdoe >> Some)
                ]

      let dr r = resource (Uri.from x) r
      //pass in uri construction for sections
      let sec = Graph.fromsec (Uri.fromsec x)

      let sdoe = match x.secondaryDomainsOfEffect with
                 | Some d -> Graph.fromsdoes d
                 | None -> Seq.empty<(Predicate * Object)>

      [dr (s |> List.choose id)
       dr (sdoe |> Seq.toList)
       dr (x.classifications |> Seq.map Graph.fromcl |> Seq.toList)
       dr (x.constituentDrugs |> Seq.map Graph.fromcd |> Seq.toList)
       dr (x.interactionLinks |> Seq.map Graph.fromil |> Seq.toList)
       dr (x.sections |> Seq.map sec |> Seq.choose id |> Seq.toList)]
       |> Assert.graph Graph.setupGraph

    static member fromdc (InheritsFromClass (c)) =
      one !!"nicebnf:inheritsFromClass" (Uri.fromdc c) [a Uri.DrugClassEntity]

    static member fromc (Classification (_,is)) =
      [(a Uri.ClassificationEntity)] @ (is |> Seq.map Graph.fromdc |> Seq.toList)

    //the label for this is in another part of the feed so will be created elsewhere
    static member fromcl (c:Classification) =
      one !!"nicebnf:hasClassification" (Uri.fromc c) (Graph.fromc c)

    static member fromil (i:InteractionLink) =
      one !!"nicebnf:hasInteraction" (Uri.from i) [a Uri.InteractionEntity]

    static member fromcd (x:ConstituentDrug) =
      one !!"nicebnf:hasConstituentDrug" (Uri.from x )
       [a Uri.ConstituentDrugEntity
        dataProperty !!"rdfs:label" ((string x)^^xsd.string)]

    static member fromtu ((x:TheraputicUse), ?name0:string) =
      let name = defaultArg name0 "nicebnf:hasTherapeuticUse"
      let s = match x with | TheraputicUse(n,u) ->
                               [Some(a Uri.TheraputicUseEntity)
                                Some(dataProperty !!"rdfs:label" (n^^xsd.string))
                                u >>= (Graph.fromtu >> Some)]
      one !!name (Uri.from x) (s |> List.choose id)

    static member fromptu (PrimaryTheraputicUse t) =
      match t with
        | Some x -> Some(Graph.fromtu (x,"nicebnf:hasPrimaryTherapeuticUse"))
        | None -> None

    static member fromstu (SecondaryTheraputicUses t) =
      match t with
        | Some x -> Some(Graph.fromtu (x,"nicebnf:hasSecondaryTherapeuticUses"))
        | None -> None

    static member fromdoe (DomainOfEffect (n,p,s)) =
      let s = [ Some(a Uri.DomainOfEffectEntity)
                n >>= (xsd.string >> (dataProperty !!"rdfs:label") >> Some)
                p >>= Graph.fromptu
                s >>= Graph.fromstu]
      s |> List.choose id

    static member hasdoe p (d:DomainOfEffect) =
      one !!("nicebnf:has" + p) (Uri.from d) (Graph.fromdoe d)

    static member frompdoe (PrimaryDomainOfEffect d) =
      d |> (Graph.hasdoe "PrimaryDomainOfEffect")

    static member fromsdoes (SecondaryDomainsOfEffect ds) =
      ds |> Seq.map (Graph.hasdoe "SecondaryDomainOfEffect")

    static member from (x:Route) =
      let l = match x with | Route r -> r^^xsd.string
      Some(one !!"nicebnf:hasRoute" (Uri.from x)
            [dataProperty !!"rdfs:label" l
             a Uri.RouteEntity])

    static member from (x:Indication) =
      let l = match x with | Indication i -> i^^xsd.string
      Some(one !!"nicebnf:hasIndication" (Uri.from x)
            [dataProperty !!"rdfs:label" l
             a Uri.IndicationEntity ])

    static member from (x:FundingIdentifier) =
      let l = match x with | FundingIdentifier f -> f^^xsd.string
      Some(one !!"nicebnf:hasFundingIdentifier" (Uri.fromfi x)
              [dataProperty !!"rdfs:label" l
               a Uri.FundingIdentifierEntity])

    static member from (x:PatientGroup) =
      [ Some(one !!"nicebnf:hasPatientGroup" (Uri.fromgrp x.Group)  [ dataProperty !!"rdfs:label" (x.Group^^xsd.string)
                                                                      a Uri.PatientGroupEntity
                                                                      ])
        Some(dataProperty !!"nicebnf:hasDosage" (x.Dosage^^xsd.string))
        ]

    static member fromti (Bnf.Drug.Title (Paragraph(s))) =
      Some(dataProperty !!"nicebnf:hasTitle" (s^^xsd.string))

    static member fromsp (Specificity (Paragraph s,r,i)) =
      let sp = [ [ dataProperty !!"rdfs:label" (s^^xsd.string)
                   a Uri.SpecificityEntity]
                 r |> List.choose Graph.from
                 i |> List.choose Graph.from] |> List.collect id
      one !!"nicebnf:hasSpecificity" (Uri.froms s) sp

    static member fromgi (GeneralInformation (sd,sp)) =
      let s = [Some(dataProperty !!"nicebnf:hasDitaContent" (xsd.xmlliteral(sd.ToString())))
               sp >>= (Graph.fromsp >> Some)]
      blank !!"nicebnf:hasGeneralInformation" (s |> List.choose id)

    static member fromda (DoseAdjustment (sd,sp)) =
      let s = [Some(dataProperty !!"nicebnf:hasDitaContent" (xsd.xmlliteral(sd.ToString())))
               sp >>= (Graph.fromsp >> Some)]
      blank !!"nicebnf:hasDoseAdjustment" (s |> List.choose id)

    static member general n i (gis:seq<GeneralInformation>) =
      let s = a !!("nicebnf:" + n) :: (gis |> Seq.map Graph.fromgi |> Seq.toList)
      one !!("nicebnf:has" + n) i s

    //ungroup the patient groups adding a route if available
    static member from (RouteOfAdministration(r,pgs)) =
      let patientGrp pg = blank !!"nicebnf:hasRouteOfAdministration"
                            ([Some(one !!"nicebnf:hasPatientGroup" (Uri.fromgrp pg.Group) [ dataProperty !!"rdfs:label" (pg.Group^^xsd.string)
                                                                                            a Uri.PatientGroupEntity
                                                                                            ])
                              Some(dataProperty !!"nicebnf:hasDosage" (pg.Dosage^^xsd.string))
                              r >>= Graph.from] |> List.choose id)
      pgs |> Seq.map patientGrp

    static member from (TheraputicIndication s) =
      Some(dataProperty !!"nicebnf:hasTheraputicIndication" (s^^xsd.string))

    static member fromidg (IndicationsAndDose(tis,roas)) =
      let s = (tis |> Seq.map Graph.from |> Seq.choose id |> Seq.toList)
              @ (roas |> Seq.collect Graph.from |> Seq.toList)
      blank !!"nicebnf:hasIndicationAndDose" s

    static member fromidgs (x:IndicationsAndDoseSection) =
      let bn s = [dataProperty !!"nicebnf:hasDitaContent" (xsd.xmlliteral(s.ToString()))]
      let dp n s = (blank !!("nicebnf:has" + n) (bn s))
      match x with
       | Pharmacokinetics s -> s |> dp "Pharmacokinetics"
       | DoseEquivalence s -> s |> dp "DoseEquivalence"
       | DoseAdjustments s -> s |> dp "DoseAdjustments"
       | ExtremesOfBodyWeight s -> s |> dp "ExtremesOfBodyWeight"
       | Potency s -> s |> dp "Potency"

    static member fromamri (AdditionalMonitoringInRenalImpairment s) =
      blank !!"nicebnf:hasAdditionalMonitoringInRenalImpairment"
        [dataProperty !!"nicebnf:hasDitaContent" (xsd.xmlliteral(s.ToString()))]

    static member frompca (p:PatientAndCarerAdvice) =
      let pca n x = blank !!("nicebnf:has" + n) (Graph.fromthree x)
      match p with
        | PatientResources (t,sp,s) -> (t,sp,s) |> pca "PatientResources"
        | AdviceAroundMissedDoses (t,sp,s) -> (t,sp,s) |> pca "AdviceAroundMissedDoses"
        | GeneralPatientAdvice (t,sp,s) -> (t,sp,s) |> pca "GeneralPatientAdvice"
        | AdviceAroundDrivingAndOtherTasks (t,sp,s) -> (t,sp,s) |> pca "AdviceAroundDrivingAndOtherTasks"
        | PatientAdviceInPregnancy  (t,sp,s) -> (t,sp,s) |> pca "PatientAdviceInPregnancy"
        | PatientAdviceInConceptionAndContraception (t,sp,s) -> (t,sp,s) |> pca "PatientAdviceInConceptionAndContraception"

    static member fromlvs (LicensingVariationStatement(Html(s))) =
      blank !!"nicebnf:hasLicensingVariationStatement"
        [dataProperty !!"nicebnf:hasDitaContent" (xsd.xmlliteral(s.ToString()))]

    static member fromhtml (Html(s)) =
      dataProperty !!"nicebnf:hasDitaContent" (xsd.xmlliteral(s.ToString()))

    static member frommfl (MedicinalFormLink(l)) =
      blank !!"nicebnf:hasMedicinalForm"
        [dataProperty !!"rdfs:label" (l.Title^^xsd.string)
         dataProperty !!"nicebnf:medicinalForm" ((Uri.bnfsite + "medicinalform/" + l.Url.[1..])^^xsd.string)]

    static member fromcsc (AllergyAndCrossSensitivityContraindications s) =
      blank !!"nicebnf:hasContraIndication"
        [dataProperty !!"nicebnf:hasDitaContent" (xsd.xmlliteral(s.ToString()))]

    static member fromcscs (AllergyAndCrossSensitivityCrossSensitivity s) =
      blank !!"nicebnf:hasCrossSensitivity"
        [dataProperty !!"nicebnf:hasDitaContent" (xsd.xmlliteral(s.ToString()))]

    static member from (x:drugProvider.Sectiondiv) =
      dataProperty !!"nicebnf:hasDitaContent" (xsd.xmlliteral(x.ToString()))

    static member frompair (sp,s:drugProvider.Sectiondiv) =
      [sp >>= (Graph.fromsp >> Some)
       Some(Graph.from s)] |> List.choose id

    static member fromthree (t,sp,s) =
      let st = [t >>= Graph.fromti] |> List.choose id
      st @ (Graph.frompair (sp,s))

    static member fromexc (ExceptionToLegalCategory (sp,s)) =
      blank !!"nicebnf:hasException" (Graph.frompair (sp,s))

    static member fromden (DentalPractitionersFormulary (sp,s)) =
      blank !!"nicebnf:hasDentalPractitionersFormulary" (Graph.frompair (sp,s))
    static member fromadp (AdviceForDentalPractitioners (sp,s)) =
      blank !!"nicebnf:hasAdviceForDentalPractitioners" (Graph.frompair (sp,s))

    static member fromlsfp (LessSuitableForPrescribing (sp,s)) =
      blank !!"nicebnf:hasInformation" (Graph.frompair (sp,s))

    static member fromhas (HandlingAndStorage (sp,s)) =
      blank !!"nicebnf:hasInformation" (Graph.frompair (sp,s))

    static member fromelt (EffectOnLaboratoryTest s) =
      blank !!"nicebnf:hasInformation" [(Graph.from s)]
    static member frompts (PreTreatmentScreening s) =
      blank !!"nicebnf:hasInformation" [(Graph.from s)]
    static member fromtc (TreatmentCessation s) =
      blank !!"nicebnf:hasInformation" [(Graph.from s)]
    static member fromdac (DrugAction s) =
      blank !!"nicebnf:hasInformation" [(Graph.from s)]

    static member fromse (x:SideEffect) =
      let l = match x with | SideEffect s -> ((string s).ToLower())^^xsd.string
      one !!"nicebnf:hasSideEffect" (Uri.fromse x) [ dataProperty !!"rdfs:label" l
                                                     a !!"nicebnf:SideEffect" ]

    static member fromsea (SideEffectAdvice (sp,s)) =
      blank !!"nicebnf:hasSideEffectAdvice" (Graph.frompair (sp,s))

    static member fromod (SideEffectsOverdosageInformation (sp,s)) =
      blank !!"nicebnf:hasSideEffectsOverdosageInformation" (Graph.frompair (sp,s))

    static member fromfre (x:FrequencyGroup) =
        let gf (f,p,ses) =
          let fq = [a !!"nicebnf:Frequency"
                    dataProperty !!"rdfs:label" (f.label^^xsd.string)]
          [ a !!"nicebnf:FrequencyGroup"
            one !!"nicebnf:hasFrequency" (Uri.fromfre f) fq
            dataProperty !!"nicebnf:hasDitaContent" ((string p)^^xsd.xmlliteral)] @ (ses |> Seq.map Graph.fromse |> Seq.toList)
        match x with
          | GeneralFrequency (f,p,ses) ->
            blank !!"nicebnf:hasGeneralSideEffects" (gf(f,p,ses))
          | FrequencyWithRoutes (f,r,p,ses) ->
            let s = match (r |> Graph.from) with
                    | Some(r) -> r :: gf(f,p,ses)
                    | None -> gf(f,p,ses)
            blank !!"nicebnf:hasSideEffectsWithRoutes" s
          | FrequencyWithIndications (f,i,p,ses) ->
            let s = match (i |> Graph.from) with
                     | Some(i) -> i :: gf(f,p,ses)
                     | None -> gf(f,p,ses)
            blank !!"nicebnf:hasSideEffectsWithIndications" s

    static member fromia (ImportantAdvice (t,sp,s)) =
      blank !!"nicebnf:hasImportantAdvice" (Graph.fromthree (t,sp,s))

    static member fromcon (x:ContraindicationsGroup) =
      let con (Contraindication x) = dataProperty !!"nicebnf:hasContraindication" (xsd.string(x.ToString()))
      let gen (p,cs) = (dataProperty !!"nicebnf:hasDitaContent" (xsd.xmlliteral(p.ToString()))) :: (cs |> List.map con)
      match x with
        | GeneralContraindications (p,cs) -> blank !!"nicebnf:hasGeneralContraindications" (gen(p,cs))
        | ContraindicationWithRoutes (t,p,cs) -> blank !!"nicebnf:hasContraindicationsWithRoutes"
                                                   (dataProperty !!"nicebnf:hasRouteAsTitle" (xsd.string(t.ToString()))
                                                    :: gen(p,cs))
        | ContraindicationWithIndications (t,p,cs) -> blank !!"nicebnf:hasContraindicationsWithIndications"
                                                       (dataProperty !!"nicebnf:hasIndicationAsTitle" (xsd.string(t.ToString()))
                                                        :: gen(p,cs))

    static member fromcg (x:CautionsGroup) =
      let cau (Caution x) = dataProperty !!"nicebnf:hasCaution" (xsd.string(x.ToString()))
      let gen (p,cs) = (dataProperty !!"nicebnf:hasDitaContent" (xsd.xmlliteral(p.ToString()))) :: (cs |> List.map cau)
      match x with
        | GeneralCautions (p,cs) -> blank !!"nicebnf:hasGeneralCautions" (gen(p,cs))
        | CautionsWithRoutes (t,p,cs) -> blank !!"nicebnf:hasCautionsWithRoutes"
                                          (dataProperty !!"nicebnf:hasRouteAsTitle" (xsd.string(t.ToString()))
                                           :: gen(p,cs))
        | CautionsWithIndications (t,p,cs) -> blank !!"nicebnf:hasCautionsWithIndications"
                                               (dataProperty !!"nicebnf:hasIndicationAsTitle" (xsd.string(t.ToString()))
                                                :: gen(p,cs))

    static member frompadi (PrescribingAndDispensingInformation (sp,s)) =
      blank !!"nicebnf:hasInformation" (Graph.frompair (sp,s))
    static member fromulu (UnlicencedUse (sp,s)) =
      blank !!"nicebnf:hasInformation" (Graph.frompair (sp,s))
    static member fromcac (ConceptionAndContraception (sp,s)) =
      blank !!"nicebnf:hasInformation" (Graph.frompair (sp,s))
    static member fromisi (ImportantSafetyInformation(t,sp,s)) =
      blank !!"nicebnf:hasInformation" (Graph.fromthree (t,sp,s))
    static member fromdfa (DirectionsForAdministration (sp,s))=
      blank !!"nicebnf:hasInformation" (Graph.frompair (sp,s))

    static member fromfd (x:FundingDecision) =
      match x with
        | NonNHS(sp,s) -> blank !!"nicebnf:hasNonNHSDecision" (Graph.frompair (sp,s))
        | SmcDecisions(sp,s) -> blank !!"nicebnf:hasSmcDecision" (Graph.frompair(sp,s))
        | NiceTechnologyAppraisals(fi,t,sp,s) ->
           let s = [sp >>= (Graph.fromsp >> Some)
                    Some(Graph.from s)
                    t >>= Graph.fromti
                    fi >>= Graph.from] |> List.choose id
           blank !!"nicebnf:hasNiceTechnologyAppraisalDecision" s

    static member frominter (Interaction(sp,s)) =
      blank !!"nicebnf:hasInformation" (Graph.frompair(sp,s))

    static member fromamp (AdditionalMonitoringInPregnancy(sp,s)) =
      blank !!"nicebnf:hasAdditionalMonitoringInPregnancy" (Graph.frompair(sp,s))

    static member fromambf (AdditionalMonitoringInBreastFeeding(sp,s)) =
      blank !!"nicebnf:hasAdditionalMonitoringInBreastFeeding" (Graph.frompair(sp,s))

    static member fromamhi (AdditionalMonitoringInHepaticImpairment(sp,s)) =
      blank !!"nicebnf:hasAdditionalMonitoringInHepaticImpairment" (Graph.frompair(sp,s))

    static member frommon (x:MonitoringRequirement) =
      match x with
        | PatientMonitoringProgrammes (sp,s) -> blank !!"nicebnf:hasPatientMonitoringProgrammes" (Graph.frompair (sp,s))
        | TheraputicDrugMonitoring (sp,s) -> blank !!"nicebnf:hasTheraputicDrugMonitoring" (Graph.frompair (sp,s))
        | MonitoringOfPatientParameters (sp,s) -> blank !!"nicebnf:hasMonitoringOfPatientParameters" (Graph.frompair (sp,s))

    static member fromsec sid (x:MonographSection) =

      let sec n i st =
        let s =  a !!("nicebnf:" + n) :: (st |> List.collect id)
        one !!("nicebnf:has" + n) i s

      let inline statments g x = x |> Seq.map g |> Seq.toList
      let inline statment g x =
        match x with
        | Some(x) -> [g x]
        | None -> []

      let xml x = dataProperty !!"nicebnf:hasDitaContent" (xsd.xmlliteral(x.ToString()))

      match x with
        | Pregnancy (i,gs,das,amps) -> Some(sec "PregnancyWarning" (sid i) [statments Graph.fromgi gs
                                                                            statments Graph.fromda das
                                                                            statments Graph.fromamp amps])
        | BreastFeeding (i,gs,ambfs) -> Some(sec "BreastFeedingWarning" (sid i) [statments Graph.fromgi gs
                                                                                 statments Graph.fromambf ambfs])
        | HepaticImpairment (i,gs,das,amhis) -> Some(sec "HepaticImpairmentWarning" (sid i) [statments Graph.fromgi gs
                                                                                             statments Graph.fromda das
                                                                                             statments Graph.fromamhi amhis])
        | RenalImpairment (i,gs,amri,das) -> Some(sec "RenalImpairmentWarning" (sid i) [statments Graph.fromgi gs
                                                                                        statments Graph.fromamri amri
                                                                                        statments Graph.fromda das])
        | IndicationsAndDoseGroup (i,g,gss) -> Some(sec "IndicationAndDosageInformation" (sid i) [statments Graph.fromidg g
                                                                                                  statments Graph.fromidgs gss])
        | PatientAndCarerAdvices (i, pcas) -> Some(sec "PatientAndCarerAdvice" (sid i) [statments Graph.frompca pcas])
        | MedicinalForms (i,lvs,html,mfls) -> Some(sec "MedicinalFormInformation" (sid i) [ statment Graph.fromlvs lvs
                                                                                            statment Graph.fromhtml html
                                                                                            statments Graph.frommfl mfls])
        | AllergyAndCrossSensitivity (i,csc,cscs) -> Some(sec "AllergyAndCrossSensitivityWarning" (sid i) [ statment Graph.fromcsc csc
                                                                                                            statment Graph.fromcscs cscs])
        | ExceptionsToLegalCategory (i,es) -> Some(sec "ExceptionsToLegalCategory" (sid i) [statments Graph.fromexc es])
        | ProfessionSpecificInformation (i,dps,adps) -> Some(sec "ProfessionSpecificInformation" (sid i) [statments Graph.fromden dps
                                                                                                          statments Graph.fromadp adps])
        | EffectOnLaboratoryTests (i,elts) -> Some(sec "EffectOnLaboratoryTests" (sid i) [statments Graph.fromelt elts])
        | PreTreatmentScreenings (i,ptss) -> Some(sec "PreTreatmentScreeningInformation" (sid i) [statments Graph.frompts ptss])
        | LessSuitableForPrescribings (i,lsfps) -> Some(sec "LessSuitableForPrescribing" (sid i) [statments Graph.fromlsfp lsfps])
        | HandlingAndStorages (i,hass) -> Some(sec "HandlingAndStorageInformation" (sid i) [statments Graph.fromhas hass])
        | TreatmentCessations (i,tcs) -> Some(sec "TreatmentCessationInformation" (sid i) [statments Graph.fromtc tcs])
        | DrugActions (i,das) -> Some(sec "DrugActions" (sid i) [statments Graph.fromdac das])
        | SideEffects (i,fres,seas,ods) -> Some(sec "SideEffects" (sid i) [statments Graph.fromfre fres
                                                                           statments Graph.fromsea seas
                                                                           statments Graph.fromod ods])
        | Contraindications (i,cogs, ias) -> Some(sec "ContraIndications" (sid i) [statments Graph.fromcon cogs
                                                                                   statments Graph.fromia ias])
        | Cautions (i,cgs,ias) -> Some(sec "Cautions" (sid i) [statments Graph.fromcg cgs
                                                               statments Graph.fromia ias])
        | PrescribingAndDispensingInformations (i,padi) -> Some(sec "PrescribingAndDispensingInformation" (sid i) [statments Graph.frompadi padi])
        | UnlicencedUses (i,ulus) -> Some(sec "UnlicencedUsageInformation" (sid i) [statments Graph.fromulu ulus])
        | ConceptionAndContraceptions (i,cacs) -> Some(sec "ConceptionAndContraceptionWarning" (sid i) [statments Graph.fromcac cacs])
        | ImportantSafetyInformations (i,isis) -> Some(sec "ImportantSafetyInformation" (sid i) [statments Graph.fromisi isis])
        | DirectionsForAdministrations (i,dfas) -> Some(sec "DirectionsForAdministration" (sid i) [statments Graph.fromdfa dfas])
        | NationalFunding (i,fds) -> Some(sec "NationalFunding" (sid i) [statments Graph.fromfd fds])
        | Interactions (i,is) -> Some(sec "Interactions" (sid i) [statments Graph.frominter is])
        | MonitoringRequirements (i,mons) -> Some(sec "MonitoringRequirements" (sid i) [statments Graph.frommon mons])
