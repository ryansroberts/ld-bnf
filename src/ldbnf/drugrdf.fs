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
  let getlabeld (DrugName n) = string n.Value.Value
  let getvald (DrugName n) = string n
  let getlabeldc (DrugClassName n) = string n.Value.Value
  let getvaldc (DrugClassName n) = string n
  let getlabelcmpi (CMPIName n) = string n.Value.Value
  let getvalcmpi (CMPIName n) = string n
  let getvtmid (Vtmid i) = Some(string i)
  let tosys (Sys s) = s

  type Graph with
    static member setupGraph = Graph.ReallyEmpty ["nicebnf",!!Uri.nicebnf
                                                  "cnt",!!"http://www.w3.org/2011/content#"
                                                  "rdfs",!!"http://www.w3.org/2000/01/rdf-schema#"
                                                  "bnfsite",!!Uri.bnfsite]

    static member from (x:CMPI) =
      let s = [ Some(a Uri.CMPIEntity)
                Some(dataProperty !!"rdfs:label" ((getlabelcmpi x.cmpiname)^^xsd.string))
                Some(dataProperty !!"nicebnf:hasTitle" ((getvalcmpi x.cmpiname)^^xsd.xmlliteral))] |> List.choose id
      let dr r = resource (Uri.from x) r
      let sec = Graph.fromsec (Uri.fromseccmpi x)
      [dr s
       dr (x.sections |> Seq.map sec |> Seq.collect id |> Seq.toList)]
       |> Assert.graph Graph.setupGraph

    static member from (x:DrugClass) =
      let s = [ Some(a Uri.DrugClassEntity)
                Some(dataProperty !!"rdfs:label" ((getlabeldc x.dcname)^^xsd.string))
                Some(dataProperty !!"nicebnf:hasTitle" ((getvaldc x.dcname)^^xsd.xmlliteral))] |> List.choose id

      let dr r = resource (Uri.from x) r
      let sec = Graph.fromsec (Uri.fromsecdc x)

      [dr s
       dr (x.sections |> Seq.map sec |> Seq.collect id |> Seq.toList)]
       |> Assert.graph Graph.setupGraph

    static member from (x:Drug) =

      let s = [ Some(a Uri.DrugEntity)
                Some(dataProperty !!"rdfs:label" ((getlabeld x.name)^^xsd.string))
                Some(dataProperty !!"nicebnf:hasTitle" ((getvald x.name)^^xsd.xmlliteral))
                x.vtmid >>= getvtmid >>= (xsd.string >> dataProperty !!"nicebnf:hasVtmid" >> Some)
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
       dr (x.classifications |> Seq.map Graph.fromcl |> Seq.toList |> List.collect id)
       dr (x.constituentDrugs |> Seq.map Graph.fromcd |> Seq.toList)
       dr (x.interactionLinks |> Seq.map Graph.fromil |> Seq.toList)
       dr (x.sections |> Seq.map sec |> Seq.collect id |> Seq.toList)]
       |> Assert.graph Graph.setupGraph

    static member fromdc (InheritsFromClass (c)) =
      one !!"nicebnf:inheritsFromClass" (Uri.fromdc c) [a Uri.DrugClassEntity]

    //the label for this is in another part of the feed so will be created elsewhere
    static member fromcl (Classification(id,ifcs)) =
      let ifs = ifcs |> Seq.map Graph.fromdc |> Seq.toList
      let cl = one !!"nicebnf:hasClassification" (Classification(id,ifcs) |> Uri.fromc) [(a Uri.ClassificationEntity)]
      cl :: ifs

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
      let fettle (s:string) = s.Trim().ToLower()
      let s = [ Some(a Uri.DomainOfEffectEntity)
                n >>= (fettle >> xsd.string >> (dataProperty !!"rdfs:label") >> Some)
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
      let l = match x with | FundingIdentifier f -> f.Title^^xsd.string
      Some(one !!"nicebnf:hasFundingIdentifier" (Uri.fromfi x)
              [dataProperty !!"rdfs:label" l
               a Uri.FundingIdentifierEntity])

    static member from (x:PatientGroup) =
      [Some(one !!"nicebnf:hasPatientGroup" (Uri.fromgrp x.Group) [dataProperty !!"rdfs:label" (x.Group^^xsd.string)
                                                                   a Uri.PatientGroupEntity])
       Some(dataProperty !!"rdfs:label" (x.Dosage^^xsd.string))
       Some(dataProperty !!"nicebnf:hasDitaContent" ((string x.dosageXml )^^xsd.xmlliteral))
       Some(a Uri.DosageEntity)]

    static member fromti (Bnf.Drug.Title (Paragraph(s,_))) =
      Some(dataProperty !!"nicebnf:hasTitle" (s^^xsd.string))

    static member fromsp (Specificity (Paragraph(s,_),r,i)) =
      let sp = [ [ dataProperty !!"rdfs:label" (s^^xsd.string)
                   a Uri.SpecificityEntity]
                 r |> List.choose Graph.from
                 i |> List.choose Graph.from] |> List.collect id
      one !!"nicebnf:hasSpecificity" (Uri.froms s) sp

    static member fromgi (GeneralInformation (sd,sp)) =
      let s = [Some(dataProperty !!"nicebnf:hasDitaContent" (xsd.xmlliteral(sd.ToString())))
               sp >>= (Graph.fromsp >> Some)]
      s |> List.choose id

    static member fromda (DoseAdjustment (sp,sd)) =
      let s = [Some(dataProperty !!"nicebnf:hasDitaContent" (xsd.xmlliteral(sd.ToString())))
               sp >>= (Graph.fromsp >> Some)]
      s |> List.choose id

    //static member general n i (gis:seq<GeneralInformation>) =
    //  let s = a !!("nicebnf:" + n) :: (gis |> Seq.map Graph.fromgi |> Seq.toList)
    //  one !!("nicebnf:has" + n) i s

    //ungroup the patient groups adding a route if available
    static member from (RouteOfAdministration(r,pgs)) =
      let patientGrp pg = blank !!"nicebnf:hasDosage"
                            ([Some(one !!"nicebnf:hasPatientGroup" (Uri.fromgrp pg.Group) [ dataProperty !!"rdfs:label" (pg.Group^^xsd.string)
                                                                                            a Uri.PatientGroupEntity])
                              Some(dataProperty !!"rdfs:label" (pg.Dosage^^xsd.string))
                              Some(dataProperty !!"nicebnf:hasDitaContent" ((string pg.dosageXml )^^xsd.xmlliteral))
                              Some(a Uri.DosageEntity)
                              r >>= Graph.from] |> List.choose id)
      pgs |> Seq.map patientGrp

    static member from (x:TheraputicIndication) =
      match x with
        | TheraputicIndication (s,p) -> Some(one !!"nicebnf:hasIndication" (Uri.from x) [a Uri.IndicationEntity
                                                                                         dataProperty !!"rdfs:label" (s^^xsd.string)
                                                                                         dataProperty !!"nicebnf:hasDitaContent" ((string p)^^xsd.xmlliteral)])

    static member fromidg (IndicationsAndDose(tis,roas)) =
      (tis |> Seq.map Graph.from |> Seq.choose id |> Seq.toList)
              @ (roas |> Seq.collect Graph.from |> Seq.toList)

    static member fromidgs (x:IndicationsAndDoseSection) =
      let dp n s = [dataProperty !!"nicebnf:hasDitaContent" (xsd.xmlliteral(s.ToString()))
                    dataProperty !!"nicebnf:hasSubject" (n^^xsd.string)]
      match x with
       | Pharmacokinetics s -> s |> dp "Pharmacokinetics"
       | DoseEquivalence s -> s |> dp "DoseEquivalence"
       | DoseAdjustments s -> s |> dp "DoseAdjustments"
       | ExtremesOfBodyWeight s -> s |> dp "ExtremesOfBodyWeight"
       | Potency s -> s |> dp "Potency"

    static member fromamri (AdditionalMonitoringInRenalImpairment s) =
      [dataProperty !!"nicebnf:hasDitaContent" (xsd.xmlliteral(s.ToString()))]

    static member frompca (p:PatientAndCarerAdvice) =
      let pca n x = dataProperty !!"nicebnf:hasSubject" (n^^xsd.string) :: Graph.fromthree x
      match p with
        | PatientResources (t,sp,s) -> (t,sp,s) |> pca "PatientResources"
        | AdviceAroundMissedDoses (t,sp,s) -> (t,sp,s) |> pca "AdviceAroundMissedDoses"
        | GeneralPatientAdvice (t,sp,s) -> (t,sp,s) |> pca "GeneralPatientAdvice"
        | AdviceAroundDrivingAndOtherTasks (t,sp,s) -> (t,sp,s) |> pca "AdviceAroundDrivingAndOtherTasks"
        | PatientAdviceInPregnancy  (t,sp,s) -> (t,sp,s) |> pca "PatientAdviceInPregnancy"
        | PatientAdviceInConceptionAndContraception (t,sp,s) -> (t,sp,s) |> pca "PatientAdviceInConceptionAndContraception"

    static member fromlvs (LicensingVariationStatement(Html(s))) =
        [dataProperty !!"nicebnf:hasDitaContent" (xsd.xmlliteral(s.ToString()))]

    static member fromhtml (Html(s)) =
      [dataProperty !!"nicebnf:hasDitaContent" (xsd.xmlliteral(s.ToString()))]

    static member frommfl (MedicinalFormLink(l)) =
      [one !!"nicebnf:hasMedicinalForm" (!!(Uri.bnfsite + "medicinalform/" + l.Url)) [dataProperty !!"rdfs:label" (l.Title^^xsd.string)]]

    static member fromcsc (AllergyAndCrossSensitivityContraindications s) =
        [dataProperty !!"nicebnf:hasDitaContent" (xsd.xmlliteral(s.ToString()))]

    static member fromcscs (AllergyAndCrossSensitivityCrossSensitivity s) =
        [dataProperty !!"nicebnf:hasDitaContent" (xsd.xmlliteral(s.ToString()))]

    static member from (x:drugProvider.Sectiondiv) =
      dataProperty !!"nicebnf:hasDitaContent" (xsd.xmlliteral(x.ToString()))

    static member frompair (sp,s:drugProvider.Sectiondiv) =
      [sp >>= (Graph.fromsp >> Some)
       Some(Graph.from s)] |> List.choose id

    static member fromthree (t,sp,s) =
      let st = [t >>= Graph.fromti] |> List.choose id
      st @ (Graph.frompair (sp,s))

    static member fromexc (ExceptionToLegalCategory (sp,s)) = Graph.frompair (sp,s)
    static member fromden (DentalPractitionersFormulary (sp,s)) = Graph.frompair (sp,s)
    static member fromadp (AdviceForDentalPractitioners (sp,s)) = Graph.frompair (sp,s)

    static member fromlsfp (LessSuitableForPrescribing (sp,s)) = Graph.frompair (sp,s)

    static member fromhas (HandlingAndStorage (sp,s)) = Graph.frompair (sp,s)

    static member fromelt (EffectOnLaboratoryTest s) = [(Graph.from s)]
    static member frompts (PreTreatmentScreening s) = [(Graph.from s)]
    static member fromtc (TreatmentCessation s) = [(Graph.from s)]
    static member fromdac (DrugAction s) = [(Graph.from s)]

    static member fromse (x:SideEffect) =
      let l = match x with | SideEffect s -> ((string s).ToLower())^^xsd.string
      one !!"nicebnf:hasSideEffect" (Uri.fromse x) [dataProperty !!"rdfs:label" l
                                                    a !!"nicebnf:SideEffect" ]

    static member fromsea (SideEffectAdvice (sp,s)) = Graph.frompair (sp,s)

    static member fromod (SideEffectsOverdosageInformation (sp,s)) = Graph.frompair (sp,s)

    static member fromfre (x:FrequencyGroup) =
        let gf (f,p,ses) =
          let fq = [a !!"nicebnf:Frequency"
                    dataProperty !!"rdfs:label" (f.label^^xsd.string)]
          [ a !!"nicebnf:FrequencyGroup"
            one !!"nicebnf:hasFrequency" (Uri.fromfre f) fq
            dataProperty !!"nicebnf:hasDitaContent" ((string p)^^xsd.xmlliteral)] @ (ses |> Seq.map Graph.fromse |> Seq.toList)
        match x with
          | GeneralFrequency (f,p,ses) ->
            dataProperty !!"nicebnf:hasSubject" ("GeneralSideEffects"^^xsd.string) :: (gf(f,p,ses))
          | FrequencyWithRoutes (f,sp,p,ses) ->
            let s = (sp |> Graph.fromsp) :: gf(f,p,ses)
            dataProperty !!"nicebnf:hasSubject" ("SideEffectsWithRoutes"^^xsd.string) :: s
          | FrequencyWithIndications (f,sp,p,ses) ->
            let s = (sp |> Graph.fromsp) :: gf(f,p,ses)
            dataProperty !!"nicebnf:hasSubject" ("SideEffectsWithIndications"^^xsd.string) :: s

    static member fromia (ImportantAdvice (t,sp,s)) = Graph.fromthree (t,sp,s)
    static member fromciri (ContraindicationsRenalImpairment (t,sp,s)) = Graph.fromthree (t,sp,s)

    static member fromcon (x:ContraindicationsGroup) =
      let sp t = [a Uri.SpecificityEntity
                  dataProperty !!"rdfs:label" ((string t)^^xsd.string)]
      let con (Contraindication x) = dataProperty !!"nicebnf:hasContraindication" (xsd.string(x.ToString()))
      let gen (p,cs) = (dataProperty !!"nicebnf:hasDitaContent" (xsd.xmlliteral(p.ToString()))) :: (cs |> List.map con)
      match x with
        | GeneralContraindications (p,cs) -> dataProperty !!"nicebnf:hasSubject" ("GeneralContraindications"^^xsd.string)
                                               ::  (gen(p,cs))
        | ContraindicationWithRoutes (t,p,cs) -> dataProperty !!"nicebnf:hasSubject" ("ContraindicationWithRoutes"^^xsd.string)
                                                    :: ((one !!"nicebnf:hasSpecificity" (Uri.froms t) (sp t))
                                                    :: gen(p,cs))
        | ContraindicationWithIndications (t,p,cs) -> dataProperty !!"nicebnf:hasSubject" ("ContraindicationWithIndications"^^xsd.string)
                                                        :: ((one !!"nicebnf:hasSpecificity" (Uri.froms t) (sp t))
                                                        :: gen(p,cs))

    static member fromcg (x:CautionsGroup) =
      let cau (Caution x) = dataProperty !!"nicebnf:hasCaution" (xsd.string(x.ToString()))
      let gen (p,cs) = (dataProperty !!"nicebnf:hasDitaContent" (xsd.xmlliteral(p.ToString()))) :: (cs |> List.map cau)
      match x with
        | GeneralCautions (p,cs) -> dataProperty !!"nicebnf:hasSubject" ("GeneralCautions"^^xsd.string) :: (gen(p,cs))
        | CautionsWithRoutes (t,p,cs) ->
                dataProperty !!"nicebnf:hasSubject" ("CautionsWithRoutes"^^xsd.string)
                :: (one !!"nicebnf:hasSpecificity" (Uri.froms(t.ToString()))
                     [dataProperty !!"rdfs:label" (xsd.string(t.ToString()))
                      a Uri.SpecificityEntity])
                      :: gen(p,cs)
        | CautionsWithIndications (t,p,cs) ->
                dataProperty !!"nicebnf:hasSubject" ("CautionsWithIndications"^^xsd.string)
                 :: (one !!"nicebnf:hasSpecificity" (Uri.froms(t.ToString()))
                    [ dataProperty !!"rdfs:label" (xsd.string(t.ToString()))
                      a Uri.SpecificityEntity])
                      :: gen(p,cs)

    static member frompadi (PrescribingAndDispensingInformation (sp,s)) = Graph.frompair (sp,s)
    static member fromulu (UnlicencedUse (sp,s)) = Graph.frompair (sp,s)
    static member fromcac (ConceptionAndContraception (sp,s)) = Graph.frompair (sp,s)
    static member fromisi (ImportantSafetyInformation(t,sp,s)) = Graph.fromthree (t,sp,s)
    static member fromdfa (DirectionsForAdministration (sp,s))= Graph.frompair (sp,s)

    static member fromfd (x:FundingDecision) =
      match x with
        | NonNHS(sp,s) -> dataProperty !!"nicebnf:hasSubject" ("NonNHS"^^xsd.string) :: (Graph.frompair (sp,s))
        | SmcDecisions(sp,s) -> dataProperty !!"nicebnf:hasSubject" ("SmcDecisions"^^xsd.string) :: (Graph.frompair(sp,s))
        | NiceTechnologyAppraisals(fi,t,sp,s) ->
           let s = [sp >>= (Graph.fromsp >> Some)
                    Some(Graph.from s)
                    t >>= Graph.fromti
                    fi >>= Graph.from] |> List.choose id
           dataProperty !!"nicebnf:hasSubject" ("NiceTechnologyAppraisals"^^xsd.string) :: s

    static member frominter (Interaction(sp,s)) = Graph.frompair(sp,s)

    static member fromamp (AdditionalMonitoringInPregnancy(sp,s)) = Graph.frompair(sp,s)

    static member fromambf (AdditionalMonitoringInBreastFeeding(sp,s)) = Graph.frompair(sp,s)

    static member fromamhi (AdditionalMonitoringInHepaticImpairment(sp,s)) = Graph.frompair(sp,s)

    static member frommon (x:MonitoringRequirement) =
      match x with
        | PatientMonitoringProgrammes (sp,s) -> dataProperty !!"nicebnf:hasSubject" ("PatientMonitoringProgrammes"^^xsd.string) :: (Graph.frompair (sp,s))
        | TheraputicDrugMonitoring (sp,s) -> dataProperty !!"nicebnf:hasSubject" ("TheraputicDrugMonitoring"^^xsd.string) :: (Graph.frompair (sp,s))
        | MonitoringOfPatientParameters (sp,s) -> dataProperty !!"nicebnf:hasSubject" ("MonitoringOfPatientParameters"^^xsd.string) :: (Graph.frompair (sp,s))


    static member bob (GeneralInformation (sd,sp)) =
      let s = [dataProperty !!"nicebnf:hasDitaContent" (xsd.xmlliteral(sd.ToString())) |> Some
               sp >>= (Graph.fromsp >> Some)]
      s |> List.choose id


    static member fromsec sid (x:MonographSection) =

      let rec applyTo a xs = 
        match xs with
         | [] -> []
         | x::xs -> x a::applyTo a xs

      //take the list, add hasSubject and type
      let add f x (sub,uri) =
        let s = f x
        let n = x.GetType().Name
        let s' = [a !!("nicebnf:" + n)
                  one !!"nicebnf:hasSubject" uri
                    [a !!("nicebnf:" + sub)]]
        blank !!("nicebnf:has" + n) (s @ s')

      //let sec n i st =
      //  let s =  a !!("nicebnf:" + n) :: (st |> List.collect id)
      //  one !!("nicebnf:has" + n) i s

      let inline sec n i stf =
        let fs = stf |> List.collect id
        fs |> applyTo (n,i)

      //let inline statments g x = x |> Seq.map g |> Seq.toList

      let inline statements g x = x |> Seq.map (add g) |> Seq.toList

      let inline statement g x =
        match x with
        | Some(x) -> [x |> add g]
        | None -> []

      match x with
        | Pregnancy (i,gs,das,amps) -> sec "PregnancyWarning" (sid i) [statements Graph.fromgi gs
                                                                       statements Graph.fromda das
                                                                       statements Graph.fromamp amps]
        | BreastFeeding (i,gs,ambfs,das) -> sec "BreastFeedingWarning" (sid i) [statements Graph.fromgi gs
                                                                                statements Graph.fromambf ambfs
                                                                                statements Graph.fromda das]
        | HepaticImpairment (i,gs,das,amhis) -> sec "HepaticImpairmentWarning" (sid i) [statements Graph.fromgi gs
                                                                                        statements Graph.fromda das
                                                                                        statements Graph.fromamhi amhis]
        | RenalImpairment (i,gs,amri,das) -> sec "RenalImpairmentWarning" (sid i) [statements Graph.fromgi gs
                                                                                   statements Graph.fromamri amri
                                                                                   statements Graph.fromda das]
        | IndicationsAndDoseGroup (i,g,gss) -> sec "IndicationAndDosageInformation" (sid i) [statements Graph.fromidg g
                                                                                             statements Graph.fromidgs gss]
        | PatientAndCarerAdvices (i, pcas) -> sec "PatientAndCarerAdvice" (sid i) [statements Graph.frompca pcas]
        | MedicinalForms (i,lvs,html,mfls) -> sec "MedicinalFormInformation" (sid i) [ statement Graph.fromlvs lvs
                                                                                       statement Graph.fromhtml html
                                                                                       statements Graph.frommfl mfls]
        | AllergyAndCrossSensitivity (i,csc,cscs) -> sec "AllergyAndCrossSensitivityWarning" (sid i) [ statement Graph.fromcsc csc
                                                                                                       statement Graph.fromcscs cscs]
        | ExceptionsToLegalCategory (i,es) -> sec "ExceptionsToLegalCategory" (sid i) [statements Graph.fromexc es]
        | ProfessionSpecificInformation (i,dps,adps) -> sec "ProfessionSpecificInformation" (sid i) [statements Graph.fromden dps
                                                                                                     statements Graph.fromadp adps]
        | EffectOnLaboratoryTests (i,elts) -> sec "EffectOnLaboratoryTests" (sid i) [statements Graph.fromelt elts]
        | PreTreatmentScreenings (i,ptss) -> sec "PreTreatmentScreeningInformation" (sid i) [statements Graph.frompts ptss]
        | LessSuitableForPrescribings (i,lsfps) -> sec "LessSuitableForPrescribing" (sid i) [statements Graph.fromlsfp lsfps]
        | HandlingAndStorages (i,hass) -> sec "HandlingAndStorageInformation" (sid i) [statements Graph.fromhas hass]
        | TreatmentCessations (i,tcs) -> sec "TreatmentCessationInformation" (sid i) [statements Graph.fromtc tcs]
        | DrugActions (i,das) -> sec "DrugActions" (sid i) [statements Graph.fromdac das]
        | SideEffects (i,fres,seas,ods) -> sec "SideEffects" (sid i) [statements Graph.fromfre fres
                                                                      statements Graph.fromsea seas
                                                                      statements Graph.fromod ods]
        | Contraindications (i,cogs,ias,ciri) -> sec "ContraIndications" (sid i) [statements Graph.fromcon cogs
                                                                                  statements Graph.fromia ias
                                                                                  statements Graph.fromciri ciri]
        | Cautions (i,cgs,ias) -> sec "Cautions" (sid i) [statements Graph.fromcg cgs
                                                          statements Graph.fromia ias]
        | PrescribingAndDispensingInformations (i,padi) -> sec "PrescribingAndDispensingInformation" (sid i) [statements Graph.frompadi padi]
        | UnlicencedUses (i,ulus) -> sec "UnlicencedUsageInformation" (sid i) [statements Graph.fromulu ulus]
        | ConceptionAndContraceptions (i,cacs) -> sec "ConceptionAndContraceptionWarning" (sid i) [statements Graph.fromcac cacs]
        | ImportantSafetyInformations (i,isis) -> sec "ImportantSafetyInformation" (sid i) [statements Graph.fromisi isis]
        | DirectionsForAdministrations (i,dfas) -> sec "DirectionsForAdministration" (sid i) [statements Graph.fromdfa dfas]
        | NationalFunding (i,fds) -> sec "NationalFunding" (sid i) [statements Graph.fromfd fds]
        | Interactions (i,is) -> sec "Interactions" (sid i) [statements Graph.frominter is]
        | MonitoringRequirements (i,mons) -> sec "MonitoringRequirements" (sid i) [statements Graph.frommon mons]
