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

  let getval (DrugName n) = n
  let getvtmid (Vtmid i) = Some(string i)
  let tosys (Sys s) = s

  type Uri with
    static member nicebnf = "http://ld.nice.org.uk/ns/bnf#"
    static member nicebnfClass = "http://ld.nice.org.uk/ns/bnf/"
    static member bnfsite = "http://bnf.nice.org.uk/"

    static member from (x:Drug) = !!(Uri.bnfsite + "drug/" + string x.id )
    static member fromsec (x:Drug) (Id i) = !!(Uri.bnfsite + "drug/" + string x.id + "#" + i)
    static member from (x:MedicinalForm) = !!(Uri.bnfsite + "medicinalform/" + string x.id )
    static member from (x:MedicinalProduct) = !!(Uri.bnfsite + "medicinalproduct/" + string x.ampid)
    static member fromdc (s:string) = !!(Uri.bnfsite + "drugclass/"  + s)
    static member from (InteractionLink (l)) = !!(Uri.bnfsite + "interactions/" + l.Url)

    static member from (Route s) = !!(Uri.nicebnfClass + "Route#" + (NameUtils.niceCamelName s))
    static member from (Indication s) = !!(Uri.nicebnfClass + "Indication#" + (NameUtils.niceCamelName s))
    static member fromc (Classification (Id s,_)) = !!(Uri.nicebnfClass + "Classification#" + s)
    static member fromfi (FundingIdentifier s) = !!(Uri.nicebnfClass + "FundingIdentifier#" + s)
    static member fromgrp (s:string) = !!(Uri.nicebnfClass + "PatientGroup#" + (NameUtils.niceCamelName s))
    static member from (TheraputicUse (s,_)) = !!(Uri.nicebnfClass + "TheraputicUse#" + (NameUtils.niceCamelName s))
    static member from (DomainOfEffect (s,_,_)) = !!(Uri.nicebnfClass + "DomainOfEffect#" + (NameUtils.niceCamelName (s.Value.Trim())))
    static member fromse (SideEffect s) = !!(Uri.nicebnfClass + "SideEffect#" + (NameUtils.niceCamelName s))
    static member fromfre s = !!(Uri.nicebnfClass + "Frequency#" + (NameUtils.niceCamelName s))

    static member DrugClassEntity = !!(Uri.nicebnf + "DrugClass")
    static member DrugEntity = !!(Uri.nicebnf + "Drug")
    static member MedicinalFormEntity = !!(Uri.nicebnf + "MedicinalForm")
    static member MedicinalProductEntity = !!(Uri.nicebnf + "MedicinalProduct")
    static member InteractionEntity = !!(Uri.nicebnf + "Interaction")
    static member ClassificationEntity = !!(Uri.nicebnfClass + "Classification")
    static member RouteEntity = !!(Uri.nicebnfClass + "Route")
    static member DomainOfEffectEntity = !!(Uri.nicebnfClass + "DomainOfEffect")
    static member TheraputicUseEntity = !!(Uri.nicebnfClass + "TheraputicUse")
    static member IndicationEntity = !!(Uri.nicebnfClass + "Indication")
    static member FundingIdentifierEntity = !!(Uri.nicebnfClass + "FundingIdentifier")
    static member PatientGroupEntity = !!(Uri.nicebnfClass + "PatientGroup")

  type Graph with
      static member ReallyEmpty xp =
        let vds = new VDS.RDF.Graph()
        xp |> List.iter (fun (p, (Uri.Sys ns)) -> vds.NamespaceMap.AddNamespace(p, ns))
        Graph vds


  type Graph with
    static member fromcal (CautionaryAdvisoryLabel(ln,p)) =
      let s = [Some(dataProperty !!"cnt:ContentAsXML" ((string p)^^xsd.xmlliteral))
               ln >>= (string >> xsd.string >> (dataProperty !!"nicebnf:hasLabelNumber") >> Some)]
               |> List.choose id
      Some(blank !!"nicebnf:hasCautionaryAdvisoryLabel" s)

    static member fromcals (CautionaryAdvisoryLabels(t,cals)) =
      let c = cals |> Array.map Graph.fromcal |> Array.toList
      let s = ((t >>= (string >> xsd.string >> (dataProperty !!"rdfs:label") >> Some)) :: c)
               |> List.choose id
      blank !!"nicebnf:hasCautionaryAdvisoryLabels" s

    static member dp n = xsd.string >> (dataProperty !!("nicebnf:has" + n))

    static member fromman (Manufacturer x) = Graph.dp "Manufacturer" x |> Some
    static member frombt (BlackTriangle x) = Graph.dp "BlackTriangle" x |> Some
    static member frommpt (MedicinalProductTitle(m,bt,t)) =
      let tc = string >> xsd.string >> (dataProperty !!"cnt:ContentAsXML") >> Some
      let s = [m >>= Graph.fromman
               bt >>= Graph.frombt
               tc t] |> List.choose id
      blank !!"nicebnf:hasMedicinalProductTitle" s |> Some

    //static member fromapmid(Ampid x) =

    static member fromsai(StrengthOfActiveIngredient p) = Graph.dp "StrengthOfActiveIngredient" (string p) |> Some

    static member fromnhsi (NhsIndicative x) = Graph.dp "NhsIndicative" x |> Some
    static member frompt (PriceText x) = Graph.dp "PriceText" x |> Some
    static member fromnhsip (NhsIndicativePrice x) = Graph.dp "NhsIndicativePrice" (string x) |> Some
    static member fromnhsii (NhsIndicativeInfo(nhsi,pt,nhsip)) =
      let s = [nhsi >>= Graph.fromnhsi
               pt >>= Graph.frompt
               nhsip >>= Graph.fromnhsip] |> List.choose id
      blank !!"nicebnf:hasNhsIndicativeInfo" s |> Some

    static member fromps (PackSize d) = Graph.dp "PackSize" (string d) |> Some
    static member fromuom u = Graph.dp "UnitOfMeasure" (string u) |> Some
    static member fromlc lc = Graph.dp "LegalCategory" (string lc) |> Some
    static member frompackinfo (PackInfo(ps,uom,lc)) =
      let s = [ps >>= Graph.fromps
               uom >>= Graph.fromuom
               lc >>= Graph.fromlc] |> List.choose id
      blank !!"nicebnf:hasPackInfo" s |> Some

    static member fromdt (DrugTarrif s) = Graph.dp "DrugTarrif" s |> Some
    static member fromdtp (DrugTariffPrice dtp) = Graph.dp "DrugTariffPrice" dtp |> Some
    static member fromdti (DrugTariffInfo(dt,pt,dtp)) =
      let s = [dt >>= Graph.fromdt
               pt >>= Graph.frompt
               dtp >>= Graph.fromdtp] |> List.choose id
      blank !!"nicebnf:hasDrugTarrifInfo" s |> Some

    static member frompack(Pack(pi,nii,dti)) =
      let s = [pi >>= Graph.frompackinfo
               nii >>= Graph.fromnhsii
               dti >>= Graph.fromdti
               Some(a !!"nicebnf:Pack")] |> List.choose id
      blank !!"nicebnf:hasPack" s

    static member from (x:MedicinalProduct) =
      let s = [Some(a Uri.MedicinalProductEntity)
               Some(x.ampid |> string |> Graph.dp "Ampid")
               x.title |> Graph.frommpt
               x.strengthOfActiveIngredient >>= Graph.fromsai
               ] |> List.choose id
      let ps = x.packs |> List.map Graph.frompack
      one !!"nicebnf:hasMedicinalProduct" (Uri.from x) (s @ ps)


    static member from (x:MedicinalForm) =
      let og = Graph.ReallyEmpty ["nicebnf",!!Uri.nicebnf
                                  "cnt",!!"http://www.w3.org/2011/content#"
                                  "rdfs",!!"http://www.w3.org/2000/01/rdf-schema#"
                                  "bnfsite",!!Uri.bnfsite]

      let s = [ Some(a Uri.MedicinalFormEntity)
                x.title >>= (string >> xsd.string >> (dataProperty !!"rdfs:label") >> Some)
                x.cautionaryAdvisoryLabels >>= (Graph.fromcals >> Some)] |> List.choose id

      let mps = x.medicinalProducts |> List.map Graph.from
      let dr r = resource (Uri.from x) r
      [dr s
       dr mps]
       |> Assert.graph og

  type Graph with
    static member from (x:Drug) =
      let og = Graph.ReallyEmpty ["nicebnf",!!Uri.nicebnf
                                  "cnt",!!"http://www.w3.org/2011/content#"
                                  "rdfs",!!"http://www.w3.org/2000/01/rdf-schema#"
                                  "bnfsite",!!Uri.bnfsite]

      let s = [ Some(a Uri.DrugEntity)
                Some(dataProperty !!"rdfs:label" ((getval x.name)^^xsd.string))
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
       dr (x.interactionLinks |> Seq.map Graph.fromil |> Seq.toList)
       dr (x.sections |> Seq.map sec |> Seq.choose id |> Seq.toList)]
       |> Assert.graph og

    static member fromdc (InheritsFromClass (c)) =
      one !!"nicebnf:inheritsFromClass" (Uri.fromdc c) [a Uri.DrugClassEntity]

    static member fromc (Classification (_,is)) =
      [(a Uri.ClassificationEntity)] @ (is |> Seq.map Graph.fromdc |> Seq.toList)

    //the label for this is in another part of the feed so will be created elsewhere
    static member fromcl (c:Classification) =
      one !!"nicebnf:hasClassification" (Uri.fromc c) (Graph.fromc c)

    static member fromil (i:InteractionLink) =
      one !!"nicebnf:hasInteraction" (Uri.from i) [a Uri.InteractionEntity]

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
      Some(dataProperty !!"rdfs:Literal" (s^^xsd.string))

    static member fromsp (Specificity (Paragraph s,r,i)) =
      let s = [ Some(dataProperty !!"rdfs:Literal" (s^^xsd.string))
                r >>= Graph.from
                i >>= Graph.from]
      blank !!"nicebnf:hasSpecificity" (s |> List.choose id)

    static member fromgi (GeneralInformation (sd,sp)) =
      let s = [Some(dataProperty !!"cnt:ContentAsXML" (xsd.string(sd.ToString())))
               sp >>= (Graph.fromsp >> Some)]
      blank !!"nicebnf:hasGeneralInformation" (s |> List.choose id)

    static member fromda (DoseAdjustment (sd,sp)) =
      let s = [Some(dataProperty !!"cnt:ContentAsXML" (xsd.string(sd.ToString())))
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
      let dp n = string >> xsd.xmlliteral >> (dataProperty !!("nicebnf:has" + n))
      match x with 
       | Pharmacokinetics s -> s |> dp "Pharmacokinetics"
       | DoseEquivalence s -> s |> dp "DoseEquivalence"
       | DoseAdjustments s -> s |> dp "DoseAdjustments"
       | ExtremesOfBodyWeight s -> s |> dp "ExtremesOfBodyWeight"
       | Potency s -> s |> dp "Potency"

    static member fromamri (AdditionalMonitoringInRenalImpairment s) =
      blank !!"nicebnf:hasAdditionalMonitoringInRenalImpairment"
        [dataProperty !!"cnt:ContentAsXML" (xsd.string(s.ToString()))]

    static member fromgpa (GeneralPatientAdvice (c,t,sp)) =
      let s = [Some(dataProperty !!"cnt:ContentAsXML" (xsd.string(c.ToString())))
               t >>= Graph.fromti
               sp >>= (Graph.fromsp >> Some)]
      blank !!"nicebnf:hasGeneralPatientAdvice" (s |> List.choose id)

    static member frommd (AdviceAroundMissedDoses s) =
      blank !!"nicebnf:hasAdviceAroundMissedDoses" [dataProperty !!"cnt:ContentAsXML" (xsd.string(s.ToString()))]

    static member fromlvs (LicensingVariationStatement(Html(s))) =
      blank !!"nicebnf:hasLicensingVariationStatement" [dataProperty !!"cnt:ContentAsXML" (xsd.string(s.ToString()))]

    static member fromhtml (Html(s)) =
      dataProperty !!"cnt:ContentAsXML" (xsd.string(s.ToString()))

    static member frommfl (MedicinalFormLink(l)) =
      blank !!"nicebnf:hasMedicinalForm"
        [dataProperty !!"rdfs:label" (l.Title^^xsd.string)
         dataProperty !!"nicebnf:medicinalForm" ((Uri.bnfsite + "medicinalform/" + l.Url.[1..])^^xsd.string)]

    static member fromcsc (AllergyAndCrossSensitivityContraindications s) =
      blank !!"nicebnf:hasAllergyAndCrossSensitivityContraindications"
        [dataProperty !!"cnt:ContentAsXML" (xsd.string(s.ToString()))]

    static member fromcscs (AllergyAndCrossSensitivityCrossSensitivity s) =
      blank !!"nicebnf:hasAllergyAndCrossSensitivityCrossSensitivity"
        [dataProperty !!"cnt:ContentAsXML" (xsd.string(s.ToString()))]

    static member from (x:drugProvider.Sectiondiv) =
      dataProperty !!"cnt:ContentAsXML" (xsd.string(x.ToString()))

    static member frompair (sp,s:drugProvider.Sectiondiv) =
      [sp >>= (Graph.fromsp >> Some)
       Some(Graph.from s)] |> List.choose id

    static member fromexc (ExceptionToLegalCategory (sp,s)) =
      blank !!"nicebnf:hasExceptionToLegalCategory" (Graph.frompair (sp,s))

    static member fromden (DentalPractitionersFormulary (sp,s)) =
      blank !!"nicebnf:hasDentalPractitionersFormulary" (Graph.frompair (sp,s))

    static member fromlsfp (LessSuitableForPrescribing (sp,s)) =
      blank !!"nicebnf:hasLessSuitableForPrescribing" (Graph.frompair (sp,s))

    static member fromhas (HandlingAndStorage (sp,s)) =
      blank !!"nicebnf:hasHandlingAndStorage" (Graph.frompair (sp,s))

    static member fromelt (EffectOnLaboratoryTest s) = Graph.from s
    static member frompts (PreTreatmentScreening s) = Graph.from s
    static member fromtc (TreatmentCessation s) = Graph.from s
    static member fromdac (DrugAction s) = Graph.from s

    static member fromfre (Frequency (f,ses)) =
      let s = ses |> Seq.map Graph.from |> Seq.toList
      let f = one !!"nicebnf:hasFrequency" (Uri.fromfre f) [a !!"nicebnf:Frequency"]
      blank !!"nicebnf:hasFrequencyGroup" (f :: s)

    static member from (x:SideEffect) =
      one !!"nicebnf:hasSideEffect" (Uri.fromse x) [a !!"nicebnf:SideEffect"]

    static member fromsea (SideEffectAdvice (sp,s)) =
      let s = [sp >>= (Graph.fromsp >> Some)
               Some(dataProperty !!"cnt:ContentAsXML" (xsd.string(s.ToString())))]
      blank !!"nicebnf:hasSideEffectAdvice" (s |> List.choose id)

    static member fromcon (Contraindication ph) =
      blank !!"nicebnf:hasContraindication" [dataProperty !!"cnt:ContentAsXML" (xsd.string(ph.ToString()))]

    static member fromcg (x:CautionsGroup) =
      let cau x = dataProperty !!"nicebnf:hasCaution" (xsd.string(x.ToString()))
      let gen (p,cs) = (dataProperty !!"cnt:ContentAsXML" (xsd.string(p.ToString()))) :: (cs |> List.map cau)
      match x with
        | GeneralCautions (p,cs) -> blank !!"nicebnf:hasGeneralCautions" (gen(p,cs))
        | CautionsWithRoutes (t,p,cs) -> blank !!"nicebnf:hasCautionsWithRoutes"
                                          (dataProperty !!"nicebnf:hasRoute" (xsd.string(t.ToString()))
                                           :: gen(p,cs))
        | CautionsWithIndications (t,p,cs) -> blank !!"nicebnf:hasCautionsWithIndications"
                                               (dataProperty !!"nicebnf:hasRoute" (xsd.string(t.ToString()))
                                                :: gen(p,cs))

    static member frompadi (PrescribingAndDispensingInformation (sp,s)) =
      blank !!"nicebnf:hasPrescribingAndDispensingInformation" (Graph.frompair (sp,s))
    static member fromulu (UnlicencedUse (sp,s)) =
      blank !!"nicebnf:hasUnlicencedUse" (Graph.frompair (sp,s))
    static member fromcac (ConceptionAndContraception (sp,s)) =
      blank !!"nicebnf:hasConceptionAndContraception" (Graph.frompair (sp,s))
    static member fromisi (ImportantSafetyInformation(t,sp,s)) =
      let st = [t >>= Graph.fromti] |> List.choose id
      blank !!"nicebnf:hasImportantSafetyInformation" (st @ (Graph.frompair (sp,s)))
    static member fromdfa (DirectionsForAdministration (sp,s)) =
      blank !!"nicebnf:hasDirectionsForAdministration" (Graph.frompair (sp,s))

    static member fromfd (x:FundingDecision) =
      match x with
        | NonNHS(sp,s) -> blank !!"nicebnf:hasNonNHS" (Graph.frompair (sp,s))
        | SmcDecisions(sp,s) -> blank !!"nicebnf:hasSmcDecisions" (Graph.frompair(sp,s))
        | NiceTechnologyAppraisals(fi,t,sp,s) ->
           let s = [sp >>= (Graph.fromsp >> Some)
                    Some(Graph.from s)
                    t >>= Graph.fromti
                    fi >>= Graph.from] |> List.choose id
           blank !!"nicebnf:hasNiceTechnologyAppraisals" s

    static member fromsec sid (x:MonographSection) =

      let sec n i st =
        let s =  a !!("nicebnf:" + n) :: (st |> List.collect id)
        one !!("nicebnf:has" + n) i s

      let inline statments g x = x |> Seq.map g |> Seq.toList
      let inline statment g x =
        match x with
        | Some(x) -> [g x]
        | None -> []

      let xml x = dataProperty !!"cnt:ContentAsXML" (xsd.string(x.ToString()))

      match x with
        | Pregnancy (i,gs) -> Some(sec "PregnancyWarning" (sid i) [statments Graph.fromgi gs])
        | BreastFeeding (i,gs) -> Some(sec "BreastFeedingWarning" (sid i) [statments Graph.fromgi gs])
        | HepaticImpairment (i,gs,das) -> Some(sec "HepaticImpairmentWarning" (sid i) [statments Graph.fromgi gs
                                                                                       statments Graph.fromda das])
        | RenalImpairment (i,gs,amri,das) -> Some(sec "RenalImpairmentWarning" (sid i) [statments Graph.fromgi gs
                                                                                        statments Graph.fromamri amri
                                                                                        statments Graph.fromda das])
        | IndicationsAndDoseGroup (i,g,gss) -> Some(sec "IndicationAndDoseGroup" (sid i) [statments Graph.fromidg g
                                                                                          statments Graph.fromidgs gss])
        | PatientAndCarerAdvice (i,md,gpa) -> Some(sec "PatientAndCarerAdvice" (sid i) [statments Graph.frommd md
                                                                                        statments Graph.fromgpa gpa ])
        | MedicinalForms (i,lvs,html,mfls) -> Some(sec "MedicinalForms" (sid i) [ statment Graph.fromlvs lvs
                                                                                  statment Graph.fromhtml html
                                                                                  statments Graph.frommfl mfls])
        | AllergyAndCrossSensitivity (i,csc,cscs) -> Some(sec "AllergyAndCrossSensitivityWarning" (sid i) [ statment Graph.fromcsc csc
                                                                                                            statment Graph.fromcscs cscs])
        | ExceptionsToLegalCategory (i,es) -> Some(sec "ExceptionsToLegalCategory" (sid i) [statments Graph.fromexc es])
        | ProfessionSpecificInformation (i,dps) -> Some(sec "ProfessionSpecificInformation" (sid i) [statments Graph.fromden dps])
        | EffectOnLaboratoryTests (i,elts) -> Some(sec "EffectOnLaboratoryTests" (sid i) [statments Graph.fromelt elts])
        | PreTreatmentScreenings (i,ptss) -> Some(sec "PreTreatmentScreenings" (sid i) [statments Graph.frompts ptss])
        | LessSuitableForPrescribings (i,lsfps) -> Some(sec "LessSuitableForPrescribings" (sid i) [statments Graph.fromlsfp lsfps])
        | HandlingAndStorages (i,hass) -> Some(sec "HandlingAndStorages" (sid i) [statments Graph.fromhas hass])
        | TreatmentCessations (i,tcs) -> Some(sec "TreatmentCessations" (sid i) [statments Graph.fromtc tcs])
        | DrugActions (i,das) -> Some(sec "DrugActions" (sid i) [statments Graph.fromdac das])
        | SideEffects (i,fres,seas) -> Some(sec "SideEffects" (sid i) [statments Graph.fromfre fres
                                                                       statments Graph.fromsea seas])
        | Contraindications (i,cs,ps) -> Some(sec "Contraindications" (sid i) [statments Graph.fromcon cs
                                                                               statments xml ps])
        | Cautions (i,cgs) -> Some(sec "Cautions" (sid i) [statments Graph.fromcg cgs])
        | PrescribingAndDispensingInformations (i,padi) -> Some(sec "PrescribingAndDispensingInformations" (sid i) [statments Graph.frompadi padi])
        | UnlicencedUses (i,ulus) -> Some(sec "UnlicencedUses" (sid i) [statments Graph.fromulu ulus])
        | ConceptionAndContraceptions (i,cacs) -> Some(sec "ConceptionAndContraceptions" (sid i) [statments Graph.fromcac cacs])
        | ImportantSafetyInformations (i,isis) -> Some(sec "ImportantSafetyInformations" (sid i) [statments Graph.fromisi isis])
        | DirectionsForAdministrations (i,dfas) -> Some(sec "DirectionsForAdministrations" (sid i) [statments Graph.fromdfa dfas])
        | NationalFunding (i,fds) -> Some(sec "NationalFunding" (sid i) [statments Graph.fromfd fds])
        | _ -> None
