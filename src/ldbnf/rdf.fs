namespace Bnf
open FSharp.RDF

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
    static member bnfsite = "http://bnf.nice.org.uk/"

    static member from (x:Drug) = !!(Uri.bnfsite + "drug/" + string x.id )
    static member fromsec (x:Drug) (Id i) = !!(Uri.bnfsite + "drug/" + string x.id + "#" + i)
    static member from (x:MedicinalForm) = !!(Uri.bnfsite + "medicinalform/" + string x.id )
    //static member frommfl (x:Link) = !!(Uri.bnfsite + "medicinalform/" + x.Url.[1..])

    static member fromdc (s:string) = !!(Uri.nicebnf + "DrugClass/"  + s)
    static member from (Route s) = !!(Uri.nicebnf + "Route#" + s)
    static member from (Indication s) = !!(Uri.nicebnf + "Indication#" + s)
    static member fromgrp (s:string) = !!(Uri.nicebnf + "Group#" + s)
    static member fromdsg (s:string) = !!(Uri.nicebnf + "Dosage#" + s)
    static member from (TheraputicUse (n,_)) = !!(Uri.nicebnf + "TheraputicUse#" + n)
    static member from (DomainOfEffect (n,_,_)) = !!(Uri.nicebnf + "DomainOfEffect#" + n.Value.Trim())
    static member fromse (SideEffect s) = !!(Uri.nicebnf + "SideEffect#" + s)
    static member fromfre x = !!(Uri.nicebnf + "Frequency#" + x)

  type Graph with
      static member ReallyEmpty xp =
        let vds = new VDS.RDF.Graph()
        xp |> List.iter (fun (p, (Uri.Sys ns)) -> vds.NamespaceMap.AddNamespace(p, ns))
        Graph vds


  type Graph with
    static member from (x:MedicinalForm)=
      let og = Graph.ReallyEmpty ["nicebnf",!!Uri.nicebnf
                                  "cnt",!!"http://www.w3.org/2011/content#"
                                  "rdfs",!!"http://www.w3.org/2000/01/rdf-schema#"
                                  "bnfsite",!!Uri.bnfsite]
      let s = [ Some(a !!"nicebnf:MedicinalForm")]
      let dr r = resource (Uri.from x) r
      [dr (s |> List.choose id)]
       |> Assert.graph og

  type Graph with
    static member from (x:Drug) =
      let og = Graph.ReallyEmpty ["nicebnf",!!Uri.nicebnf
                                  "cnt",!!"http://www.w3.org/2011/content#"
                                  "rdfs",!!"http://www.w3.org/2000/01/rdf-schema#"
                                  "bnfsite",!!Uri.bnfsite]

      let s = [ Some(a !!"nicebnf:Drug")
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
      objectProperty !!"nicebnf:inheritsFromClass" (Uri.fromdc c)

    //the label for this is in another part of the feed so will be created elsewhere
    static member fromcl (Classification (Id l,is)) =
      one !!"nicebnf:hasClassification" !!("nicebnf:Classification/" + l) (is |> Seq.map Graph.fromdc |> Seq.toList)

    static member fromil (InteractionLink (l)) =
      objectProperty !!"nicebnf:interaction" !!("bnfsite:interactions/" + l.Url)


    static member fromtu ((x:TheraputicUse), ?name0:string) =
      let name = defaultArg name0 "nicebnf:hasTherapeuticUse"
      let s = match x with | TheraputicUse(n,u) ->
                               [Some(a !!"nicebnf:TheraputicUse")
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
      let s = [Some(a !!"nicebnf:DomainOfEffect")
               n >>=  (xsd.string >> (dataProperty !!"rdfs:label") >> Some)
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
             a !!"nicebnf:Route"])

    static member from (x:Indication) =
      Some(objectProperty !!"nicebnf:hasIndication" (Uri.from x))

    static member from (x:PatientGroup) =
      [Some(objectProperty !!"nicebnf:hasGroup" (Uri.fromgrp x.Group))
       Some(objectProperty !!"nicebnf:hasDosage" (Uri.fromdsg x.Dosage))]

    static member fromti (Bnf.Drug.Title (Paragraph(s))) =
      Some(dataProperty !!"rdfs:Literal" (s^^xsd.string))

    static member fromsp (Specificity (Paragraph s,r,i)) =
      let s = [Some(dataProperty !!"rdfs:Literal" (s^^xsd.string))
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
                           ([Some(objectProperty !!"nicebnf:hasGroup" (Uri.fromgrp pg.Group))
                             Some(dataProperty !!"nicebnf:hasDosage" (pg.Dosage^^xsd.string))
                             r >>= Graph.from] |> List.choose id)
      pgs |> Seq.map patientGrp

    static member from (TheraputicIndication s) =
      Some(dataProperty !!"nicebnf:hasTheraputicIndication" (s^^xsd.string))

    static member fromidg (IndicationsAndDose(tis,roas)) =
      let s = (tis |> Seq.map Graph.from |> Seq.choose id |> Seq.toList)
              @ (roas |> Seq.collect Graph.from |> Seq.toList)
      blank !!"nicebnf:hasIndicationAndDose" s

    static member fromamri (AdditionalMonitoringInRenalImpairment s) =
      blank !!"nicebnf:hasAdditionalMonitoringInRenalImpairment"
        [dataProperty !!"cnt:ContentAsXML" (xsd.string(s.ToString()))]

    static member fromgpa (GeneralPatientAdvice (c,t,sp)) =
      let s = [Some(dataProperty !!"cnt:ContentAsXML" (xsd.string(c.ToString())))
               t >>= Graph.fromti
               sp >>= (Graph.fromsp >> Some)]
      blank !!"nicebnf:hasGeneralPatientAdvice" (s |> List.choose id)

    static member frommd (AdviceAroundMissedDoses s) =
      blank !!"nicebnf:hasGeneralPatientAdvice" [dataProperty !!"cnt:ContentAsXML" (xsd.string(s.ToString()))]

    static member fromlvs (LicensingVariationStatement(Html(s))) =
      blank !!"nicebnf:hasLicensingVariationStatement" [dataProperty !!"cnt:ContentAsXML" (xsd.string(s.ToString()))]

    static member fromhtml (Html(s)) =
      dataProperty !!"cnt:ContentAsXML" (xsd.string(s.ToString()))

    static member frommfl (MedicinalFormLink(l)) =
      blank !!"nicebnf:hasMedicinalFormLink"
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

    static member from (x:CautionsGroup) =
      let cau x = dataProperty !!"nicebnf:hasCaution" (xsd.string(x.ToString()))
      let gen (p,cs) = (dataProperty !!"cnt:ContentAsXML" (xsd.string(p.ToString()))) :: (cs |> List.map cau)
      match x with
        | GeneralCautions (p,cs) -> blank !!"nicebnf:hasGeneralCautions" (gen(p,cs))
        | CautionsWithRoutes (t,p,cs) -> blank !!"nicebnf:hasCautionsWithRoutes"
                                          (dataProperty !!"nicebnf:hasRoute" (xsd.string(t.ToString()))
                                           :: gen(p,cs))

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
        | RenalImpairment (i,gs,amri,das) -> Some(sec "RenalImpairment" (sid i) [statments Graph.fromgi gs
                                                                                 statments Graph.fromamri amri
                                                                                 statments Graph.fromda das])
        | IndicationsAndDoseGroup (i,g) -> Some(sec "IndicationAndDoseGroup" (sid i) [statments Graph.fromidg g])
        | PatientAndCarerAdvice (i,md,gpa) -> Some(sec "PatientAndCarerAdvice" (sid i) [statments Graph.frommd md
                                                                                        statments Graph.fromgpa gpa ])
        | MedicinalForms (i,lvs,html,mfls) -> Some(sec "MedicinalForms" (sid i) [statment Graph.fromlvs lvs
                                                                                 statment Graph.fromhtml html
                                                                                 statments Graph.frommfl mfls])
        | AllergyAndCrossSensitivity (i,csc,cscs) -> Some(sec "AllergyAndCrossSensitivity" (sid i) [statment Graph.fromcsc csc
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
        | _ -> None
