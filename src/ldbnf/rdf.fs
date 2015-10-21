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
    static member from (InheritsFromClass l) = !!(Uri.nicebnf + "Classification#"  + l)
    static member from (Route s) = !!(Uri.nicebnf + "Route#" + s)
    static member from (Indication s) = !!(Uri.nicebnf + "Indication#" + s)
    static member fromgrp (s:string) = !!(Uri.nicebnf + "Group#" + s)
    static member fromdsg (s:string) = !!(Uri.nicebnf + "Dosage#" + s)
    static member from (TheraputicUse (n,_)) = !!(Uri.nicebnf + "TheraputicUse#" + n)
    static member fromsec (x:Drug) (Id i) = !!(Uri.bnfsite + "drug/" + string x.id + "#" + i)

    static member from (x:MedicinalForm) = !!(Uri.bnfsite + "drug/" + string x.id )

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
                 | Some d -> Graph.fromsdoe d
                 | None -> Seq.empty<(Predicate * Object)>


      [dr (s |> List.choose id)
       dr (sdoe |> Seq.toList)
       dr (x.classifications |> Seq.map Graph.fromcl |> Seq.toList)
       dr (x.interactionLinks |> Seq.map Graph.fromil |> Seq.toList)
       dr (x.sections |> Seq.map sec |> Seq.choose id |> Seq.toList)]
       |> Assert.graph og

    static member fromdc (InheritsFromClass (c)) =
      objectProperty !!"nicebnf:drugclass" !!("bnfsite:drugclasses/" + c)

    //the label for this is in another part of the feed so will be created elsewhere
    static member fromcl (Classification (Id l,is)) =
      one !!"nicebnf:hasClassification" !!("bnfsite:classification#" + l) (is |> Seq.map Graph.fromdc |> Seq.toList)

    static member fromil (InteractionLink (l)) =
      objectProperty !!"nicebnf:interaction" !!("bnfsite:interactions/" + l.Url)


    static member fromtu ((x:TheraputicUse), ?name0:string) =
      let name = defaultArg name0 "nicebnf:hasTherapeuticUse"
      let s = match x with | TheraputicUse(n,u) ->
                               [Some(dataProperty !!"rdfs:label" (n^^xsd.string))
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
      let s = [n >>=  (xsd.string >> (dataProperty !!"rdfs:label") >> Some)
               p >>= Graph.fromptu
               s >>= Graph.fromstu]
      s |> List.choose id

    static member frompdoe (PrimaryDomainOfEffect d) =
      one !!"nicebnf:hasPrimaryDomainOfEffect" !!"bnfsite:DomainOfEffect" (Graph.fromdoe d)

    static member fromsdoe (SecondaryDomainsOfEffect ds) =
      ds |> Seq.map (Graph.fromdoe >> (one !!"nicebnf:hasSecondaryDomainOfEffect" !!"nicebnf:SecondaryDomainOfEffect"))

    static member from (x:Route) =
      Some(objectProperty !!"nicebnf:hasRoute" (Uri.from x))

    static member from (x:Indication) =
      Some(objectProperty !!"nicebnf:hasIndication" (Uri.from x))

    static member from (x:PatientGroup) =
      [Some(objectProperty !!"nicebnf:hasGroup" (Uri.fromgrp x.Group))
       Some(objectProperty !!"nicebnf:hasDosage" (Uri.fromdsg x.Dosage))]

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
                             Some(objectProperty !!"nicebnf:hasDosage" (Uri.fromdsg pg.Dosage))
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
        [dataProperty !!"cnt:ContentAsXML" (xsd.string(s))]

    static member fromsec sid (x:MonographSection) =

      let sec n i st =
        let s =  a !!("nicebnf:" + n) :: (st |> List.collect id)
        one !!("nicebnf:has" + n) i s

      let inline tostatments g x = x |> Seq.map g |> Seq.toList

      match x with
        | Pregnancy (i,gs) -> Some(sec "PregnancyWarning" (sid i) [tostatments Graph.fromgi gs])
        | BreastFeeding (i,gs) -> Some(sec "BreastFeedingWarning" (sid i) [tostatments Graph.fromgi gs])
        | HepaticImpairment (i,gs,das) -> Some(sec "HepaticImpairmentWarning" (sid i) [tostatments Graph.fromgi gs
                                                                                       tostatments Graph.fromda das])
        | RenalImpairment (i,gs,amri,das) -> Some(sec "RenalImpairment" (sid i) [tostatments Graph.fromgi gs
                                                                                 tostatments Graph.fromamri amri
                                                                                 tostatments Graph.fromda das])
        | IndicationsAndDoseGroup (i,g) -> Some(sec "IndicationAndDoseGroup" (sid i) [tostatments Graph.fromidg g])
        | _ -> None
