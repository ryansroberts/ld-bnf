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
    static member nicesite = "http://bnf.nice.org.uk/"
    static member from (x:Drug) = !!(Uri.nicesite + "drug/" + string x.id )
    static member from (InheritsFromClass l) = !!(Uri.nicebnf + "Classification#"  + l)
    static member from (Route s) = !!(Uri.nicebnf + "Route#" + s)
    static member from (Indication s) = !!(Uri.nicebnf + "Indication#" + s)
    static member from (TheraputicUse (n,_)) = !!(Uri.nicebnf + "TheraputicUse#" + n)
    static member fromsec (x:Drug) (Id i) = !!(Uri.nicesite + "drug/" + string x.id + "#" + i)

    static member from (x:MedicinalForm) = !!(Uri.nicesite + "drug/" + string x.id )

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
                                  "bnfsite",!!Uri.nicesite]
      let s = [ Some(a !!"nicebnf:MedicinalForm")]
      let dr r = resource (Uri.from x) r
      [dr (s |> List.choose id)]
       |> Assert.graph og

  type Graph with
    static member from (x:Drug) =
      let og = Graph.ReallyEmpty ["nicebnf",!!Uri.nicebnf
                                  "cnt",!!"http://www.w3.org/2011/content#"
                                  "rdfs",!!"http://www.w3.org/2000/01/rdf-schema#"
                                  "bnfsite",!!Uri.nicesite]

      let s = [ Some(a !!"nicebnf:Drug")
                Some(dataProperty !!"rdfs:label" ((getval x.name)^^xsd.string))
                x.vtmid >>= getvtmid >>= (xsd.string >> dataProperty !!"nicebnf:vtmid" >> Some)]

      let dr r = resource (Uri.from x) r

      //pass in uri construction for sections
      let sec = Graph.fromsec (Uri.fromsec x)

      [dr (s |> List.choose id)
       dr (x.primaryDomainOfEffect |> Graph.from)
       dr (x.secondaryDomainsOfEffect |> Graph.from)
       dr (x.classifications |> Seq.map Graph.from |> Seq.toList)
       dr (x.interactionLinks |> Seq.map Graph.from |> Seq.toList)
       dr (x.sections |> Seq.map sec |> Seq.choose id |> Seq.toList)]
       |> Assert.graph og

    static member fromdc (InheritsFromClass (c)) = 
      objectProperty !!"nicebnf:drugclass" !!("bnfsite:drugclasses/" + c)

    //the label for this is in another part of the feed so will be created elsewhere
    static member from (Classification (Id l,is)) =
      one !!"nicebnf:hasClassification" !!("bnfsite:classification#" + l) (is |> Seq.map Graph.fromdc |> Seq.toList)

    static member from (InteractionLink (l)) =
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

    static member from (DomainOfEffect (n,p,s)) =
      let s = [n >>=  (xsd.string >> (dataProperty !!"rdfs:label") >> Some)
               p >>= Graph.fromptu
               s >>= Graph.fromstu]
      s |> List.choose id

    static member from (PrimaryDomainOfEffect d) =
      one !!"nicebnf:hasPrimaryDomainOfEffect" !!"nicebnf:PrimaryDomainOfEffect" (Graph.from d)

    static member from (SecondaryDomainsOfEffect ds) =
      ds |> Seq.map (Graph.from >> (one !!"nicebnf:hasPrimaryDomainOfEffect" !!"nicebnf:PrimaryDomainOfEffect"))

    static member from (x:Route) =
      Some(objectProperty !!"nicebnf:hasRoute" (Uri.from x))

    static member from (x:Indication) =
      Some(objectProperty !!"nicebnf:hasIndication" (Uri.from x))

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

    //static member from (Pregnancy (Id(i),gis)) =
    //  one !!"nicebnf:pregnancy" !!("nicebnf:pregnancy#" + i) (gis |> Seq.map Graph.from |> Seq.toList)

    static member general n i (gis:seq<GeneralInformation>) =
      let s = a !!("nicebnf:" + n) :: (gis |> Seq.map Graph.fromgi |> Seq.toList)
      one !!("nicebnf:has" + n) i s

    static member fromsec sid (x:MonographSection) =
      match x with
        | Pregnancy (i,gis) -> Some(Graph.general "PregnancyWarning" (sid i) gis)
        | BreastFeeding (i,gis) -> Some(Graph.general "BreastFeedingWarning" (sid i) gis)
        | HepaticImpairment (i,gis,das) ->
            Some(one !!"nicebnf:hasHepaticImpairmentWarning" (sid i) (a !!"nicebnf:HepaticImpairmentWarning" :: (gis |> Seq.map Graph.fromgi |> Seq.toList) @ (das |> Seq.map Graph.fromda |> Seq.toList)))
        | _ -> None

