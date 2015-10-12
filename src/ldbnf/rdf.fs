namespace Bnf
open FSharp.RDF

module DrugRdf =
  open resource
  open Bnf.Drug
  open Assertion
  open rdf

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

  let (>>=) a b = Option.bind b a

  //ld.nice.org.uk/ns/bnf/concept#someshitwefishedoutofxml
  //---/classification#PHP106980

  type Graph with
    static member ReallyEmpty xp =
      let vds = new VDS.RDF.Graph()
      xp |> List.iter (fun (p, (Uri.Sys ns)) -> vds.NamespaceMap.AddNamespace(p, ns))
      Graph vds

    static member from (x:Drug) =
      let og = Graph.ReallyEmpty ["nicebnf",!!Uri.nicebnf
                                  "cnt",!!"http://www.w3.org/2011/content#"
                                  "rdfs",!!"http://www.w3.org/2000/01/rdf-schema#"]
 
      let s = [ Some(a !!"nicebnf:Drug")
                Some(dataProperty !!"rdfs:label" ((getval x.name)^^xsd.string))
                x.vtmid >>= getvtmid >>= (xsd.string >> dataProperty !!"nicebnf:vtmid" >> Some)]

      let dr r = resource (Uri.from x) r

      //pass in uri construction for sections
      let sec = Graph.fromsec (Uri.fromsec x)
 
      [dr (s |> List.choose id)
       dr (x.classifications |> Seq.map Graph.from |> Seq.toList)
       dr (x.interactionLinks |> Seq.map Graph.from |> Seq.toList)
       dr (x.sections |> Seq.map sec |> Seq.choose id |> Seq.toList)]
       |> Assert.graph og

    //the label for this is in another part of the feed so will be created elsewhere
    static member from (Classification (Id l,is)) =
      one !!"nicebnf:hasClassification" !!("nicebnf:classification#" + l) (is |> Seq.map (Uri.from >> a) |> Seq.toList)

    static member from (InteractionLink (l)) =
      objectProperty !!"nicebnf:interaction" !!("nicebnf:interactions#" + l.Url)


    static member from (TheraputicUse (n,u)) =
      let s = [Some(dataProperty !!"rdfs:label" (n^^xsd.string))
               u >>= (Graph.from >> Some)]
      one !!"nicebnf:therapeuticUse" (Uri.from (TheraputicUse (n,u))) (s |> List.choose id)

    //static member from (DomainOfEffect (n,p,s)) =

    //PrimaryTheraputicUse
    //SecondaryTheraputicUses

    static member from (x:Route) =
      Some(objectProperty !!"nicebnf:hasRoute" (Uri.from x))

    static member from (x:Indication) =
      Some(objectProperty !!"nicebnf:hasIndication" (Uri.from x))

    static member from (Specificity (Paragraph s,r,i)) =
      let s = [Some(dataProperty !!"rdfs:Literal" (s^^xsd.string))
               r >>= Graph.from
               i >>= Graph.from]
      blank !!"nicebnf:#hasSpecificity" (s |> List.choose id)

    static member from (GeneralInformation (sd,sp)) =
      let s = [Some(dataProperty !!"cnt:ContentAsXML" (xsd.string(sd.ToString())))
               sp >>= (Graph.from >> Some)]
      blank !!"nicebnf:generalInformation" (s |> List.choose id)

    static member from (DoseAdjustment (sd,sp)) =
      let s = [Some(dataProperty !!"cnt:ContentAsXML" (xsd.string(sd.ToString())))
               sp >>= (Graph.from >> Some)]
      blank !!"nicebnf:doseadjsutment" (s |> List.choose id)

    //static member from (Pregnancy (Id(i),gis)) =
    //  one !!"nicebnf:pregnancy" !!("nicebnf:pregnancy#" + i) (gis |> Seq.map Graph.from |> Seq.toList) 

    static member general n i (gis:seq<GeneralInformation>) =
      one !!("nicebnf:" + n) i (gis |> Seq.map Graph.from |> Seq.toList)

    static member fromsec sid (x:MonographSection) =
      match x with
        | Pregnancy (i,gis) -> Some(Graph.general "pregnancy" (sid i) gis)
        | BreastFeeding (i,gis) -> Some(Graph.general "breastfeeding" (sid i) gis)
        | HepaticImpairment (i,gis,das) ->
            Some(one !!"nicebnf:hasHepaticImpairment" (sid i) ((gis |> Seq.map Graph.from |> Seq.toList) @ (das |> Seq.map Graph.from |> Seq.toList)))
        | _ -> None

//figure out how to pass the drug id around, probably a partially executed function
