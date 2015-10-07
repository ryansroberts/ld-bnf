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
    static member nicebnf = "http://ld.nice.org.uk/ns/bnf/"
    static member nicesite = "http://bnf.nice.org.uk/"
    static member from (x:Drug) = !!(Uri.nicesite + "drug/" + string x.id )
    static member from (InheritsFromClass l) = !!(Uri.nicebnf + "Classification#"  + l)
    static member from (Route s) = !!(Uri.nicebnf + "Route#" + s)
    static member from (Indication s) = !!(Uri.nicebnf + "Indication#" + s)
    static member from (TheraputicUse (n,_)) = !!(Uri.nicebnf + "TheraputicUse#" + n)

  let (>>=) a b = Option.bind b a

  //ld.nice.org.uk/ns/bnf/concept#someshitwefishedoutofxml
  //---/classification#PHP106980

  type Graph with
    static member ReallyEmpty xp =
      let vds = new VDS.RDF.Graph()
      xp |> List.iter (fun (p, (Uri.Sys ns)) -> vds.NamespaceMap.AddNamespace(p, ns))
      Graph vds

    static member from (x:Drug) =
      let og = Graph.ReallyEmpty ["nicebnf",!!"http://ld.nice.org.uk/ns/bnf/"
                                  "cnt",!!"http://www.w3.org/2011/content#"]
 
      let s = [ Some(a !!"nicebnf:Drug")
                Some(dataProperty !!"nicebnf:name" ((getval x.name)^^xsd.string))
                x.vtmid >>= getvtmid >>= (xsd.string >> dataProperty !!"nicebnf:vtmid" >> Some)]

      let dr r = resource (Uri.from x) r
 
      [dr (s |> List.choose id)
       dr (x.classifications |> Seq.map Graph.from |> Seq.toList)
       dr (x.interactionLinks |> Seq.map Graph.from |> Seq.toList)
       dr (x.sections |> Seq.map Graph.from |> Seq.choose id |> Seq.toList)]
       |> Assert.graph og

    static member from (Classification (Id l,is)) =
      one !!"nicebnf:hasClassification" !!("nicebnf:classification#" + l) (is |> Seq.map (Uri.from >> a) |> Seq.toList)
      //do the label later
      //dataProperty !!"rdfs:label" (l^^xsd.string) ::


    static member from (InteractionLink (l)) =
      objectProperty !!"nicebnf:interaction" !!("nicebnf:interactions#" + l.Url)

    //abandoning this for the time being
    static member from (TheraputicUse (n,u)) =
      //build the child theraputic use
      one !!"nicebnf:therapeuticUse" (Uri.from (TheraputicUse (n,u))) [
        dataProperty !!"rdfs:label" (n^^xsd.string)
        ]

    //static member from (DomainOfEffect (n,p,s)) =

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
      one !!("nicebnf:" + n) !!((sprintf "nicebnf:%s#" n) + i) (gis |> Seq.map Graph.from |> Seq.toList)

    static member from (x:MonographSection) =
      match x with
        | Pregnancy (Id(i),gis) -> Some(Graph.general "pregnancy" i gis)
        | BreastFeeding (Id(i),gis) -> Some(Graph.general "breastfeeding" i gis)
        | HepaticImpairment (Id(i),gis,das) ->
            Some(one !!"nicebnf:hasHepaticImpairment" !!("nicebnf:HepaticImpairment#" + i) (gis |> Seq.map Graph.from |> Seq.toList))
        | _ -> None
