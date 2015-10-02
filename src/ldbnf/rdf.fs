namespace Bnf
open FSharp.RDF

module DrugRdf =
  open resource
  open Bnf.Drug
  open Assertion
  open rdf

  let getval (DrugName n) = n
  let getid (Id n) = "http://bnf.nice.org.uk/" + n + ".html"
  let getvtmid (Vtmid i) = Some(string i)
  let tosys (Sys s) = s

  type Uri with
    static member from (InheritsFromClass l) = !!("base:classification" + l.Url)

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
      
      let s = [ Some(a !!"nicebnf:drug")
                Some(dataProperty !!"rdfs:label" ((getval x.name)^^xsd.string))
                x.vtmid >>= getvtmid >>= (xsd.string >> dataProperty !!"nicebnf:vtmid" >> Some)]

      let dr r = resource !!(getid x.id) r

      let rd = dr (s |> List.choose id)
      let rc = dr (x.classifications |> Seq.map Graph.from |> Seq.toList)
      let il = dr (x.interactionLinks |> Seq.map Graph.from |> Seq.toList)
      let se = dr (x.sections |> Seq.map Graph.from |> Seq.choose id |> Seq.toList)

      [rd;rc;il;se] |> Assert.graph og

    static member from (Classification (Id l,is)) =
      let s = is |> Seq.map (Uri.from >> a) |> Seq.toList
      one !!"nicebnf:classification" !!("nicebnf:classification#" + l) ( dataProperty !!"rdfs:label" (l^^xsd.string) :: s)

    static member from (InteractionLink (l)) =
      objectProperty !!"nicebnf:interaction" !!("nicebnf:interactions#" + l.Url)

    //abandoning this for the time being
    static member from (TheraputicUse (n,u)) =
      //build the child theraputic use
      one !!"nicebnf:therapeuticUse" (!!("nicebnf:theraputicuse#" + n)) [
        dataProperty !!"rdfs:label" (n^^xsd.string)
        ]

    //static member from (DomainOfEffect (n,p,s)) =

    static member from (Route s) =
      Some(objectProperty !!"nicebnf:route" !!("nicebnf:route#" + s))

    static member from (Indication s) =
      Some(objectProperty !!"nicebnf:indication" !!("nicebnf:indication#" + s))

    static member from (Specificity (Paragraph s,r,i)) =
      let s = [Some(dataProperty !!"rdfs:Literal" (s^^xsd.string))
               r >>= Graph.from
               i >>= Graph.from]
      blank !!"nicebnf:specificity" (s |> List.choose id)

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
            Some(one !!"nicebnf:hepaticimpairment" !!("nicebnf:hepaticimpairment" + i) (gis |> Seq.map Graph.from |> Seq.toList))
        | _ -> None
