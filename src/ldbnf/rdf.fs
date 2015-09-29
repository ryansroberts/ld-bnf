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

  type Classification with
    static member uri (Classification (l,is)) = !!l.Url
  type InheritsFromClass with
    static member uri (InheritsFromClass l) = !!l.Url

  let (>>=) a b = Option.bind b a

  //ld.nice.org.uk/ns/bnf/concept#someshitwefishedoutofxml
  //---/classification#PHP106980

  type Graph with
    static member from (x:Drug) =
      let og = Graph.empty (!!"http://ld.nice.org.uk/ns/bnf") [("base",!!"http://ld.nice.org.uk/ns/bnf")]

      let s = [ Some(a !!"base:Drug")
                Some(dataProperty !!"rdfs:label" ((getval x.name)^^xsd.string))
                x.vtmid >>= getvtmid >>= (xsd.string >> dataProperty !!":vtmid" >> Some)]
      let r =
        resource !!(getid x.id) (s |> List.choose id)
      [r] |> Assert.graph og

    static member from (x:Classification) =
      let c (Classification(l,is)) = l,is

      let s = snd(c x) |> Seq.map (fun i -> a (InheritsFromClass.uri i)) |> Seq.toList

      let title (Classification (l,is)) = l.Title
      resource (Classification.uri x) (( dataProperty !!"rdfs:label" ((title x)^^xsd.string) :: s))

