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

      let dr r = resource !!(getid x.id) r

      let rd = dr (s |> List.choose id)
      let rc = dr (x.classifications |> Seq.map Graph.from |> Seq.toList)
      [rd;rc] |> Assert.graph og

    static member from (Classification (l,is)) =
      let s = is |> Seq.map (InheritsFromClass.uri >> a) |> Seq.toList
      one !!"base:classification" !!l.Url ( dataProperty !!"rdfs:label" (l.Title^^xsd.string) :: s)

