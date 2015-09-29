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

  let (>>=) a b = Option.bind b a

  type Graph with
    static member from (x:Drug) =
      let og = Graph.empty (!!"http://ld.nice.org.uk/ns/bnf") [("base",!!"http://ld.nice.org.uk/ns/bnf")]

      let s = [ Some(a !!"base:Drug")
                Some(dataProperty !!"rdfs:label" ((getval x.name)^^xsd.string))
                x.vtmid >>= getvtmid >>= (xsd.string >> dataProperty !!":vtmid" >> Some)]
      let r =
        resource !!(getid x.id) (s |> List.choose id)
      [r] |> Assert.graph og
