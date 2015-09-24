namespace Bnf
open FSharp.RDF

module DrugRdf =
  open resource
  open Bnf.Drug
  open Assertion
  open rdf

  let getval (DrugName n) = n
  let getid (Id n) = "http://bnf.nice.org.uk/" + n + ".html"

  type Graph with
    static member from (x:Drug) =
      let og = Graph.empty (!!"http://ld.nice.org.uk/ns/bnf") [("base",!!"http://ld.nice.org.uk/ns/bnf")]
      let r =
        resource !!(getid x.id)
            [ a !!"base:Drug"
              dataProperty !!"rdfs:label" ((getval x.name)^^xsd.string)]
      [r] |> Assert.graph og
