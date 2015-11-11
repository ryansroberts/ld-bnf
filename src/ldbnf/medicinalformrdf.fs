namespace Bnf
open FSharp.RDF
open FSharp.Data.Runtime


module DrugClassificationRdf =
  open prelude
  open resource
  open DrugClassification
  open Assertion
  open rdf
  open Rdf
  open Shared
  open RdfUris

  type Graph with
    static member from (DrugClassifications cs) =
      let og = Graph.ReallyEmpty ["nicebnf",!!Uri.nicebnf
                                  "cnt",!!"http://www.w3.org/2011/content#"
                                  "rdfs",!!"http://www.w3.org/2000/01/rdf-schema#"
                                  "bnfsite",!!Uri.bnfsite]

      cs |> List.map Graph.from |> Assert.graph og

    static member from (x:Classification) =
      resource !!(Uri.nicebnfClass + "Classification#" + x.key)
         [dataProperty !!"rdfs:Label" (x.value^^xsd.string)]


module TreatmentSummaryRdf =
  open prelude
  open resource
  open Bnf.TreatmentSummary
  open Assertion
  open rdf
  open Shared
  open Rdf
  open RdfUris

  type Graph with
    static member from (x:TreatmentSummary) =
      let og = Graph.ReallyEmpty ["nicebnf",!!Uri.nicebnf
                                  "cnt",!!"http://www.w3.org/2011/content#"
                                  "rdfs",!!"http://www.w3.org/2000/01/rdf-schema#"
                                  "bnfsite",!!Uri.bnfsite]

      let s = [a Uri.TreatmentSummaryEntity |> Some
               Graph.secondary x] |> List.choose id
      let p = Graph.fromts x
      let dr r = resource (Uri.from x) r
      [dr s
       dr p] |> Assert.graph og

    static member secondary (TreatmentSummary (_,x)) =
      match x with
        | ComparativeInformation _ -> a !!(Uri.nicebnf + "ComparativeInformation") |> Some
        | ManagementOfConditions _ -> a !!(Uri.nicebnf + "ManagementOfConditions") |> Some
        | MedicalEmergenciesBodySystems _ -> a !!(Uri.nicebnf + "MedicalEmergenciesBodySystems") |> Some
        | TreatmentOfBodySystems _ -> a !!(Uri.nicebnf + "TreatmentOfBodySystems") |> Some
        | Generic _ -> None


    static member fromti (Title s) =
      dataProperty !!"nicebnf:hasTitle" (s^^xsd.string)
    static member fromdoi (Shared.Doi s) =
      dataProperty !!"nicebnf:hasDoi" (s^^xsd.string)
    static member frombs (BodySystem s) =
      dataProperty !!"nicebnf:hasBodySystem" (s^^xsd.string)
    static member fromta (TargetAudience s) =
      dataProperty !!"nicebnf:hasTargetAudience" (s^^xsd.string)
    static member fromcontent (Content(s,ta)) =
      let s = [ta >>= (Graph.fromta >> Some)
               Some(dataProperty !!"cnt:ContentAsXML" ((string s)^^xsd.xmlliteral))] |> List.choose id
      blank !!"nicebnf:hasContent" s

    static member from (x:Link) =
      dataProperty !!"nicebnf:hasLink" (x.uri^^xsd.string) //this needs to be a uri at some point
    static member from (x:Summary) =
      let ls = x.links |> Seq.map Graph.from |> Seq.toList
      let cs = x.content |> List.map Graph.fromcontent
      [Graph.fromti x.title
       Graph.fromdoi x.doi
       Graph.frombs x.bodySystem] @ ls @ cs

    static member fromts (TreatmentSummary (_,x)) =
      match x with
        | ComparativeInformation s -> Graph.from s
        | ManagementOfConditions s -> Graph.from s
        | MedicalEmergenciesBodySystems s -> Graph.from s
        | TreatmentOfBodySystems s -> Graph.from s
        | Generic s -> Graph.from s

module MedicinalFormRdf =
  open prelude
  open resource
  open Bnf.Drug
  open Bnf.MedicinalForm
  open Assertion
  open rdf
  open Shared
  open Rdf
  open RdfUris

  type Graph with
    static member from (x:MedicinalForm) =
      let og = Graph.ReallyEmpty ["nicebnf",!!Uri.nicebnf
                                  "cnt",!!"http://www.w3.org/2011/content#"
                                  "rdfs",!!"http://www.w3.org/2000/01/rdf-schema#"
                                  "bnfsite",!!Uri.bnfsite]

      let s = [ Some(a Uri.MedicinalFormEntity)
                x.title >>= (string >> xsd.string >> (dataProperty !!"rdfs:label") >> Some)
                x.cautionaryAdvisoryLabels >>= (Graph.fromcals >> Some)
                x.excipients >>= Graph.fromexc
                x.electrolytes >>= Graph.fromele] |> List.choose id

      let mps = x.medicinalProducts |> List.map Graph.from
      let dr r = resource (Uri.from x) r
      [dr s
       dr mps]
       |> Assert.graph og

    static member fromcal (CautionaryAdvisoryLabel(ln,p)) =
      let s = [Some(dataProperty !!"cnt:ContentAsXML" ((string p)^^xsd.xmlliteral))
               ln >>= (string >> xsd.string >> (dataProperty !!"nicebnf:hasLabelNumber") >> Some)]
               |> List.choose id
      Some(blank !!"nicebnf:hasCautionaryAdvisoryLabel" s)

    static member fromcals (CautionaryAdvisoryLabels(t,cals)) =
      let c = cals |> Array.map Graph.fromcal |> Array.toList
      let s = ((t >>= (string >> xsd.string >> (dataProperty !!"rdfs:label") >> Some)) :: c)
               |> List.choose id
      blank !!"nicebnf:hasCautionaryAdvisoryLabels" s

    static member dp n = xsd.string >> (dataProperty !!("nicebnf:has" + n))

    static member fromman (Manufacturer x) = Graph.dp "Manufacturer" x |> Some
    static member frombt (BlackTriangle x) = Graph.dp "BlackTriangle" x |> Some
    static member frommpt (MedicinalProductTitle(m,bt,t)) =
      let tc = string >> xsd.string >> (dataProperty !!"cnt:ContentAsXML") >> Some
      let s = [m >>= Graph.fromman
               bt >>= Graph.frombt
               tc t] |> List.choose id
      blank !!"nicebnf:hasMedicinalProductTitle" s |> Some

    static member fromexc (Excipients e) =
      dataProperty !!"nicebnf:hasExcipients" ((string e)^^xsd.xmlliteral) |> Some

    static member fromele (Electrolytes e) =
      dataProperty !!"nicebnf:hasElectrolytes" ((string e)^^xsd.xmlliteral) |> Some

    static member fromsai(StrengthOfActiveIngredient p) = Graph.dp "StrengthOfActiveIngredient" (string p) |> Some

    static member fromnhsi (NhsIndicative x) = Graph.dp "NhsIndicative" x |> Some
    static member frompt (PriceText x) = Graph.dp "PriceText" x |> Some
    static member fromnhsip (NhsIndicativePrice x) = Graph.dp "NhsIndicativePrice" (string x) |> Some
    static member fromnhsii (NhsIndicativeInfo(nhsi,pt,nhsip)) =
      let s = [nhsi >>= Graph.fromnhsi
               pt >>= Graph.frompt
               nhsip >>= Graph.fromnhsip] |> List.choose id
      blank !!"nicebnf:hasNhsIndicativeInfo" s |> Some

    static member fromps (PackSize d) = Graph.dp "PackSize" (string d) |> Some
    static member fromuom u = Graph.dp "UnitOfMeasure" (string u) |> Some
    static member fromlc lc = Graph.dp "LegalCategory" (string lc) |> Some
    static member frompackinfo (PackInfo(ps,uom,lc)) =
      let s = [ps >>= Graph.fromps
               uom >>= Graph.fromuom
               lc >>= Graph.fromlc] |> List.choose id
      blank !!"nicebnf:hasPackInfo" s |> Some

    static member fromdt (DrugTarrif s) = Graph.dp "DrugTarrif" s |> Some
    static member fromdtp (DrugTariffPrice dtp) = Graph.dp "DrugTariffPrice" dtp |> Some
    static member fromdti (DrugTariffInfo(dt,pt,dtp)) =
      let s = [dt >>= Graph.fromdt
               pt >>= Graph.frompt
               dtp >>= Graph.fromdtp] |> List.choose id
      blank !!"nicebnf:hasDrugTarrifInfo" s |> Some

    static member frompack(Pack(pi,nii,dti)) =
      let s = [pi >>= Graph.frompackinfo
               nii >>= Graph.fromnhsii
               dti >>= Graph.fromdti
               Some(a !!"nicebnf:Pack")] |> List.choose id
      blank !!"nicebnf:hasPack" s

    static member from (x:MedicinalProduct) =
      let s = [Some(a Uri.MedicinalProductEntity)
               Some(x.ampid |> string |> Graph.dp "Ampid")
               x.title |> Graph.frommpt
               x.strengthOfActiveIngredient >>= Graph.fromsai
               ] |> List.choose id
      let ps = x.packs |> List.map Graph.frompack
      one !!"nicebnf:hasMedicinalProduct" (Uri.from x) (s @ ps)
