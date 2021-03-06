namespace Bnf
open FSharp.RDF
open FSharp.Data.Runtime

module InteractionRdf = 
  open prelude
  open resource
  open Bnf.Interaction
  open Assertion
  open rdf
  open Rdf
  open Shared
  open RdfUris

  type Graph with
    static member from (InteractionList(id,t,il)) =
      let og = Graph.ReallyEmpty ["nicebnf",!!Uri.nicebnf
                                  "cnt",!!"http://www.w3.org/2011/content#"
                                  "rdfs",!!"http://www.w3.org/2000/01/rdf-schema#"
                                  "bnfsite",!!Uri.bnfsite]
      let s = [ a Uri.InteractionEntity
                t |> (string >> xsd.xmlliteral >> (dataProperty !!"rdfs:label"))]

      let iwuri = Uri.fromiw id

      let importance i =
        match i.importance with
          | High -> dataProperty !!"nicebnf:hasImportance" ("High"^^xsd.string)
          | NotSet -> dataProperty !!"nicebnf:hasImportance" ("NotSet"^^xsd.string)

      let interactionDetail i = one !!"nicebnf:hasInteraction" (iwuri i)
                                 [a Uri.InteractionDetailEntity
                                  objectProperty !!"nicebnf:interactsWith" (Uri.fromiwl i)
                                  importance i
                                  dataProperty !!"cnt:ContentAsXml" ((string i.message)^^xsd.xmlliteral)
                                  dataProperty !!"nicebnf:hasImportance" ((string i.importance)^^xsd.string)]

      let dr r = resource (Uri.fromil id) r
      [dr s
       dr (il |> List.map interactionDetail)]
       |> Assert.graph og

module BorderlineSubstanceRdf =
  open prelude
  open resource
  open Bnf.BorderlineSubstance
  open Assertion
  open rdf
  open Rdf
  open Shared
  open RdfUris

  let inline dpo n x = x >>= (string >> xsd.string >> (dataProperty !!("nicebnf:has" + n)) >> Some)

  type Graph with
    static member from (x:BorderlineSubstance) =
      let og = Graph.ReallyEmpty ["nicebnf",!!Uri.nicebnf
                                  "cnt",!!"http://www.w3.org/2011/content#"
                                  "rdfs",!!"http://www.w3.org/2000/01/rdf-schema#"
                                  "bnfsite",!!Uri.bnfsite]

      let s = [ a Uri.BorderlineSubstanceEntity |> Some
                x.title |> (string >> xsd.xmlliteral >> (dataProperty !!"rdfs:label")) |> Some
                x.category |> (string >> xsd.string >> (dataProperty !!"nicebnf:hasCategory")) |> Some
                x.intro >>= (string >> xsd.string >> (dataProperty !!"nicebnf:hasIntroductoryNote") >> Some)] |> List.choose id

      let ds = x.details |> List.map Graph.fromdetails

      let dr r = resource (Uri.from x) r
      [dr s
       dr ds]
       |> Assert.graph og


    static member frompackinfo (PackInfo(ps,uom,acbs)) =
      [ps |> dpo "PackSize"
       uom |> dpo "UnitOfMeasure"
       acbs |> dpo "Acbs"] |> List.choose id

    static member fromnhsindicativeinfo (NhsIndicativeInfo(nhsi,pt,nhsip)) =
      [nhsi |> dpo "NhsIndicative"
       pt |> dpo "PriceText"
       nhsip |> dpo "NhsIndicativePrice"] |> List.choose id

    static member frompricetarrif (PackSizePriceTariff(pi,nhs)) =
      let s = [pi >>= (Graph.frompackinfo >> Some)
               nhs >>= (Graph.fromnhsindicativeinfo >> Some)] |> List.choose id |> List.collect id
      blank !!"nicebnf:hasPackSizePriceTariff" s

    static member fromprep (BorderlineSubstancePrep(t,pts)) =
      let s = match t with
              | Some (PreparationTitle(p,m)) ->
                 [p |> (string >> xsd.xmlliteral >> (dataProperty !!"bnfsite:hasTitle")) |> Some
                  m >>= (string >> xsd.string >> (dataProperty !!"bnfsite:hasManufacturer") >> Some)] |> List.choose id
              | None -> []
      let ts = pts |> List.map Graph.frompricetarrif
      blank !!"nicebnf:hasBorderlineSubstancePrep" (s @ ts)

    static member fromdetail (x:Detail) =
      let inline dp n s = dataProperty !!("nicebnf:has" + n) ((string s)^^xsd.string)
      match x with
        | Formulation s -> s |> dp "Formulation"
        | EnergyKj e -> e |> dp "EnergyKj"
        | EnergyKcal e -> e |> dp "EnergyKcal"
        | ProteinGrams p -> p |> dp "ProteinGrams"
        | ProteinConstituents p -> p |> dp "ProteinConstituents"
        | CarbohydrateGrams c -> c |> dp "CarbohydrateGrams"
        | CarbohydrateConstituents c -> c |> dp "CarbohydrateConstituents"
        | FatGrams f -> f |> dp "FatGrams"
        | FatConstituents f -> f |> dp "FatConstituents"
        | FibreGrams f -> f |> dp "FibreGrams"
        | SpecialCharacteristics s -> s |> dp "SpecialCharacteristics"
        | Acbs a -> a |> dp "Acbs"
        | Presentation p -> p |> dp "Presentation"

    static member fromdetails (Details(ds,bsps)) =
      let dps = ds |> List.map Graph.fromdetail
      let preps = bsps |> List.map Graph.fromprep
      blank !!"nicebnf:hasDetails" (dps @ preps)


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
         [dataProperty !!"rdfs:label" (x.value^^xsd.string)]


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
               Some(dataProperty !!"nicebnf:hasDitaContent" ((string s)^^xsd.xmlliteral))] |> List.choose id
      blank !!"nicebnf:hasContent" s

    static member from (x:Link) =
      dataProperty !!"nicebnf:hasLink" (x.uri^^xsd.string) //this needs to be a uri at some point
    static member from (x:Summary) =
      let ls = x.links |> Seq.map Graph.from |> Seq.toList
      let cs = x.content |> List.map Graph.fromcontent
      let s = [Graph.fromti x.title |> Some
               x.doi >>= (Graph.fromdoi >> Some)
               x.bodySystem >>= (Graph.frombs >> Some)] |> List.choose id
      s @ ls @ cs

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
                x.excipients >>= Graph.fromexc
                x.electrolytes >>= Graph.fromele] |> List.choose id

      let cals = match x.cautionaryAdvisoryLabels with
                 | Some x -> Graph.fromcals x
                 | None -> []

      let mps = x.medicinalProducts |> List.map Graph.from
      let dr r = resource (Uri.from x) r
      [dr s
       dr mps
       dr cals]
       |> Assert.graph og

    static member fromcal (CautionaryAdvisoryLabel(ln,p)) =
      let s = [Some(dataProperty !!"nicebnf:hasDitaContent" ((string p)^^xsd.xmlliteral))
               ln >>= (string >> xsd.string >> (dataProperty !!"nicebnf:hasLabelNumber") >> Some)]
               |> List.choose id
      Some(blank !!"nicebnf:hasCautionaryAdvisoryLabel" s)

    static member fromcals (CautionaryAdvisoryLabels(_,cals)) =
      cals |> Array.choose Graph.fromcal |> Array.toList

    static member dp n = xsd.string >> (dataProperty !!("nicebnf:has" + n))

    static member fromman (Manufacturer x) = Graph.dp "Manufacturer" x |> Some
    static member frombt (BlackTriangle x) = Graph.dp "BlackTriangle" x |> Some
    static member frommpt (MedicinalProductTitle(m,bt,t)) =
      let tc = string >> xsd.string >> (dataProperty !!"nicebnf:hasDitaContent") >> Some
      let s = [m >>= Graph.fromman
               bt >>= Graph.frombt
               tc t] |> List.choose id
      blank !!"nicebnf:hasMedicinalProductTitle" s |> Some

    static member fromexc (Excipients e) =
      dataProperty !!"nicebnf:hasExcipients" ((string e)^^xsd.xmlliteral) |> Some

    static member fromele (Electrolytes e) =
      dataProperty !!"nicebnf:hasElectrolytes" ((string e)^^xsd.xmlliteral) |> Some

    static member fromsai(StrengthOfActiveIngredient p) = Graph.dp "StrengthOfActiveIngredient" (string p) |> Some
    static member fromcd(ControlledDrug p) = Graph.dp "ControlledDrug" (string p) |> Some

    static member fromnhsi (NhsIndicative x) = Graph.dp "NhsIndicative" x |> Some
    static member frompt (PriceText x) = Graph.dp "PriceText" x |> Some
    static member fromnhsip (NhsIndicativePrice x) = Graph.dp "NhsIndicativePrice" (string x) |> Some
    static member fromnhsii (NhsIndicativeInfo(nhsi,pt,nhsip)) =
      [nhsi >>= Graph.fromnhsi
       pt >>= Graph.frompt
       nhsip >>= Graph.fromnhsip] |> List.choose id

    static member fromps (PackSize d) = Graph.dp "PackSize" (string d) |> Some
    static member fromuom u = Graph.dp "UnitOfMeasure" (string u) |> Some
    static member fromlc lc = Graph.dp "LegalCategory" (string lc) |> Some
    static member frompackinfo (PackInfo(ps,uom,lc)) =
      [ps >>= Graph.fromps
       uom >>= Graph.fromuom
       lc >>= Graph.fromlc] |> List.choose id

    static member fromdt (DrugTarrif s) = Graph.dp "DrugTarrif" s |> Some
    static member fromdtp (DrugTariffPrice dtp) = Graph.dp "DrugTariffPrice" (string dtp) |> Some
    static member fromdti (DrugTariffInfo(dt,pt,dtp)) =
      [dt >>= Graph.fromdt
       pt >>= Graph.frompt
       dtp >>= Graph.fromdtp] |> List.choose id

    static member frompack(Pack(pi,nii,dti)) =
      let s = [pi >>= (Graph.frompackinfo >> Some)
               nii >>= (Graph.fromnhsii >> Some)
               dti >>= (Graph.fromdti >> Some)
               Some([ a !!"nicebnf:Pack" ])] |> List.choose id |> List.collect id
      blank !!"nicebnf:hasPack" s

    static member from (x:MedicinalProduct) =
      let sais = x.strengthOfActiveIngredient |> List.map Graph.fromsai
      let cds = x.controlledDrugs |> List.map Graph.fromcd
      let s = [Some(a Uri.MedicinalProductEntity)
               Some(x.ampid |> string |> Graph.dp "Ampid")
               x.title |> Graph.frommpt] @ sais @ cds
               |> List.choose id
      let ps = x.packs |> List.map Graph.frompack
      one !!"nicebnf:hasMedicinalProduct" (Uri.from x) (s @ ps)
