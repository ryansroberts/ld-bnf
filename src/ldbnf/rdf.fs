namespace Bnf
open FSharp.RDF
open FSharp.Data.Runtime

module Rdf =
  open prelude
  open resource
  open Bnf.Drug
  open Bnf.MedicinalForm
  open Assertion
  open rdf
  open Shared

  type Graph with
    static member ReallyEmpty xp =
      let vds = new VDS.RDF.Graph()
      xp |> List.iter (fun (p, (Uri.Sys ns)) -> vds.NamespaceMap.AddNamespace(p, ns))
      Graph vds

module RdfUris =
  open prelude
  open resource
  open Bnf.Drug
  open Bnf.MedicinalForm
  open Bnf.TreatmentSummary
  open Assertion
  open rdf
  open Shared

  type Uri with
    static member nicebnf = "http://ld.nice.org.uk/ns/bnf#"
    static member nicebnfClass = "http://ld.nice.org.uk/ns/bnf/"
    static member bnfsite = "http://bnf.nice.org.uk/"

    static member from (x:Drug) = !!(Uri.bnfsite + "drug/" + string x.id )
    static member from (x:DrugClass) = !!(Uri.bnfsite + "drugclass/" + string x.id )
    static member from (x:CMPI) = !!(Uri.bnfsite + "clinicalMedicinalProductInformation/" + string x.id )
    static member fromsec (x:Drug) (Id i) = !!(Uri.bnfsite + "drug/" + string x.id + "#" + i)
    static member fromsecdc (x:DrugClass) (Id i) = !!(Uri.bnfsite + "drugclass/" + string x.id + "#" + i)
    static member fromseccmpi (x:CMPI) (Id i) = !!(Uri.bnfsite + "clinicalMedicinalProductInformation/" + string x.id + "#" + i)

    static member from (x:MedicinalForm) = !!(Uri.bnfsite + "medicinalform/" + string x.id )
    static member from (x:MedicinalProduct) = !!(Uri.bnfsite + "medicinalproduct/" + string x.ampid)
    static member from (x:TreatmentSummary) = match x with | TreatmentSummary (i,_) -> !!(Uri.bnfsite + "treatmentsummary/" + string i)
    static member fromdc (s:string) = !!(Uri.bnfsite + "drugclass/"  + s)
    static member from (InteractionLink (l)) = !!(Uri.bnfsite + "interactions/" + l.Url)
    static member from (ConstituentDrug (l)) = !!(Uri.bnfsite + "constituentDrug/" + l.Url)
    static member from (Route s) = !!(Uri.nicebnfClass + "Route#" + (NameUtils.niceCamelName s))
    static member from (Indication s) = !!(Uri.nicebnfClass + "Indication#" + (NameUtils.niceCamelName s))
    static member fromc (Classification (Id s,_)) = !!(Uri.nicebnfClass + "Classification#" + s)
    static member fromfi (FundingIdentifier s) = !!(Uri.nicebnfClass + "FundingIdentifier#" + s)
    static member fromgrp (s:string) = !!(Uri.nicebnfClass + "PatientGroup#" + (NameUtils.niceCamelName s))
    static member from (TheraputicUse (s,_)) = !!(Uri.nicebnfClass + "TheraputicUse#" + (NameUtils.niceCamelName s))
    static member from (DomainOfEffect (s,_,_)) = !!(Uri.nicebnfClass + "DomainOfEffect#" + (NameUtils.niceCamelName (s.Value.Trim())))
    static member fromse (SideEffect s) = !!(Uri.nicebnfClass + "SideEffect#" + (NameUtils.niceCamelName s.Value.Value))
    static member fromfre (f:Frequency) = !!(Uri.nicebnfClass + "Frequency#" + (NameUtils.niceCamelName f.frequencyid))

    static member DrugClassEntity = !!(Uri.nicebnf + "DrugClass")
    static member CMPIEntity = !!(Uri.nicebnf + "ClinicalMedicinalProductInformation")
    static member DrugEntity = !!(Uri.nicebnf + "Drug")
    static member MedicinalFormEntity = !!(Uri.nicebnf + "MedicinalForm")
    static member MedicinalProductEntity = !!(Uri.nicebnf + "MedicinalProduct")
    static member InteractionEntity = !!(Uri.nicebnf + "Interaction")
    static member ConstituentDrugEntity = !!(Uri.nicebnf + "ConstituentDrug")
    static member ClassificationEntity = !!(Uri.nicebnfClass + "Classification")
    static member RouteEntity = !!(Uri.nicebnfClass + "Route")
    static member DomainOfEffectEntity = !!(Uri.nicebnfClass + "DomainOfEffect")
    static member TheraputicUseEntity = !!(Uri.nicebnfClass + "TheraputicUse")
    static member IndicationEntity = !!(Uri.nicebnfClass + "Indication")
    static member FundingIdentifierEntity = !!(Uri.nicebnfClass + "FundingIdentifier")
    static member PatientGroupEntity = !!(Uri.nicebnfClass + "PatientGroup")
    static member TreatmentSummaryEntity = !!(Uri.nicebnfClass + "TreatmentSummary")
