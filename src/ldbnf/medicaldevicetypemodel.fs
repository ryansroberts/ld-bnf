namespace Bnf
open FSharp.Data
open prelude

module MedicalDeviceType =
  open Shared
  open Drug
  open MedicinalForm

  type DeviceDescription = | DeviceDescription of Id * drugProvider.Sectiondiv

  type ComplicanceStandards = | ComplicanceStandards of Id * drugProvider.Sectiondiv

  type ClinicalMedicalDeviceInformationGroup ={
     id:Id;
     title:Title;
     sections:MonographSection list;
     products:MedicinalProduct list;
     description:DeviceDescription option;
     complicance:ComplicanceStandards option}

  type MedicalDeviceType = {
     id:Id;
     title:Title;
     groups:ClinicalMedicalDeviceInformationGroup list;
    }

module MedicalDeviceTypeParser =
  open Shared
  open Drug
  open DrugParser
  open MedicinalForm
  open MedicinalFormParser
  open MedicalDeviceType

  let sections (x:drugProvider.Topic) =
    match x.Body with
    | Some (b) -> b.Sections
    | _ -> failwith "body is required"

  let firstsd c (x:drugProvider.Topic) =
    let sd = x |> sections |> Array.collect (fun s -> s.Sectiondivs)
    c(Id(x.Id),sd.[0])

  type DeviceDescription with
    static member from  = firstsd DeviceDescription >> Some

  type ComplicanceStandards with
    static member from = firstsd ComplicanceStandards >> Some

  type ClinicalMedicalDeviceInformationGroup with
    static member list (x:drugProvider.Topic) =
      x.Topics |> Array.map ClinicalMedicalDeviceInformationGroup.from
    static member from (x:drugProvider.Topic) =
      let mss = x.Topics |> Array.map MonographSection.section |> Array.choose id |> Array.toList

      let mps = x.Topics
                 |> Array.choose (withoc "clinicalMedicinalProducts")
                 |> Array.collect sections
                 |> Array.choose (withoco "medicinalProduct")
                 |> Array.map MedicinalProduct.from
                 |> Array.toList

      let des = x.Topics |> Array.tryPick (withoc "deviceDescription") >>= DeviceDescription.from
      let com = x.Topics |> Array.tryPick (withoc "complianceStandards") >>= ComplicanceStandards.from

      {id=Id(x.Id); title=Title(x.Title.Value.Value); sections=mss; products=mps; description = des; complicance = com;}

  type MedicalDeviceType with
    static member parse (x:drugProvider.Topic) =
      let gs = x.Topics |> Array.collect ClinicalMedicalDeviceInformationGroup.list |> Array.toList
      {id=Id(x.Id); title=Title(x.Title.Value.Value); groups=gs}
