namespace Bnf

open System
open System.IO
open FSharp.Data
open Nessos.Argu
open FSharp.Control
open FSharpx.Control
open System.Text.RegularExpressions
open FSharp.Collections.ParallelSeq

open Shared

open System.Xml.Linq
open FSharp.Data
open Bnf.Drug
open Bnf.DrugParser
open Bnf.TreatmentSummary
open Bnf.TreatmentSummaryParser
open Bnf.DrugClassification
open Bnf.DrugClassificationParser
open Bnf.MedicinalForm
open Bnf.MedicinalFormParser
open Bnf.BorderlineSubstance
open Bnf.BorderlineSubstanceParser
open Bnf.Interaction
open Bnf.InteracitonParser
open Bnf.MedicalDeviceType
open Bnf.MedicalDeviceTypeParser
open FSharp.RDF

open resource
open Assertion
open rdf
open Bnf.DrugRdf
open Bnf.MedicinalFormRdf
open Bnf.TreatmentSummary
open Bnf.TreatmentSummaryRdf
open Bnf.DrugClassificationRdf
open Bnf.BorderlineSubstanceRdf
open Bnf.InteractionRdf
open Bnf.MedicalDeviceTypeRdf


module Iterator =

  let private fileasync (fileName : string) = async { use! file = File.AsyncOpenText fileName
                                                      return file.ReadToEnd() }
  let private file (fn:string) = File.OpenText fn

  let private xmlFromFileSynch (fileName : string) =
    let file = File.OpenText fileName
    drugProvider.Load file

  /// writes a file
  let private toFile fileName (contents : string) =
    async {
      File.AsyncWriteAllText(fileName, contents) |> Async.Start
      return ()
    }

  let private toFileSynch fileName (contents : string) =
    File.WriteAllText(fileName, contents)
    ()

  let (++) a b = System.IO.Path.Combine(a, b)

  type Arguments =
    | [<Mandatory>] XmlDirectory of string
    | [<Mandatory>] OutputDirectory of string
  with
    interface IArgParserTemplate with
      member s.Usage = 
        match s with
          | XmlDirectory _ -> "Specify a directoy for the source xml"
          | OutputDirectory _ -> "Specify an output directory for the ttl"

  let generate f =
    //get the type from the filename, somehow
    let t = Directory.GetParent(f).Name
    use fi = file f
    //parse in different ways for differnt types
    let m = match t with
            | "drug" -> fi |> drugProvider.Load |> Drug.parse |> Graph.from |> Some
            | "medicinalForm" -> fi |> drugProvider.Load |> MedicinalForm.parse |> Graph.from |> Some
            | "treatmentSummary" -> fi |> tsProvider.Load |> TreatmentSummary.parse |> Graph.from |> Some
            | "drugClassifications" -> fi |> dcProvider.Load |> DrugClassifications.parse |> Graph.from |> Some
            | "drugClass" -> fi |> drugProvider.Load |> DrugClass.parse |> Graph.from |> Some
            | "clinicalMedicinalProductInformation" -> fi |> drugProvider.Load |> CMPI.parse |> Graph.from |> Some
            | "borderlineSubstance" -> fi |> bsProvider.Load |> BorderlineSubstance.parse |> Graph.from |> Some
            | "interaction" -> fi |> inProvider.Load |> InteractionList.parse |> Graph.from |> Some
            | "medicalDeviceType" -> fi |> drugProvider.Load |> MedicalDeviceType.parse |> Graph.from |> Some
            | _ -> None

    match m with
        | Some graph ->
          let sb = new System.Text.StringBuilder()
          use tw = toString sb
          graph |> Graph.writeTtl tw |> ignore
          let fn = Path.GetFileName f
          let nfn = Path.ChangeExtension(fn,"ttl")
          Some(sb.ToString(),nfn,t)
        | _ -> None

  let apply o f =
      printfn "%s" f
      match (generate f) with
       | Some(text,fn,t) ->
           let dir = (o ++ t)
           if (not(Directory.Exists(dir))) then
             Directory.CreateDirectory(dir) |> ignore
           let fn = (dir ++ fn)
           toFileSynch fn text
           Some(fn)
       | None -> None

  [<EntryPoint>]
  let main args = 
    let parser = ArgumentParser.Create<Arguments>()
    let useage = parser.Usage()
    let results = parser.ParseCommandLine(args,errorHandler=ProcessExiter())
    let xmlDirectory = results.GetResult <@ XmlDirectory @>
    let outputDirectory = results.GetResult <@ OutputDirectory @>
    printfn "%s" xmlDirectory

    let fs = Directory.EnumerateFiles(xmlDirectory,"*.*",SearchOption.AllDirectories)
    fs |> Seq.map (apply outputDirectory)
       |> Seq.choose id
       |> Seq.iter (fun s -> printfn "%s" s)
    //AsyncSeq.ofSeq fs
    //    |> AsyncSeq.map (apply outputDirectory)
    //    |> AsyncSeq.iter (fun s -> printfn "%s" (Async.RunSynchronously s))
    //    |> Async.RunSynchronously
    0
