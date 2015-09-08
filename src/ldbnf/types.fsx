#I "../../packages/FSharp.Data/lib/net40/"
#I "../../packages/FParsec/lib/net40-client/"
#r "../../packages/FParsec/lib/net40-client/FParsec.dll"
#r "../../packages/FParsec/lib/net40-client/FParsecCS.dll"
#r "../../packages/FSharp.Data/lib/net40/FSharp.Data.dll"
#r "../../packages/Arachne.Core/lib/net45/Arachne.Core.dll"


#r "../../packages/Arachne.Uri/lib/net45/Arachne.Uri.dll"
#r "System.Xml.Linq.dll"
#load "./drugmodel.fs"

open FSharp.Data
open Arachne.Uri
open Bnf.Drug
open Bnf.DrugParser

let drugXml = drugProvider.GetSample()

let parse (x:drugProvider.Topic) =
    let interactionLinks = InteractionLinks(x.Body.P.Xrefs |> Array.map InteractionLink.from)

    let classifications = Classifications(x.Body.Datas
                        |> Array.filter (hasName "classifications")
                        |> Array.collect (fun c -> c.Datas)
                        |> Array.collect (fun c -> c.Datas)
                        |> Array.map classFn
                        |> Array.choose id)

    let vtmid =
        (x.Body.Datas
        |> Array.filter ( hasName "vtmid" )
        |> Array.map (value >> Option.map Vtmid)
        |> Array.choose id).[0]

    let inline sectionFn x =
        match x with
            | HasOutputClass "indicationsAndDose" -> Some(IndicationsAndDose (IndicationsAndDose.from x))
            | HasOutputClass "pregnancy" -> Some(generalInformation MonographSection.Pregnancy x)
            | HasOutputClass "breastFeeding" -> Some(generalInformation MonographSection.BreastFeeding x)
            | HasOutputClass "hepaticImpairment" -> Some(generalInformation MonographSection.HepaticImpairment x)
            | HasOutputClass "renalImpairment" -> Some(renalImpairment x)
            | HasOutputClass "patientAndCarerAdvice" -> Some(patientAndCarerAdvice x)
            | HasOutputClass "medicinalForms" -> Some(medicinalForms x)
            | _ -> None

    let sections =
        x.Topics |> Array.map sectionFn |> Array.choose id

    Drug(interactionLinks,classifications,vtmid,sections)


let drugModel = parse drugXml
