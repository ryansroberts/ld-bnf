namespace Bnf

open System
open System.IO
open FSharp.Data
open Nessos.Argu
open FSharp.Control
open FSharpx.Control
open System.Text.RegularExpressions

open System.Xml.Linq
open FSharp.Data
open Bnf.Drug
open Bnf.DrugParser
open FSharp.RDF

open resource
open Assertion
open rdf
open Bnf.DrugRdf


module Iterator =
  let private xmlFromFile (fileName : string) = async { use! file = File.AsyncOpenText fileName
                                                        return drugProvider.Load file }

  let private fromFile (fileName : string) = async { use! file = File.AsyncOpenText fileName
                                                     return file.ReadToEnd() }

  /// writes a file
  let private toFile fileName (contents : string) =
    async {
      File.AsyncWriteAllText(fileName, contents) |> Async.Start
      return ()
    }

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

  let generate f o =
    async {
        let! d = xmlFromFile f
        let m = parse d
        let s = ""
        let sb = new System.Text.StringBuilder(s)

        let graph = Graph.from m

        graph |> Graph.writeTtl (toString sb) |> ignore
        let fn = Path.GetFileNameWithoutExtension f

        return (sb.ToString(),o ++ fn ++ ".ttl")
    }

  let apply o f =
    async {
      let! (text,fn) = generate f o
      let fn = (o ++ fn)
      toFile fn text |> Async.Start
      return fn
    }

  [<EntryPoint>]
  let main args = 
    let parser = ArgumentParser.Create<Arguments>()
    let useage = parser.Usage()
    let results = parser.ParseCommandLine(args,errorHandler=ProcessExiter())
    let xmlDirectory = results.GetResult <@ XmlDirectory @>
    let outputDirectory = results.GetResult <@ OutputDirectory @>

    let fs = Directory.EnumerateDirectories xmlDirectory
    AsyncSeq.ofSeq fs
        |> AsyncSeq.map (apply outputDirectory)
        |> AsyncSeq.iter (fun s -> printfn "%s" (Async.RunSynchronously s))
        |> Async.RunSynchronously
    0
