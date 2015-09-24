namespace Bnf

open System
open System.IO
open FSharp.Data
open Nessos.Argu
open FSharp.Control
open FSharpx.Control
open System.Text.RegularExpressions


module Iterator =
  let private fromFile (fileName : string) = async { use! file = File.AsyncOpenText
                                                                  fileName
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

  let generate x =
    async {
        return ("content",x ++ "filename")
    }

  let apply o f =
    async {
      let! (text,fn) = generate f
      let fn = (o ++ fn)
      toFile fn text |> Async.Start
      return fn
    }

  let main args = 
    let parser = ArgumentParser.Create<Arguments>()
    let usage = parser.Usage()
    let results = parser.Parse args
    let xmlDirectory = results.GetResult <@ XmlDirectory @>
    let outputDirectory = results.GetResult <@ OutputDirectory @>

    let fs = Directory.EnumerateDirectories xmlDirectory
    AsyncSeq.ofSeq fs
        |> AsyncSeq.map (apply outputDirectory)
        |> AsyncSeq.iter (fun s -> printfn "%s" (Async.RunSynchronously s))
        |> Async.RunSynchronously

    0
