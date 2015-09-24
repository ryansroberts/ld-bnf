namespace Bnf

open System
open System.IO
open FSharp.Data
open Nessos.Argu
open FSharp.Control
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

  type Arguments =
    | [<Mandatory>] XmlDirectory of string
    | [<Mandatory>] OutputDirectory of string
  with
    interface IArgParserTemplate with
      member s.Usage = 
        match s with
          | XmlDirectory _ -> "Specify a directoy for the source xml"
          | OutputDirectory _ -> "Specify an output directory for the ttl"

  let parser = ArgumentParser.Create<Arguments>()
  let usage = parser.Usage()
