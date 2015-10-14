namespace Bnf

module prelude = 
    let (|?) = defaultArg

    let (>=>) a b x =
      match (a x) with
        | Some x -> b x
        | None -> None

    let (>>|) a b x =
      match (a x) with
        | Some x -> Some(b x)
        | None -> None

    let (>>=) a b = Option.bind b a
