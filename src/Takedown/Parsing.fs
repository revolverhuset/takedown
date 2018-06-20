module Parsing
open System
open System.Text.RegularExpressions;

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let tryParseInt str =
    match Int32.TryParse(str : string) with
    | (true, int) -> Some int
    | _ -> None

let tryParseDecimal str =
    match Decimal.TryParse(str: string) with
    | (true, decimal) -> Some decimal
    | _ -> None

let isNullOrEmpty s = String.IsNullOrEmpty s
let hasContent s = not (isNullOrEmpty s)
