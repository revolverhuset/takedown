module Parsing
open System.Text.RegularExpressions;

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let tryParseInt str =
    match System.Int32.TryParse(str) with
    | (true, int) -> Some int
    | _ -> None

let tryParseDecimal str =
    match System.Decimal.TryParse(str) with
    | (true, decimal) -> Some decimal
    | _ -> None

let (|Int|_|) str = tryParseInt
let (|Decimal|_|) str = tryParseDecimal
