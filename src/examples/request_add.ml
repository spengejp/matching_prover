open SCaml
open Proof_market

let req = {
    program = Some "let add p s = p + s";
    specification = "The function `add` sums two integers.";
    deadline = Timestamp "2020-04-01T00:00:00Z";
    whitelist = None;
  }
