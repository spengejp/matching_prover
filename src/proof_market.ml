open SCaml

(* main information of proof request *)
type request = {
    program:       string option;      (* sample program that will be proved *)
    specification: string;             (* specification of the program *)
    deadline:      timestamp;          (* if deadline is over, the request is canceled *)
    whitelist:     address set option; (* to specify provers *)
  }

(* result of request *)
type judge = ProofApproved | ProofDenied of string

type request_status = {
    id:        nat;            (* id of a request *)
    request:   request;        (* main information of the request *)
    requester: address;        (* address of the requester *)
    prover:    address option; (* address of a prover if exists *)
    proof:     string option;  (* a proof if submitted *)
    budget:    tz;             (* budget of the request *)
    judge:     judge option;   (* final result of request *)
  }

(* database of people *)
type role = Requester | Prover | Admin
type people = (address, role) map
