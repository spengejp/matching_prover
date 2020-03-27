open SCaml
open Proof_market

type storage = {
    people:          people;
    requests:        (nat, request_status) big_map;
    next_request_id: nat;
  }

let get_role_of_sender storage =
  Map.get (Global.get_sender ()) storage.people

(* A requester calls this entry point to register a request *)
let [@entry] request req s =
  begin
    match get_role_of_sender s with
    | Some Requester -> ()
    | _ -> failwith "You are not registered as a requester."
  end;

  let id = s.next_request_id in
  let budget = Global.get_amount () in
  let request_status = {
      id;
      request = req;
      requester = Global.get_sender ();
      prover = None;
      proof = None;
      budget;
      judge = None;
    }
  in
  ([],
   {s with
     requests = BigMap.update id (Some request_status) s.requests;
     next_request_id = id +^ (Nat 1)})

(* A prover calls this entry point to submit proof *)
let [@entry] submit_proof (id, proof) s =
  begin
    match get_role_of_sender s with
    | Some Prover -> ()
    | _ -> failwith "You are not registered as a prover."
  end;

  let prover = Global.get_sender () in
  
  let request_status =
    match BigMap.get id s.requests with
    | None -> failwith ("No request of the id", id)
    | Some rs -> rs
  in

  (* check whether the request is already done or not *)
  begin
    match request_status.prover with
    | None -> ()
    | Some _ -> failwith "The request is already taken."
  end;

  (* check whitelist *)
  begin
    match request_status.request.whitelist with
    | Some w ->
       if not (Set.mem prover w)
       then failwith "You are not in the whitelist of the request."
    | None -> ()
  end;

  (* check deadline *)
  let t = Global.get_now () in
  if request_status.request.deadline < t
  then failwith "Request was closed";

  let new_request_status = {request_status with prover = Some prover; proof; } in
  ([], {s with requests = BigMap.update id (Some new_request_status) s.requests})

(* requester calls this entry point to pay or not pay for the proof *)
let [@entry] judge (id, judge) s =
  let request_status =
    match BigMap.get id s.requests with
    | None -> failwith "No request of the id"
    | Some rs -> rs
  in

  (* check the sender is a owner of the request or not *)
  if request_status.requester <> Global.get_sender ()
  then failwith "You are not the requester of the request of the id";

  let prover = match request_status.prover with
    | None   -> failwith "This request is not proved"
    | Some p -> p
  in
  
  begin
    match request_status.judge with
    | Some _ -> failwith "You already judged"
    | None   -> ()
  end;
  
  match judge with
  | ProofApproved ->
     let new_request_status = {request_status with judge = Some judge} in
     let prover_contract =
       match Contract.contract prover with
       | None -> failwith "Invalid address"
       | Some c -> c
     in
     ([Operation.transfer_tokens () request_status.budget prover_contract],
      {s with requests = BigMap.update id (Some new_request_status) s.requests})
  | ProofDenied _ ->
     let requester_contract =
       match Contract.contract request_status.requester with
       | None -> failwith "Invalid address"
       | Some c -> c
     in
     let new_request_status = {request_status with judge = Some judge} in
     ([Operation.transfer_tokens () request_status.budget requester_contract],
      {s with requests = BigMap.update id (Some new_request_status) s.requests})

(* requester calls this entry point to retrieve money, if the deadline is over *)
let deadline_is_over_without_proof id s =
  let request_status =
    match BigMap.get id s.requests with
    | None -> failwith "No request of the id"
    | Some rs -> rs
  in

  (* check the sender is a owner of the request or not *)
  if request_status.requester <> Global.get_sender ()
  then failwith "You are not the requester of the request of the id";

  begin
    match request_status.proof with
    | Some _ -> failwith "Proof exists"
    | None -> ()
  end;

  let requester_contract =
    match Contract.contract request_status.requester with
    | None -> failwith "Invalid address"
    | Some c -> c
  in

  let budget = request_status.budget in
  let new_request_status = {request_status with budget = Tz 0.0} in
  ([Operation.transfer_tokens () budget requester_contract],
   {s with requests = BigMap.update id (Some new_request_status) s.requests})



(* The followings are for admins *)

let [@entry] register (addr, role) s =
  match get_role_of_sender s with
  | Some Admin ->
     (* check whether the address is already registered or not *)
     if Map.mem addr s.people
     then failwith "the address is already registered";
     ([], {s with people = Map.update addr role s.people})
  | _ -> failwith "You are not an administorator."

let [@entry] ban addr s =
  match get_role_of_sender s with
  | Some Admin -> ([], {s with people = Map.update addr None s.people})
  | _ -> failwith "You are not an administorator";
