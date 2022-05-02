type silly = nat ticket
type storage = (address,silly) map

type parameter =
  Deposit 
| Withdraw of nat

type return = operation list * storage

(* Taxes *)
// tax rate in %
let tax_rate = 15n
// tax  multiplier in tez
let tax_mult : tez = abs(100n - tax_rate) * 0.01tez
// taxes computation    
let withdraw_amount_after_tax (requested:nat):tez =
    requested * tax_mult

(* Storage manipulation *)
// let t_opt,store = find addr store in ...
let find (addr : address) (store : storage) : silly option * storage = 
    Map.get_and_update addr (None : silly option) store
// let store = update addr (Some ticket) store in ...
let update (addr : address) (ticket : silly option) (store : storage) : storage =
    Map.update addr ticket store 

(* creation of tickets : value is tez amount deposited (rounded) *)
// create
let create_silly (amnt:tez) : silly =
    let n : nat = amnt / 1tez in
    Tezos.create_ticket 1n n
// join
let join (t1_opt : silly option) (t2 :silly) : silly =
    match t1_opt with
        | None -> t2
        | Some t1 -> 
        (match Tezos.join_tickets (t1,t2) with
            | None -> (failwith "Could not join" : silly)
            | Some t -> t)
// substract
let substract_from_ticket (requested:nat) (ticket:silly) : silly option = 
    let (_addr, (_payload, total)), ticket = Tezos.read_ticket ticket in
    let _check = if (total < requested) then failwith "You don't have enough funds"  in  
    //  second ticket (with requested amount) is burned
    if total > requested then
        let t_left, _to_burn =
            match Tezos.split_ticket ticket ((abs(total - requested) : nat), requested) with
                None -> (failwith "failed to split" : silly * silly)
                | Some split_tickets -> split_tickets
        in Some t_left
    else None



(* operation *)
let transaction_of_withdraw (requested:nat)  = 
    let sender_contract : unit contract =
      match (Tezos.get_contract_opt (Tezos.sender) : unit contract option) with
        Some contract -> contract
      | None -> (failwith "Contract not found. Probably not an issue." : unit contract) in
    Tezos.transaction () (withdraw_amount_after_tax requested ) sender_contract


(* ACTIONS *)

(* Deposit 
    - deposited amount is XTZ sent with transaction
    - must be amount >= 1tez
    - ticket store at addr of sender
    - ticket joined with existing ticket*)
let deposit (store : storage) : return = 
    // check amount >= 1 
    let _check = if( Tezos.amount < 1tez) then failwith "no ticket for less than a tez" in    
    // create ticket
    let ticket = create_silly Tezos.amount in
    // join to existing ticket if any
    let old_ticket_opt,store = find Tezos.sender store in
    let new_ticket = join old_ticket_opt ticket in  
    let new_store = update Tezos.sender (Some new_ticket) store in
    [],new_store

(* Withdraw
    - can't send amount with transaction
    - can't withdrawn nothing
    - withdraw from ticket of sender
    - amount left (if any) is stored back
    - taxes are deduced from returned amount 
*)
let withdraw (store, requested : storage * nat) : return = 
    // checks
    let _check = if( Tezos.amount > 0tez) then failwith "fee is deduced from withdrawal" in
    let _check = if( requested = 0n) then failwith "can't withdraw nothing" in
    // check ticket
    let ticket_opt,store = find Tezos.sender store in
    let ticket : silly = match ticket_opt with
        | None -> (failwith "No ticket for you" : silly)
        | Some t -> t
    in
    // split
    let t_left = substract_from_ticket requested ticket in
    // store back if any (does nothing if None)
    let new_store = update Tezos.sender t_left store in
    // transfer amount
    let op = transaction_of_withdraw requested in
    [op],new_store   
    
(* ENDPOINTS *)
let main (action, store : parameter * storage) : return = 
     match action with
          Deposit  -> deposit (store)
        | Withdraw (y) -> withdraw (store, y)


