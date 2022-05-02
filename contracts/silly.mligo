type silly = nat ticket
type storage = (address,silly) map

type parameter =
  Deposit 
| Withdraw of nat

type return = operation list * storage

// tax rate in %
let tax_rate = 15n
// tax  multiplier in tez
let tax_mult : tez = 1tez - (tax_rate * 0.01tez)

let create_silly (amnt:tez) : silly =
    let n : nat = amnt / 1tez in
    Tezos.create_ticket 1n n

let find (addr : address) (store : storage) : silly option * storage = 
    Map.get_and_update addr (None : silly option) store

let update (addr : address) (ticket : silly) (store : storage) : storage =
    let new_store = Map.update addr (Some ticket) store in
    new_store

let join (t1_opt : silly option) (t2 :silly) : silly =
    match t1_opt with
        | None -> t2
        | Some t1 -> 
        (match Tezos.join_tickets (t1,t2) with
            | None -> (failwith "Could not join" : silly)
            | Some t -> t)

let deposit (store : storage) : return = 
    let _assert = if( Tezos.amount < 1tez) then failwith "no ticket for nothing" in    
    let ticket = create_silly Tezos.amount in(* 
    let copy = ticket in
    let store = Map.update Tezos.self_address  (Some copy) store in *)
    let old_ticket_opt,store = find Tezos.sender store in(* 
    let old_ticket_opt = Map.find_opt Tezos.sender store in *)
    let new_ticket = join old_ticket_opt ticket in  
    let new_store = update Tezos.sender new_ticket store in(* 
    let copy,new_store = Map.get_and_update ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address) (Some new_ticket) store in
    match copy with
        | None -> failwith "toto"
        | Some s -> 
    let _,new_new_store = Map.get_and_update ("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" : address)  (Some s) new_store in  *)
    [],new_store
    
let withdraw_amount_after_tax (requested:nat):tez =
    requested * tax_mult

let transaction_of_withdraw (requested:nat):operation = 
    let sender_contract : unit contract =
      match (Tezos.get_contract_opt (Tezos.sender) : unit contract option) with
        Some contract -> contract
      | None -> (failwith "Contract not found." : unit contract) in
    Tezos.transaction () (withdraw_amount_after_tax requested ) sender_contract

let withdraw (store, requested : storage * nat) : return = 
    // check amount (0)
    let _assert = if( Tezos.amount > 0tez) then failwith "fee is deduced from withdrawal" in

    // check ticket
    let ticket_opt,store = find Tezos.sender store in
    let ticket : silly = match ticket_opt with
        | None -> (failwith "No ticket for you" : silly)
        | Some t -> t
    in

    // split
    let (_addr, (_payload, total)), ticket = Tezos.read_ticket ticket in
    let _assert = if (total < requested) then failwith "You don't have enough funds"  in  
    //  second ticket (with requested amount) is burned
    let t_left, _to_burn =
        match Tezos.split_ticket ticket ((abs(total - requested) : nat), requested) with
            None -> (failwith "failed to split" : silly * silly)
            | Some split_tickets -> split_tickets
    in

    // transfer amount
    let new_store = 
        if total > requested then
            // there is some left to store back
            update Tezos.sender t_left store 
        else
            // nothing left 
            store
    in
    let op = transaction_of_withdraw requested in
    [op],new_store   
    
let main (action, store : parameter * storage) : return = 
     match action with
          Deposit  -> deposit (store)
        | Withdraw (y) -> withdraw (store, y)


