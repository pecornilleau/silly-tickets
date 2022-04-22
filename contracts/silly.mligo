type silly = nat ticket
type storage = (address,silly) map

type parameter =
  Deposit 
| Withdraw of nat

type return = operation list * storage

let create_silly (amnt:tez) : silly =
    let n : nat = amnt / 1tez in
    Tezos.create_ticket 1n n

let find (addr : address) (store : storage) : silly option * storage = 
    Map.get_and_update addr (None : silly option) store

let update (addr : address) (ticket : silly) (store : storage) : storage =
    let _ , new_store = Map.get_and_update addr (Some ticket) store in
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
    let ticket = create_silly Tezos.amount in
    let old_ticket_opt,store = find Tezos.sender store in
    let new_ticket = join old_ticket_opt ticket in  
    let new_store = update Tezos.sender new_ticket store in
    [],new_store
    
    

let operation_of_withdraw (requested:nat):operation = 
    let sender_contract : unit contract =
      match (Tezos.get_contract_opt (Tezos.sender) : unit contract option) with
        Some contract -> contract
      | None -> (failwith "Contract not found." : unit contract) in
    Tezos.transaction () (requested * 0.85tez) sender_contract

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
    let (_addr, (_payload, n)), ticket = Tezos.read_ticket ticket in
    let _assert = if (n < requested) then failwith "You don't have enough funds"  in  

    let t_left, _ =
        match Tezos.split_ticket ticket ((abs(n - requested) : nat), requested) with
            None -> (failwith "failed to split" : silly * silly)
            | Some split_tickets -> split_tickets
    in
    // transfer amount
    let new_store = 
        if n > requested then
            update Tezos.sender t_left store 
        else store
    in
    let op = operation_of_withdraw requested in
    [op],new_store   
    


let main (action, store : parameter * storage) : return = 
     match action with
          Deposit  -> deposit (store)
        | Withdraw (y) -> withdraw (store, y)
