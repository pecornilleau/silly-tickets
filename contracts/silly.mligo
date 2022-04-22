type silly = nat ticket
type storage = (address,silly) map

type parameter =
  Deposit 
| Withdraw of nat

type return = operation list * storage

let create_silly (amnt:tez) : silly =
    let n : nat = amnt / 1tez in
    Tezos.create_ticket 1n n

let join_or_fail (t1 , t2 : silly * silly) : silly =
    match Tezos.join_tickets (t1,t2) with
        | None -> (failwith "Could not join" : silly)
        | Some t -> t

let join_with_existing (ticket , store : silly * storage) : silly = 
    match Map.find_opt Tezos.sender store with
        | None -> ticket
        | Some t -> join_or_fail (t,ticket)
    

let deposit (store : storage) : return = 
    if( Tezos.amount < 1tez) then 
        (failwith "no ticket for nothing" : return)
    else  (
        let ticket = create_silly Tezos.amount in
        let ticket = join_with_existing (ticket,store) in  
        let _ , new_store = Map.get_and_update Tezos.sender (Some ticket) store in
        [],new_store
    )
    

let operation_of_withdraw (requested:nat):operation = 
    let sender_contract : unit contract =
      match (Tezos.get_contract_opt (Tezos.sender) : unit contract option) with
        Some contract -> contract
      | None -> (failwith "Contract not found." : unit contract) in
    Tezos.transaction () (requested * 0.85tez) sender_contract

let withdraw (store, requested : storage * nat) : return = 
    // check amount (0)
    if( Tezos.amount > 0tez) then (failwith "fee is deduced from withdrawal" : return)
    else (

    // check ticket
    let ticket : silly = match Map.find_opt Tezos.sender store with
        | None -> (failwith "No ticket for you" : silly)
        | Some t -> t
    in
    // split
    let (_addr, (_payload, n)), ticket = Tezos.read_ticket ticket in
    if (n < requested) then (failwith "You don't have enough funds" : return)
    else (

    let t_left, _ =
        match Tezos.split_ticket ticket ((abs(n - requested) : nat), requested) with
            None -> (failwith "failed to split" : silly * silly)
            | Some split_tickets -> split_tickets
    in
    // transfer amount
    let new_store = 
        if n > requested then
            let _ , new_store = Map.get_and_update Tezos.sender (Some t_left) store in
            new_store
        else store
    in
    let op = operation_of_withdraw requested in
    [op],new_store
    )
    )


let main (action, store : parameter * storage) : return = 
     match action with
          Deposit  -> deposit (store)
        | Withdraw (y) -> withdraw (store, y)
 