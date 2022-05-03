type silly = nat ticket
type parameter =
      Deposit 
    | Withdraw of silly

type wallet_parameter =
    New_Ticket of silly

type storage = silly option
    
let receive (ticket:silly) (store:storage) : storage = 
    match store with
        | None -> Some ticket
        | Some old_ticket -> 
            // if join fails (returns None), both tickets are lost
            Tezos.join_tickets (ticket,old_ticket) 

(* ENDPOINTS *)
let main (action, store : wallet_parameter * storage) : operation list * storage = 
    [] , 
    (match action with
          New_Ticket ticket  -> receive ticket store )
    

let test =    
  let _taddr, _, _ = Test.originate main (None : silly option) 0tez in
  assert true