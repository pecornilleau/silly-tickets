type silly = nat ticket
type parameter =
      Deposit 
    | Withdraw of silly

type wallet_parameter =
    New_Ticket of silly

let tax_rate = 15n // in %

let create_silly () : silly =
    let n : nat = Tezos.amount / 1mutez in
    Tezos.create_ticket 1n n

let transaction_of_deposit (ticket:silly) =
    let sender_contract : wallet_parameter contract =
      match (Tezos.get_contract_opt (Tezos.sender) : wallet_parameter contract option) with
        Some contract -> contract
      | None -> (failwith "Contract not found. Maybe no wallet_parameter" : wallet_parameter contract) in
    Tezos.transaction (New_Ticket ticket) 0tez sender_contract

let deposit () : operation list = 
    let _check = if( Tezos.amount < 1tez) then failwith "no ticket for less than a tez" in    
    let ticket = create_silly () in
    let op = transaction_of_deposit ticket in
    []


let withdraw_amount_after_tax (requested:nat) = 
    let n = (requested * abs(100n - tax_rate)) / 100n in
    n * 1mutez

let transaction_of_withdraw (requested:nat)  = 
    let sender_contract : unit contract =
      match (Tezos.get_contract_opt (Tezos.sender) : unit contract option) with
        Some contract -> contract
      | None -> (failwith "Contract not found. Probably not an issue." : unit contract) in
    Tezos.transaction () (withdraw_amount_after_tax requested ) sender_contract

let withdraw (ticket:silly) : operation list = 
    let (_addr, (_payload, total)), _ticket = Tezos.read_ticket ticket in
    let op = transaction_of_withdraw total in
    [op]


(* ENDPOINTS *)
let main (action, _store : parameter * unit) : operation list * unit = 
    (match action with
          Deposit  -> deposit ()
        | Withdraw (ticket) -> withdraw (ticket))
    , ()

let test =    
  let _taddr, _, _ = Test.originate main () 0tez in
  assert true