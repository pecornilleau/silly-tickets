type silly = nat ticket
type storage = (address,silly) map
type parameter = Deposit 


(* ENDPOINTS *)
let main (action, store : parameter * storage) : operation list * storage = 
     match action with
          Deposit  -> [],Map.update Tezos.sender (Some (Tezos.create_ticket 1n 1n)) store


(* TEST OK *)
let test_initial_storage_empty =
  let taddr, _, _ = Test.originate main (Map.empty:storage) 0tez in
  // this assert works
  assert ((Map.size (Test.get_storage taddr)) = 0n)


(* ERROR *)
let test_storage_after_deposit =
  let taddr, _, _ = Test.originate main (Map.empty:storage) 0tez in
  let _gas_cons = Test.transfer_to_contract_exn (Test.to_contract taddr) Deposit 0tez in
  // this assert does NOT works : untranspilable (ticket nat) (Pair "KT1KG3JZXN2t322iq9WU6Q9S3VwGtsamP3zP" 1 1)
  assert ((Map.size (Test.get_storage taddr)) = 1n)