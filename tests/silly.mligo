#include "../with_storage/silly.mligo"

let _u = Test.reset_state 5n ([] : tez list)
let debug = false 

let test_initial_storage_empty =
  let () = if debug then Test.log "test_initial_storage_empty" in
  let initial_storage : storage = Map.empty in
  let taddr, _, _ = Test.originate main initial_storage 0tez in
  let () = if debug then Test.log ("contract address", Tezos.address (Test.to_contract taddr)) in
  assert ((Map.size (Test.get_storage taddr)) = 0n)




let test_balance_after_deposit = 
  let () = if debug then Test.log "test_balance_after_deposit" in
  let taddr, _, _ = Test.originate main (Map.empty : storage) 0tez in
  let () = if debug then Test.log ("contract address", Tezos.address (Test.to_contract taddr)) in
  let contr = Test.to_contract taddr in
  let deposit = 1000tez in
  let _gas_cons = Test.transfer_to_contract_exn contr Deposit deposit in
  // the contract's balance is increased by "deposit"
  assert ( Test.get_balance (Tezos.address contr) = deposit) 




let test_withdraw_after_deposit = 
  let () = if debug then Test.log "test_withdraw_after_deposit" in
  let taddr, _, _ = Test.originate main (Map.empty : storage) 0tez in
  let () = if debug then Test.log ("contract address", Tezos.address (Test.to_contract taddr)) in
  let contr = Test.to_contract taddr in
  let my_source = Test.nth_bootstrap_account 2 in
  let _set_source = Test.set_source my_source in
  let deposit = 1000tez in
  let withdraw = 100n in
  let funds_before_deposit = Test.get_balance my_source in
  let _log = if debug then Test.log ("funds before",funds_before_deposit) in
  let gas_cons = Test.transfer_to_contract_exn contr Deposit deposit in
  let _log = if debug then Test.log ("gas consumption",gas_cons) in
  let funds_after_deposit = Test.get_balance my_source in
  let _log = if debug then Test.log ("funds after deposit",funds_after_deposit) in
  let _log = if debug then Test.log ("funds lost",funds_before_deposit - funds_after_deposit ) in
  let _log = if debug then Test.log ("deposit fee",funds_before_deposit - funds_after_deposit - 1000tez) in
  let gas_cons = Test.transfer_to_contract_exn contr (Withdraw withdraw) 0tez in
  let _log = if debug then Test.log ("gas consumption",gas_cons) in
  // the contract has kept the taxes
  let _assert_contract_balance = assert_with_error 
    ( Test.get_balance (Tezos.address contr) = 900tez + (tax_rate * 1tez)) 
    "the contract should have kept the taxes"
  in
  let funds_after_withdraw = Test.get_balance my_source in
  let _log = if debug then Test.log ("funds after withdrawal",funds_after_withdraw) in
  let gain = funds_after_withdraw - funds_after_deposit in
  let _log = if debug then Test.log ("funds gained",gain) in
  let _log = if debug then Test.log ("withdraw fee",(withdraw_amount_after_tax withdraw)-gain) in
  // the user received at most "withdrawn amount - taxes"
  assert_with_error 
    ( gain < (withdraw_amount_after_tax withdraw)) 
    "the user should have received at most 'withdrawn amount - taxes'"

let test_balance_after_2_deposit = 
  let () = if debug then Test.log "test_balance_after_2_deposit" in
  let taddr, _, _ = Test.originate main (Map.empty : storage) 0tez in
  let contr = Test.to_contract taddr in
  let _gas_cons = Test.transfer_to_contract_exn contr Deposit 1000tez in
  let _gas_cons = Test.transfer_to_contract_exn contr Deposit 500tez in
  assert_with_error 
    ( Test.get_balance (Tezos.address contr) = 1500tez) 
    "contract balance should have all deposits"

let test_empty_after_full_withdraw = 
  let () = if debug then Test.log "test_empty_after_full_withdraw" in
  let taddr, _, _ = Test.originate main (Map.empty : storage) 0tez in
  let contr = Test.to_contract taddr in
  let () = if debug then Test.log ("contract address", Tezos.address contr) in
  let _set_source = Test.set_source (Test.nth_bootstrap_account 2) in
  let deposit = 1000tez in
  let withdraw = 1000n in
  let _gas_cons = Test.transfer_to_contract_exn contr Deposit deposit in
  // amount in escrow = deposited amount
  let _check = assert_with_error 
    ( Test.get_balance (Tezos.address contr) = deposit) 
    "amount in escrow != deposited amount"
  in
  let _gas_cons = Test.transfer_to_contract_exn contr (Withdraw withdraw) 0tez in
  // amount left : 15% of initial deposit (if initial deposit is a natural number of tez)
  let _check = assert_with_error
    (Test.get_balance (Tezos.address contr) = ((deposit/ 1tez) * (tax_rate * 0.01tez))) 
    "amount left should be 15% of initial deposit (if initial deposit is a natural number of tez)"
  in
  // after withdrawal of all deposited funds, storage should be empty
  assert_with_error 
    ((Map.size (Test.get_storage taddr)) = 0n)
    "contract storage should be empty"