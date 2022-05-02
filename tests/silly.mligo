#include "../contracts/silly.mligo"

let _u = Test.reset_state 5n ([] : tez list)

let test_initial_storage_empty =
  let initial_storage : storage = Map.empty in
  let taddr, _, _ = Test.originate main initial_storage 0tez in
  assert ((Map.size (Test.get_storage taddr)) = 0n)

let test_balance_after_deposit = 
  let taddr, _, _ = Test.originate main (Map.empty : storage) 0tez in
  let contr = Test.to_contract taddr in
  let deposit = 1000tez in
  let _gas_cons = Test.transfer_to_contract_exn contr Deposit deposit in
   assert ( Test.get_balance (Tezos.address contr) = deposit) 

let () = Test.log ("tax_rate",tax_rate)
let () = Test.log ("tax_mult",tax_mult)

let test_withdraw_after_deposit = 
  let taddr, _, _ = Test.originate main (Map.empty : storage) 0tez in
  let contr = Test.to_contract taddr in
  let my_source = Test.nth_bootstrap_account 2 in
  let _set_source = Test.set_source my_source in
  let deposit = 1000tez in
  let withdraw = 100n in
  let funds_before_deposit = Test.get_balance my_source in
  let _log = Test.log ("funds before",funds_before_deposit) in
  let gas_cons = Test.transfer_to_contract_exn contr Deposit deposit in
  let _log = Test.log ("gas consumption",gas_cons) in
  let funds_after_deposit = Test.get_balance my_source in
  let _log = Test.log ("funds after deposit",funds_after_deposit) in
  let _log = Test.log ("funds lost",funds_before_deposit - funds_after_deposit ) in
  let _log = Test.log ("deposit fee",funds_before_deposit - funds_after_deposit - 1000tez) in
  let gas_cons = Test.transfer_to_contract_exn contr (Withdraw withdraw) 0tez in
  let _log = Test.log ("gas consumption",gas_cons) in
  let _assert_contract_balance = assert ( Test.get_balance (Tezos.address contr) = 900tez + (tax_rate * 1tez)) in
  let funds_after_withdraw = Test.get_balance my_source in
  let _log = Test.log ("funds after withdrawal",funds_after_withdraw) in
  let gain = funds_after_withdraw - funds_after_deposit in
  let _log = Test.log ("funds gained",gain) in
  let _log = Test.log ("withdraw fee",(withdraw_amount_after_tax withdraw)-gain) in
  assert ( gain < (withdraw_amount_after_tax withdraw))

let test_balance_after_deposit_2 = 
  let taddr, _, _ = Test.originate main (Map.empty : storage) 0tez in
  let contr = Test.to_contract taddr in
  let _gas_cons = Test.transfer_to_contract_exn contr Deposit 1000tez in
  let _gas_cons = Test.transfer_to_contract_exn contr Deposit 500tez in
  assert ( Test.get_balance (Tezos.address contr) = 1500tez) 