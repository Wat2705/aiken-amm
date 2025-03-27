use aiken/collection/list
use aiken/math.{sqrt}
use cardano/address.{Address, Script}
use cardano/assets.{
  PolicyId, ada_asset_name, ada_policy_id, add, from_asset, quantity_of,
}
use cardano/transaction.{Input, Output, OutputReference, Transaction}
use mocktail
use mocktail/virgin_address.{mock_pub_key_address, mock_script_address}
use mocktail/virgin_output_reference.{mock_utxo_ref}

pub type AmmRedeemer {
  CreatePool
  Swap
  Deposit
  Withdraw
}

pub type Datum {
  lp_amount: Int,
  amm_policy_id: PolicyId,
}

pub const asset_a_policy_id = ada_policy_id

pub const asset_a_asset_name = ada_asset_name

pub const asset_b_policy_id =
  #"e16c2dc8ae937e8d3790c7fd7168d7b994621ba14ca11415f39fed72"

pub const asset_b_asset_name = #"4d494e"

pub const locked_ada = 3_000_000

pub const lp_token_name = #"4c505f544f4b454e"

pub const auth_token_name = #"415554485f544f4b454e"

fn check_swap(own_ref: OutputReference, tx: Transaction) -> Bool {
  let Transaction { inputs, outputs, .. } = tx
  expect Some(utxo) =
    list.find(
      inputs,
      fn(input) {
        let Input { output_reference, .. } = input
        output_reference == own_ref
      },
    )
  let Input {
    output: Output { address: input_address, value: input_value, .. },
    ..
  } = utxo
  expect Address { payment_credential: Script(amm_policy_id), .. } =
    input_address

  expect Some(amm_output) =
    list.find(
      outputs,
      fn(output) {
        quantity_of(output.value, amm_policy_id, auth_token_name) == 1
      },
    )

  let x_swap = quantity_of(input_value, asset_a_policy_id, asset_a_asset_name)
  let x_pool =
    quantity_of(amm_output.value, asset_a_policy_id, asset_a_asset_name) - locked_ada
  let y_pool =
    quantity_of(amm_output.value, asset_b_policy_id, asset_b_asset_name)
  expect Some(y_swap_output) =
    list.find(
      outputs,
      fn(output) {
        quantity_of(output.value, asset_b_policy_id, asset_b_asset_name) == y_pool - x_pool * y_pool / (
          x_pool + x_swap
        )
      },
    )
  let y_swap =
    quantity_of(y_swap_output.value, asset_b_policy_id, asset_b_asset_name)
  expect and {
      x_pool > 0,
      y_pool > 0,
      x_swap > 0,
      y_swap > 0,
    }
  ( x_swap + x_pool ) / x_pool == y_pool / ( y_pool - y_swap )
}

fn check_spend_deposit(own_ref: OutputReference, tx: Transaction) {
  let Transaction { inputs, outputs, .. } = tx
  expect Some(utxo) =
    list.find(
      inputs,
      fn(input) {
        let Input { output_reference, .. } = input
        output_reference == own_ref
      },
    )
  let Input {
    output: Output { address: input_address, value: input_value, .. },
    ..
  } = utxo
  expect Address { payment_credential: Script(amm_policy_id), .. } =
    input_address

  expect Some(amm_output) =
    list.find(
      outputs,
      fn(output) {
        quantity_of(output.value, amm_policy_id, auth_token_name) == 1
      },
    )

  let x_add = quantity_of(input_value, asset_a_policy_id, asset_a_asset_name)
  let y_add = quantity_of(input_value, asset_b_policy_id, asset_b_asset_name)
  let x_pool =
    quantity_of(amm_output.value, asset_a_policy_id, asset_a_asset_name) - locked_ada
  let y_pool =
    quantity_of(amm_output.value, asset_b_policy_id, asset_b_asset_name)
  expect and {
      x_pool > 0,
      y_pool > 0,
      x_add > 0,
      y_add > 0,
    }
  x_add / y_add == x_pool / y_pool
}

fn check_spend_withdraw(tx: Transaction, datum: Option<Datum>) {
  let Transaction { outputs, .. } = tx
  expect Some(Datum { lp_amount, amm_policy_id }) = datum
  expect Some(amm_output) =
    list.find(
      outputs,
      fn(output) {
        quantity_of(output.value, amm_policy_id, auth_token_name) == 1
      },
    )

  let x_pool =
    quantity_of(amm_output.value, asset_a_policy_id, asset_a_asset_name) - locked_ada
  let y_pool =
    quantity_of(amm_output.value, asset_b_policy_id, asset_b_asset_name)
  expect Some(old_lp_amount) = sqrt(x_pool * y_pool)
  let withdraw_percent =
    100 - ( old_lp_amount - lp_amount ) * 100 / old_lp_amount
  let x_withdraw = x_pool - x_pool * withdraw_percent / 100
  let y_withdraw = y_pool - y_pool * withdraw_percent / 100
  expect and {
      x_withdraw > 0,
      y_withdraw > 0,
      x_pool > 0,
      y_pool > 0,
    }
  x_pool / y_pool == x_withdraw / y_withdraw
}

validator amm {
  spend(
    datum: Option<Datum>,
    redeemer: AmmRedeemer,
    own_ref: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
      Swap -> check_swap(own_ref, tx)
      Deposit -> check_spend_deposit(own_ref, tx)
      Withdraw -> check_spend_withdraw(tx, datum)
      _ -> False
    }
  }

  mint(redeemer: AmmRedeemer, amm_policy_id: PolicyId, tx: Transaction) {
    expect redeemer == CreatePool
    let Transaction { mint, outputs, .. } = tx
    expect [amm_output, ..] = outputs
    let x =
      quantity_of(amm_output.value, asset_a_policy_id, asset_a_asset_name) - locked_ada
    let y = quantity_of(amm_output.value, asset_b_policy_id, asset_b_asset_name)
    expect Some(lp_amount) = sqrt(x * y)
    expect and {
        x > 0,
        y > 0,
      }
    expect quantity_of(amm_output.value, amm_policy_id, auth_token_name) == 1
    expect amm_output.address.payment_credential == Script(amm_policy_id)
    let expected_mint_value =
      from_asset(amm_policy_id, lp_token_name, lp_amount)
        |> add(amm_policy_id, auth_token_name, 1)
    mint == expected_mint_value
  }

  else(_) {
    fail
  }
}

fn get_test_mint_tx(
  amm_address: Address,
  amm_policy_id: PolicyId,
) -> Transaction {
  let out_ref = mock_utxo_ref(2, 1)
  let amount_in =
    from_asset(asset_a_policy_id, asset_a_asset_name, 1_000_000_000)
      |> add(asset_b_policy_id, asset_b_asset_name, 1_000_000_000)
      |> add(ada_policy_id, ada_asset_name, 10_000_000_000)
  let amm_output_value =
    from_asset(asset_a_policy_id, asset_a_asset_name, 1_000_000_000)
      |> add(asset_b_policy_id, asset_b_asset_name, 1_000_000_000)
      |> add(amm_policy_id, auth_token_name, 1)
      |> add(ada_policy_id, ada_asset_name, 3_000_000)
  mocktail.mocktail_tx()
    |> mocktail.tx_in(
        condition: True,
        tx_hash: out_ref.transaction_id,
        tx_index: out_ref.output_index,
        amount: amount_in,
        address: mock_pub_key_address(0, None),
      )
    |> mocktail.tx_out(
        condition: True,
        address: amm_address,
        amount: amm_output_value,
      )
    |> mocktail.tx_out_inline_datum(
        condition: True,
        datum: Datum { lp_amount: 1_000_000_000, amm_policy_id },
      )
    |> mocktail.mint(
        condition: True,
        quantity: 1,
        policy_id: amm_policy_id,
        token_name: auth_token_name,
      )
    |> mocktail.mint(
        condition: True,
        quantity: 1000000000,
        policy_id: amm_policy_id,
        token_name: lp_token_name,
      )
    |> mocktail.complete()
}

fn get_test_swap_tx(
  amm_address: Address,
  amm_policy_id: PolicyId,
  out_ref: OutputReference,
) -> Transaction {
  let amm_out_ref = mock_utxo_ref(2, 1)
  let amount_in = from_asset(asset_a_policy_id, asset_a_asset_name, 100_000_000)
  let amount_out = from_asset(asset_b_policy_id, asset_b_asset_name, 90_909_091)
  let amm_amount_in =
    from_asset(asset_a_policy_id, asset_a_asset_name, 1_000_000_000)
      |> add(asset_b_policy_id, asset_b_asset_name, 1_000_000_000)
      |> add(ada_policy_id, ada_asset_name, 10_000_000_000)

  let amm_output_value =
    from_asset(asset_a_policy_id, asset_a_asset_name, 1_000_000_000)
      |> add(asset_b_policy_id, asset_b_asset_name, 1_000_000_000)
      |> add(amm_policy_id, auth_token_name, 1)
      |> add(ada_policy_id, ada_asset_name, 3_000_000)

  mocktail.mocktail_tx()
    |> mocktail.tx_in(
        condition: True,
        tx_hash: amm_out_ref.transaction_id,
        tx_index: amm_out_ref.output_index,
        amount: amm_amount_in,
        address: amm_address,
      )
    |> mocktail.tx_out(
        condition: True,
        address: mock_pub_key_address(0, None),
        amount: amount_out,
      )
    |> mocktail.tx_in(
        condition: True,
        tx_hash: out_ref.transaction_id,
        tx_index: out_ref.output_index,
        amount: amount_in,
        address: mock_script_address(0, None),
      )
    |> mocktail.tx_out(
        condition: True,
        address: amm_address,
        amount: amm_output_value,
      )
    |> mocktail.complete()
}

fn get_test_deposit_tx(
  amm_address: Address,
  amm_policy_id: PolicyId,
  out_ref: OutputReference,
) -> Transaction {
  let amm_out_ref = mock_utxo_ref(2, 1)
  let amount_in =
    from_asset(asset_a_policy_id, asset_a_asset_name, 100_000_000)
      |> add(asset_b_policy_id, asset_b_asset_name, 100_000_000)

  let amm_amount_in =
    from_asset(asset_a_policy_id, asset_a_asset_name, 1_000_000_000)
      |> add(asset_b_policy_id, asset_b_asset_name, 1_000_000_000)
      |> add(ada_policy_id, ada_asset_name, 10_000_000_000)

  let amm_output_value =
    from_asset(asset_a_policy_id, asset_a_asset_name, 1_100_000_000)
      |> add(asset_b_policy_id, asset_b_asset_name, 1_100_000_000)
      |> add(amm_policy_id, auth_token_name, 1)
      |> add(ada_policy_id, ada_asset_name, 3_000_000)

  mocktail.mocktail_tx()
    |> mocktail.tx_in(
        condition: True,
        tx_hash: amm_out_ref.transaction_id,
        tx_index: amm_out_ref.output_index,
        amount: amm_amount_in,
        address: amm_address,
      )
    |> mocktail.tx_out(
        condition: True,
        address: amm_address,
        amount: amm_output_value,
      )
    |> mocktail.mint(
        condition: True,
        quantity: 1,
        policy_id: amm_policy_id,
        token_name: auth_token_name,
      )
    |> mocktail.mint(
        condition: True,
        quantity: 1100000000,
        policy_id: amm_policy_id,
        token_name: lp_token_name,
      )
    |> mocktail.tx_in(
        condition: True,
        tx_hash: out_ref.transaction_id,
        tx_index: out_ref.output_index,
        amount: amount_in,
        address: mock_script_address(0, None),
      )
    |> mocktail.complete()
}

fn get_test_withdraw_tx(
  amm_address: Address,
  amm_policy_id: PolicyId,
) -> Transaction {
  let amm_out_ref = mock_utxo_ref(2, 1)
  let amount_out =
    from_asset(asset_a_policy_id, asset_a_asset_name, 100_000_000)
      |> add(asset_b_policy_id, asset_b_asset_name, 100_000_000)
  let amm_amount_in =
    from_asset(asset_a_policy_id, asset_a_asset_name, 1_000_000_000)
      |> add(asset_b_policy_id, asset_b_asset_name, 1_000_000_000)
      |> add(ada_policy_id, ada_asset_name, 10_000_000_000)

  let amm_output_value =
    from_asset(asset_a_policy_id, asset_a_asset_name, 900_000_000)
      |> add(asset_b_policy_id, asset_b_asset_name, 900_000_000)
      |> add(amm_policy_id, auth_token_name, 1)
      |> add(ada_policy_id, ada_asset_name, 3_000_000)

  mocktail.mocktail_tx()
    |> mocktail.tx_in(
        condition: True,
        tx_hash: amm_out_ref.transaction_id,
        tx_index: amm_out_ref.output_index,
        amount: amm_amount_in,
        address: amm_address,
      )
    |> mocktail.tx_out(
        condition: True,
        address: amm_address,
        amount: amm_output_value,
      )
    |> mocktail.tx_out_inline_datum(
        condition: True,
        datum: Datum { lp_amount: 1_000_000_000, amm_policy_id },
      )
    |> mocktail.tx_in(
        condition: True,
        tx_hash: amm_out_ref.transaction_id,
        tx_index: amm_out_ref.output_index,
        amount: amm_amount_in,
        address: amm_address,
      )
    |> mocktail.tx_out(
        condition: True,
        amount: amount_out,
        address: mock_script_address(0, None),
      )
    |> mocktail.tx_out(
        condition: True,
        address: amm_address,
        amount: amm_output_value,
      )
    |> mocktail.mint(
        condition: True,
        quantity: 1,
        policy_id: amm_policy_id,
        token_name: auth_token_name,
      )
    |> mocktail.mint(
        condition: True,
        quantity: 900000000,
        policy_id: amm_policy_id,
        token_name: lp_token_name,
      )
    |> mocktail.complete()
}

test test_create_pool() {
  let amm_address = mock_script_address(0, None)
  let amm_script_hash =
    #"122469f4894e616d69a4f9303598b073b8024ef868f670ae98bda107"
  let tx = get_test_mint_tx(amm_address, amm_script_hash)
  amm.mint(CreatePool, amm_script_hash, tx)
}

test test_swap() {
  let out_ref = mock_utxo_ref(2, 1)
  let amm_address = mock_script_address(0, None)
  let amm_script_hash =
    #"122469f4894e616d69a4f9303598b073b8024ef868f670ae98bda107"
  let tx = get_test_swap_tx(amm_address, amm_script_hash, out_ref)
  amm.spend(None, Swap, out_ref, tx)
}

test test_deposit() {
  let out_ref = mock_utxo_ref(2, 1)
  let amm_address = mock_script_address(0, None)
  let amm_script_hash =
    #"122469f4894e616d69a4f9303598b073b8024ef868f670ae98bda107"
  let tx = get_test_deposit_tx(amm_address, amm_script_hash, out_ref)
  expect amm.spend(None, Deposit, out_ref, tx)
  amm.mint(CreatePool, amm_script_hash, tx)
}

test test_withdraw() {
  let out_ref = mock_utxo_ref(2, 1)
  let amm_address = mock_script_address(0, None)
  let amm_script_hash =
    #"122469f4894e616d69a4f9303598b073b8024ef868f670ae98bda107"
  let datum = Datum { lp_amount: 100000000, amm_policy_id: amm_script_hash }
  let tx = get_test_withdraw_tx(amm_address, amm_script_hash)
  expect amm.spend(Some(datum), Withdraw, out_ref, tx)
  amm.mint(CreatePool, amm_script_hash, tx)
}
