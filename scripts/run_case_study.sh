#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
OUT_JSON="${1:-$ROOT_DIR/docs/data/case-study-results.json}"
OUT_DIR="$ROOT_DIR/target/stellar/case-study"
NETWORK="${NETWORK:-${STELLAR_NETWORK:-testnet}}"
SOURCE_ALIAS="${SOURCE_ALIAS:-${STELLAR_ACCOUNT:-default}}"

PACKAGES=(
  soroban-case-study-increment-manual
  soroban-case-study-increment-auto
  soroban-case-study-increment-symbolic
  soroban-case-study-account-manual
  soroban-case-study-account-auto
  soroban-case-study-account-symbolic
  soroban-case-study-token-lite-manual
  soroban-case-study-token-lite-auto
  soroban-case-study-token-lite-symbolic
)

mkdir -p "$OUT_DIR" "$(dirname "$OUT_JSON")"

command -v stellar >/dev/null
command -v jq >/dev/null

ADMIN_ADDRESS="$(stellar keys address "$SOURCE_ALIAS")"
MEASURED_AT="$(date -u +"%Y-%m-%dT%H:%M:%SZ")"

deployments_file="$(mktemp)"
measurements_file="$(mktemp)"
trap 'rm -f "$deployments_file" "$measurements_file"' EXIT

ensure_identity() {
  local alias_name="$1"
  if stellar keys public-key "$alias_name" >/dev/null 2>&1; then
    return
  fi
  stellar keys generate "$alias_name" >/dev/null
}

create_account_if_missing() {
  local alias_name="$1"
  local starting_balance="${2:-10000000}"
  local address
  address="$(stellar keys public-key "$alias_name")"
  if stellar tx new payment \
    --source-account "$SOURCE_ALIAS" \
    --destination "$address" \
    --amount 1 \
    --network "$NETWORK" \
    --build-only >/dev/null 2>&1; then
    return
  fi

  stellar tx new create-account \
    --source-account "$SOURCE_ALIAS" \
    --destination "$address" \
    --starting-balance "$starting_balance" \
    --network "$NETWORK" >/dev/null
}

invoke() {
  local contract_id="$1"
  shift
  stellar contract invoke \
    --id "$contract_id" \
    --source-account "$SOURCE_ALIAS" \
    --network "$NETWORK" \
    "$@"
}

invoke_send() {
  local contract_id="$1"
  shift
  invoke "$contract_id" --send yes -- "$@" >/dev/null
}

invoke_view() {
  local contract_id="$1"
  shift
  invoke "$contract_id" --send no -- "$@"
}

record_deployment() {
  local family="$1"
  local variant="$2"
  local package="$3"
  local contract_id="$4"
  jq -nc \
    --arg family "$family" \
    --arg variant "$variant" \
    --arg package "$package" \
    --arg contract_id "$contract_id" \
    '{family: $family, variant: $variant, package: $package, contract_id: $contract_id}' \
    >> "$deployments_file"
}

record_measurement() {
  local family="$1"
  local variant="$2"
  local key_name="$3"
  local durability="$4"
  local bytes="$5"
  jq -nc \
    --arg family "$family" \
    --arg variant "$variant" \
    --arg key_name "$key_name" \
    --arg durability "$durability" \
    --argjson bytes "$bytes" \
    '{family: $family, variant: $variant, key: $key_name, durability: $durability, bytes: $bytes}' \
    >> "$measurements_file"
}

build_package() {
  local package="$1"
  stellar contract build --optimize --package "$package" --out-dir "$OUT_DIR" --profile contract >/dev/null
}

deploy_package() {
  local wasm="$1"
  local output
  output="$(stellar contract deploy --wasm "$wasm" --source-account "$SOURCE_ALIAS" --network "$NETWORK" 2>&1)"
  printf '%s\n' "$output" >&2
  printf '%s\n' "$output" | tail -n 1
}

if [[ "$NETWORK" == "mainnet" ]]; then
  OWNER_ALIAS="${OWNER_ALIAS:-mainnet-study-owner}"
  SPENDER_ALIAS="${SPENDER_ALIAS:-mainnet-study-spender}"
  RECIPIENT_ALIAS="${RECIPIENT_ALIAS:-mainnet-study-recipient}"

  ensure_identity "$OWNER_ALIAS"
  ensure_identity "$SPENDER_ALIAS"
  ensure_identity "$RECIPIENT_ALIAS"

  create_account_if_missing "$OWNER_ALIAS" 10000000
  create_account_if_missing "$SPENDER_ALIAS" 10000000
  create_account_if_missing "$RECIPIENT_ALIAS" 10000000

  OWNER_ADDRESS="$(stellar keys address "$OWNER_ALIAS")"
  SPENDER_ADDRESS="$(stellar keys address "$SPENDER_ALIAS")"
  RECIPIENT_ADDRESS="$(stellar keys address "$RECIPIENT_ALIAS")"
else
  OWNER_ADDRESS="$(stellar keys address "${OWNER_ALIAS:-blaine}")"
  SPENDER_ADDRESS="$(stellar keys address "${SPENDER_ALIAS:-bob}")"
  RECIPIENT_ADDRESS="$(stellar keys address "${RECIPIENT_ALIAS:-testuser}")"
fi

echo "Building case-study contracts..."
for package in "${PACKAGES[@]}"; do
  build_package "$package"
done

echo "Deploying and measuring increment variants..."
for variant in manual auto symbolic; do
  package="soroban-case-study-increment-$variant"
  wasm="$OUT_DIR/${package//-/_}.wasm"
  contract_id="$(deploy_package "$wasm")"
  record_deployment "increment" "$variant" "$package" "$contract_id"
  invoke_send "$contract_id" increment
  record_measurement "increment" "$variant" "counter" "instance" "$(invoke_view "$contract_id" counter_key_len)"
done

echo "Deploying and measuring account variants..."
for variant in manual auto symbolic; do
  package="soroban-case-study-account-$variant"
  wasm="$OUT_DIR/${package//-/_}.wasm"
  contract_id="$(deploy_package "$wasm")"
  record_deployment "account" "$variant" "$package" "$contract_id"
  invoke_send "$contract_id" initialize --owner "$OWNER_ADDRESS" --guardian "$SPENDER_ADDRESS"
  invoke_send "$contract_id" bump_nonce
  record_measurement "account" "$variant" "owner" "instance" "$(invoke_view "$contract_id" owner_key_len)"
  record_measurement "account" "$variant" "guardian" "instance" "$(invoke_view "$contract_id" guardian_key_len)"
  record_measurement "account" "$variant" "nonce" "instance" "$(invoke_view "$contract_id" nonce_key_len)"
done

echo "Deploying and measuring token-lite variants..."
for variant in manual auto symbolic; do
  package="soroban-case-study-token-lite-$variant"
  wasm="$OUT_DIR/${package//-/_}.wasm"
  contract_id="$(deploy_package "$wasm")"
  record_deployment "token-lite" "$variant" "$package" "$contract_id"
  invoke_send "$contract_id" initialize --admin "$ADMIN_ADDRESS"
  invoke_send "$contract_id" mint --to "$OWNER_ADDRESS" --amount 100
  invoke_send "$contract_id" mint --to "$RECIPIENT_ADDRESS" --amount 25
  invoke_send "$contract_id" approve --owner "$OWNER_ADDRESS" --spender "$SPENDER_ADDRESS" --amount 50
  record_measurement "token-lite" "$variant" "admin" "instance" "$(invoke_view "$contract_id" admin_key_len)"
  record_measurement "token-lite" "$variant" "balance(address)" "persistent" "$(invoke_view "$contract_id" balance_key_len --addr "$OWNER_ADDRESS")"
  record_measurement "token-lite" "$variant" "allowance(owner,spender)" "persistent" "$(invoke_view "$contract_id" allowance_key_len --owner "$OWNER_ADDRESS" --spender "$SPENDER_ADDRESS")"
done

jq -n \
  --arg measured_at "$MEASURED_AT" \
  --arg network "$NETWORK" \
  --arg source_alias "$SOURCE_ALIAS" \
  --arg admin "$ADMIN_ADDRESS" \
  --arg owner "$OWNER_ADDRESS" \
  --arg spender "$SPENDER_ADDRESS" \
  --arg recipient "$RECIPIENT_ADDRESS" \
  --slurpfile deployments "$deployments_file" \
  --slurpfile measurements "$measurements_file" \
  '
  {
    measured_at: $measured_at,
    network: $network,
    source_alias: $source_alias,
    sample_addresses: {
      admin: $admin,
      owner: $owner,
      spender: $spender,
      recipient: $recipient
    },
    deployments: $deployments,
    measurements: $measurements,
    interaction_totals: [
      {
        family: "increment",
        interaction: "increment",
        variants: (
          $measurements
          | map(select(.family == "increment"))
          | group_by(.variant)
          | map({
              variant: .[0].variant,
              bytes: (map(select(.key == "counter").bytes) | add)
            })
        )
      },
      {
        family: "account",
        interaction: "initialize",
        variants: (
          $measurements
          | map(select(.family == "account"))
          | group_by(.variant)
          | map({
              variant: .[0].variant,
              bytes: (map(select(.key == "owner" or .key == "guardian" or .key == "nonce").bytes) | add)
            })
        )
      },
      {
        family: "account",
        interaction: "rotate_guardian",
        variants: (
          $measurements
          | map(select(.family == "account"))
          | group_by(.variant)
          | map({
              variant: .[0].variant,
              bytes: (map(select(.key == "guardian").bytes) | add)
            })
        )
      },
      {
        family: "token-lite",
        interaction: "mint",
        variants: (
          $measurements
          | map(select(.family == "token-lite"))
          | group_by(.variant)
          | map({
              variant: .[0].variant,
              bytes: (map(select(.key == "admin" or .key == "balance(address)").bytes) | add)
            })
        )
      },
      {
        family: "token-lite",
        interaction: "approve",
        variants: (
          $measurements
          | map(select(.family == "token-lite"))
          | group_by(.variant)
          | map({
              variant: .[0].variant,
              bytes: (map(select(.key == "allowance(owner,spender)").bytes) | add)
            })
        )
      },
      {
        family: "token-lite",
        interaction: "transfer_from",
        variants: (
          $measurements
          | map(select(.family == "token-lite"))
          | group_by(.variant)
          | map({
              variant: .[0].variant,
              bytes: (
                (map(select(.key == "allowance(owner,spender)").bytes) | add) +
                ((map(select(.key == "balance(address)").bytes) | add) * 2)
              )
            })
        )
      }
    ]
  }
  ' > "$OUT_JSON"

echo "Wrote case study results to $OUT_JSON"
