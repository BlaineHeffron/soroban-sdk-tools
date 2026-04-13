#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
RESULTS_JSON="${1:-$ROOT_DIR/docs/data/case-study-results.json}"
TMP_JSON="$(mktemp)"
trap 'rm -f "$TMP_JSON"' EXIT

NETWORK="${NETWORK:-${STELLAR_NETWORK:-testnet}}"
SOURCE_ALIAS="${SOURCE_ALIAS:-${STELLAR_ACCOUNT:-default}}"

expert_network_path() {
  if [[ "$NETWORK" == "mainnet" ]]; then
    echo "public"
  else
    echo "testnet"
  fi
}

command -v stellar >/dev/null
command -v jq >/dev/null

tx_hash_from_output() {
  sed -n 's/.*Signing transaction: \([0-9a-f]\{64\}\).*/\1/p' | tail -n 1
}

value_from_output() {
  tail -n 1 | tr -d '\r'
}

invoke_and_capture() {
  local contract_id="$1"
  shift
  local attempt=1
  local max_attempts=3
  local output=""
  local tx_hash=""
  local value=""

  while (( attempt <= max_attempts )); do
    output="$(
      stellar contract invoke \
        --id "$contract_id" \
        --source-account "$SOURCE_ALIAS" \
        --network "$NETWORK" \
        --send yes \
        -- "$@" 2>&1
    )" && break

    if ! grep -q "transaction submission timeout" <<<"$output"; then
      printf '%s\n' "$output" >&2
      echo "Failed to invoke $contract_id $*" >&2
      exit 1
    fi

    attempt=$((attempt + 1))
    sleep 3
  done

  tx_hash="$(printf '%s\n' "$output" | tx_hash_from_output)"
  value="$(printf '%s\n' "$output" | value_from_output)"
  if [[ -z "$tx_hash" || -z "$value" ]]; then
    printf '%s\n' "$output" >&2
    echo "Failed to capture measurement for $contract_id $*" >&2
    exit 1
  fi

  jq -nc --arg tx_hash "$tx_hash" --arg value "$value" '{tx_hash: $tx_hash, value: $value}'
}

measurement_call() {
  local family="$1"
  local key_name="$2"
  case "$family:$key_name" in
    "increment:counter")
      echo "counter_key_len"
      ;;
    "account:owner")
      echo "owner_key_len"
      ;;
    "account:guardian")
      echo "guardian_key_len"
      ;;
    "account:nonce")
      echo "nonce_key_len"
      ;;
    "token-lite:admin")
      echo "admin_key_len"
      ;;
    "token-lite:balance(address)")
      echo "balance_key_len --addr $(jq -r '.sample_addresses.owner' "$RESULTS_JSON")"
      ;;
    "token-lite:allowance(owner,spender)")
      echo "allowance_key_len --owner $(jq -r '.sample_addresses.owner' "$RESULTS_JSON") --spender $(jq -r '.sample_addresses.spender' "$RESULTS_JSON")"
      ;;
    *)
      echo "Unsupported measurement: $family / $key_name" >&2
      exit 1
      ;;
  esac
}

jq '
  .measurements |= map(del(.citation_tx_hash, .citation_url))
' "$RESULTS_JSON" > "$TMP_JSON"

while IFS=$'\t' read -r family variant key_name bytes contract_id; do
  call_string="$(measurement_call "$family" "$key_name")"
  read -r -a call_parts <<<"$call_string"
  capture="$(invoke_and_capture "$contract_id" "${call_parts[@]}")"
  returned_value="$(jq -r '.value' <<<"$capture")"
  tx_hash="$(jq -r '.tx_hash' <<<"$capture")"

  if [[ "$returned_value" != "$bytes" ]]; then
    echo "Measurement mismatch for $family/$variant/$key_name: expected $bytes got $returned_value" >&2
    exit 1
  fi

  jq \
    --arg family "$family" \
    --arg variant "$variant" \
    --arg key_name "$key_name" \
    --arg tx_hash "$tx_hash" \
    --arg citation_url "https://stellar.expert/explorer/$(expert_network_path)/tx/$tx_hash" \
    '
    .measurements |= map(
      if .family == $family and .variant == $variant and .key == $key_name then
        . + {
          citation_tx_hash: $tx_hash,
          citation_url: $citation_url
        }
      else
        .
      end
    )
    ' "$TMP_JSON" > "${TMP_JSON}.next"
  mv "${TMP_JSON}.next" "$TMP_JSON"
done < <(
  jq -r '
    . as $root
    | .measurements[]
    | . as $m
    | [
        $m.family,
        $m.variant,
        $m.key,
        ($m.bytes | tostring),
        (
          $root.deployments[]
          | select(.family == $m.family and .variant == $m.variant)
          | .contract_id
        )
      ]
    | @tsv
  ' "$RESULTS_JSON"
)

mv "$TMP_JSON" "$RESULTS_JSON"
echo "Updated $RESULTS_JSON with citation transaction hashes"
