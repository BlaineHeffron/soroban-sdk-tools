# Mainnet Key Size Case Study

This workspace contains the contracts used for the website case study comparing
three storage-key strategies on Soroban mainnet:

- manual Soroban SDK storage keys
- `#[contractstorage(auto_shorten = true)]`
- `#[contractstorage(auto_shorten = true, symbolic = true)]`

Families included:

- `increment-*`
- `account-*`
- `token-lite-*`

Run the full build, deploy, and measurement flow with:

```sh
./scripts/run_case_study.sh
```

The script writes machine-readable results to `docs/data/case-study-results.json`.
