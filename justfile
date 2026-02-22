set dotenv-load := true

export PATH := './target/bin:' + env_var('PATH')
export CONFIG_DIR := 'target/'

[private]
path:
    just --list

# build contracts
build:
    just _build soroban-errors-external-contract
    just _build soroban-errors-calc-contract
    just _build soroban-errors-contract
    just _build soroban-auth-vault
    just _build soroban-auth-token
    just _build soroban-auth-swap

[private]
_build arg:
    stellar contract build --optimize --package {{ arg }} --out-dir target/stellar --profile contract

# Setup the project to use a pinned version of the CLI
setup:
    git config core.hooksPath .githooks
    -cargo binstall -y stellar-cli --version 23.3.0 --force --install-path ./target/bin
