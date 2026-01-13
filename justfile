set dotenv-load := true

export PATH := './target/bin:' + env_var('PATH')
export CONFIG_DIR := 'target/'

[private]
path:
    just --list

# build contracts
build:
    stellar contract build --optimize --package soroban-errors-external-contract --out-dir target/stellar
    stellar contract build --optimize --package soroban-errors-contract --out-dir target/stellar

# Setup the project to use a pinned version of the CLI
setup:
    git config core.hooksPath .githooks
    -cargo binstall -y stellar-cli --version 23.3.0 --force --install-path ./target/bin
