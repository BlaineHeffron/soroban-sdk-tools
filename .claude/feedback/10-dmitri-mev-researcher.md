---
agent: Dmitri Volkov
background: Former Ethereum MEV researcher at Flashbots, now analyzing economic attack surfaces on non-EVM chains, published "Extractable Value in Stellar" at FC 2025
date: 2026-03-21
---

# Review by Dmitri Volkov

## Overall Impression

I come from the world of MEV -- maximal extractable value -- where the question is never "does the code have a bug?" but rather "given correct code, can an economically rational actor extract value at the expense of other users?" This codebase focuses heavily on authorization correctness, which is necessary but insufficient. The economic attack surface of composed smart contracts extends well beyond "who is allowed to call what."

The architecture is clean. The provider pattern is interesting from an MEV perspective because it concentrates economic logic in a single, swappable component. But there are economic attack vectors that the documentation does not acknowledge.

## Strengths

1. **The auth caching pattern (`let __auth_addr = Self::Provider::owner(env)`) prevents a subtle front-running attack.** If the auth resolution and the `require_auth()` call were separate (e.g., check owner, then some logic, then require_auth on a different address), a sandwich attack could change the owner between resolution and check. Caching the resolved address eliminates this vector. This is good.

2. **The sealed macro prevents governance extraction attacks.** On Ethereum, one of the most devastating MEV vectors is governance manipulation: if an attacker can override the authorization logic, they can steal the entire protocol. `impl_ownable!` makes this class of attack impossible at the code level. The attack would have to target the deployment pipeline instead.

3. **The provider pattern enables rapid response to economic attacks.** If a `SingleOwner` deployment is being exploited via social engineering of the owner key, the protocol can deploy a new contract with `MultisigOwner` without rewriting business logic. This is valuable for incident response.

## Concerns

1. **The `owner()` function as an auth source is a single point of failure for economic extraction.** The `#[auth(Self::owner)]` pattern means every guarded action depends on a single storage read. If an attacker compromises the owner key (via phishing, key extraction, or social engineering), they gain control of ALL guarded methods simultaneously. This is the "god key" problem. Compare this to role-based systems where compromising one role does not grant access to others. The documentation should discuss this threat model explicitly and recommend multi-role setups for high-value deployments.

2. **No reentrancy analysis.** The provider pattern means business logic is executed AFTER the auth check. But what if the business logic itself makes a cross-contract call that triggers a reentrant call to the same contract? Soroban's execution model provides some protection here (it uses a different model than EVM), but the documentation should explicitly state whether the generated code is reentrant-safe and under what conditions.

3. **The pause mechanism has a griefing vector.** If `Pausable` and `Ownable` share the same owner (which is the default -- `SingleOwner` implements both), then the owner can grief all users by pausing the contract indefinitely. In a DeFi context, this is an economic attack: the owner pauses the contract, then executes a trade on another venue that profits from the locked liquidity. The documentation should recommend separating pause authority from other authorities in DeFi deployments.

4. **No rate limiting or cooldown periods.** `transfer_ownership` can be called repeatedly with no cooldown. An attacker who gains temporary access to the owner key can transfer ownership, extract value, and transfer back -- all in a single transaction bundle on Stellar. This is a flash-loan-style attack adapted to authorization. A cooldown period on ownership transfers would mitigate this.

5. **The `FungibleToken` example in the blog post has a classic balance manipulation issue.** The provider code:
   ```rust
   BalanceStorage::update_balances(env, &from, |b| b.unwrap_or(0) - amount);
   BalanceStorage::update_balances(env, &to, |b| b.unwrap_or(0) + amount);
   ```
   If `from == to`, this is a no-op that should be explicitly handled. More importantly, negative balances from integer underflow should be checked. The `unwrap_or(0) - amount` pattern will panic on underflow in debug mode but wrap in release mode (depending on Soroban's integer overflow behavior). This is a critical economic vulnerability.

6. **No consideration of oracle manipulation.** If the `owner()` function's return value depends on external state (e.g., a governance token's majority holder), an attacker could manipulate that external state to temporarily become the owner, execute privileged actions, and then revert the manipulation. The provider pattern makes this easy to implement but dangerous if done naively.

## Suggestions

1. **Add a "Threat Model" document.** Explicitly enumerate: (a) what attacks the sealed macro prevents, (b) what attacks the auth caching prevents, (c) what attacks require additional mitigation (rate limiting, timelocks, role separation), and (d) what attacks are out of scope (key compromise, oracle manipulation).

2. **Implement a `TimelockOwner` provider as a reference.** This provider should enforce a minimum delay between `transfer_ownership` initiation and execution. The delay creates a window for monitoring systems to detect and respond to unauthorized transfers.

3. **Add a `#[reentrancy_guard]` attribute.** Even if Soroban's model prevents EVM-style reentrancy, cross-contract calls on Stellar can still create complex call graphs. A simple "entered" flag in storage would prevent reentrant calls to guarded methods.

4. **Document the `from == to` transfer case.** Should it be a no-op? Should it revert? Should it emit an event but skip the balance update? This is an economic design decision that should be explicit.

5. **Consider generating a "circuit breaker" in the outer trait.** If a guarded method is called more than N times within a single ledger close, automatically pause the contract. This mitigates extraction attacks that rely on rapid repeated calls.

## Unique Perspective

In MEV research, we have a saying: "The bug is not in the code; the bug is in the incentives." This codebase does an excellent job of ensuring that the code correctly enforces authorization. But authorization correctness is only half the story. The other half is: given that authorization is enforced, can an authorized actor exploit their position for economic gain at the expense of other participants?

The provider pattern is a double-edged sword here. It makes legitimate upgrades easy (swap `SingleOwner` for `MultisigOwner`), but it also makes illegitimate upgrades easy (swap `SafeProvider` for `MaliciousProvider`). The person who controls the provider choice controls the economic rules of the system. The documentation should be explicit about this: the provider pattern shifts the trust assumption from "trust the code" to "trust the deployer's choice of provider."

In DeFi, the deployer IS often the attacker. The provider pattern needs governance constraints around provider selection -- not just at deployment time, but for upgrades.

## Would I Use This?

For simple ownership patterns (admin keys for non-financial contracts), yes, immediately. For DeFi protocols with significant TVL, not without additional economic safeguards: timelocks, rate limits, role separation, and a formal threat model. The authorization layer is solid; the economic layer needs more thought.
