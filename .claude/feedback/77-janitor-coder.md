---
agent: Ray Gutierrez
background: Building custodian at a university for 22 years, self-taught programmer who builds maintenance scheduling apps on nights and weekends, speaks in practical metaphors
date: 2026-03-21
---

# Review by Ray Gutierrez

## Overall Impression

I maintain a building with 400 rooms, 12 HVAC zones, 6 elevator banks, and about 2,000 things that can break on any given day. I also write code on nights and weekends. So when I read this codebase, I see two things at once: a software architecture and a building system. And honestly? This architecture makes more sense than half the building management systems I deal with.

Let me explain this project the way I understand it, in building terms.

## Strengths

1. **The two-trait split is like separating the master key system from the task list.** In my building, there is a master key system (who can access which rooms) and there are work orders (what needs to be done in each room). These are different systems. The key system does not care about the work -- it only cares about whether you are allowed in. The work order does not care about keys -- it only cares about the task. `OwnableInternal` is the work order (what to do). The outer `Ownable` trait is the key system (who is allowed). Keeping them separate means I can change the lock system without rewriting every work order. Smart.

2. **The provider pattern is like having vendor contracts.** I do not fix elevators myself. I have a contract with Otis. If Otis does a bad job, I switch to ThyssenKrupp. Same elevators, same maintenance schedule, different vendor. `type Provider = SingleOwner` is my contract with a vendor. If SingleOwner does a bad job, I switch to MultisigOwner. Same building (contract), different vendor (provider). The building's tenants never notice the switch.

3. **The sealed macro is like a fire door.** Fire doors cannot be propped open (well, they should not be -- people do it anyway). They are there by code, they close automatically, and you cannot modify them without triggering an alarm. `impl_ownable!` is a fire door for authorization: it closes automatically, it cannot be propped open (overridden), and it is required by code (the security model). Fire doors save lives. Sealed macros save contracts.

4. **The tests are like building inspections.** The four tests in the example are like the four inspections I run: (a) does the system work normally? (b) does the safety system work? (c) does the key system prevent unauthorized access? (d) does the alarm system (AuthClient) actually trigger? Good coverage for a first pass.

## Concerns

1. **No maintenance schedule.** I maintain a 400-room building because I have a preventive maintenance schedule. Every 90 days: HVAC filters. Every year: fire extinguisher inspection. Every 5 years: elevator certification. This contract has no maintenance schedule. What about storage TTL renewal? What about key rotation? What about periodic health checks? A building without maintenance falls apart. A contract without maintenance becomes a liability.

2. **The `expect("not initialized")` is a pipe that bursts at 3 AM.** In my building, the pipes that burst are always the ones nobody thought about. `expect("not initialized")` is a pipe that works fine as long as someone remembered to call `init()`. But what if they did not? What if the initialization transaction failed silently? Then the first time anyone tries to use the contract, it panics. At 3 AM. With no recovery plan. I have seen this exact pattern in building systems: "the contractor was supposed to connect the backup generator, but they forgot, and nobody checked until the power went out."

3. **No redundancy.** My building has two boilers, two water pumps, and an emergency generator. If one fails, the other takes over. This contract has one owner key. If the key is lost or compromised, the entire contract is down with no failover. Where is the backup? Where is the emergency generator? A production system needs redundancy in its authorization, not just in its infrastructure.

4. **No signage.** In my building, every fire exit has a sign. Every electrical panel has a label. Every valve has a tag saying what it controls. This codebase has good module-level documentation, but the generated code has no "signage" -- when a developer encounters `__auth_addr` in a stack trace, there is no label explaining what it is or what generated it. Add signage. Label everything.

5. **The cleanup procedure is missing.** When a tenant moves out, I have a 30-point checklist: return keys, final inspection, utility disconnection, security code reset, forwarding address. When a contract is decommissioned... what? There is no `decommission()`, no `transfer_all_assets()`, no `revoke_all_access()`. Buildings that are abandoned without proper decommissioning become hazards. Contracts that are abandoned without proper decommissioning become liabilities.

## Suggestions

1. **Add a "Building Manual" -- a maintenance checklist for deployed contracts.**
   - Monthly: verify owner key is still accessible
   - Quarterly: extend storage TTL for all critical entries
   - Annually: rotate owner key to a new address
   - Ad-hoc: review provider for known vulnerabilities

2. **Implement a "backup key" mechanism.** Like a building's master key that is stored in a safe at the management office, there should be a recovery mechanism. A pre-configured backup address that can claim ownership if the primary owner is non-responsive for N days.

3. **Add status indicators.** My building has a panel that shows the status of every system: HVAC (green), elevators (green), fire alarm (green), water pressure (yellow). Generate a `status()` method that returns the health of the contract: initialized (yes/no), owner set (yes/no), paused (yes/no), storage TTL remaining (N ledgers).

4. **Label the generated code.** Add comments to the macro output that say:
   ```rust
   // Generated by #[contracttrait] macro from trait Ownable
   // This auth check was specified by #[auth(Self::owner)]
   // Do not modify -- use impl_ownable! macro to change behavior
   ```
   These are the labels on the electrical panel. They prevent the next person from making a mistake.

5. **Write a "Tenant Guide" -- documentation for contract consumers.** Not the developer who writes the contract, but the developer who interacts with it. What methods can I call? What authorization do I need? What errors can I expect? This is the tenant handbook: "Here is how the building works. Here is what to do if something breaks."

## Unique Perspective

I have maintained a building for 22 years. The building does not care about my elegant maintenance plans or my clever scheduling algorithms. It cares about whether the boiler works when it is cold, the AC works when it is hot, and the locks work when someone tries to break in.

Smart contracts are buildings. They house assets and activities. They need access control, maintenance, redundancy, signage, and decommissioning plans. They need someone who checks on them regularly and fixes things before they break.

This codebase builds good locks (auth enforcement) and good key management (provider pattern). But a building is more than its locks. It needs a maintenance schedule, a backup plan, status indicators, and a decommissioning procedure.

The developers who build with this tool will not have a janitor checking on their contracts every night. The tool itself needs to be the janitor -- or at least provide the janitor's checklist.

## Would I Use This?

If I were building a smart contract (which I might -- I have been thinking about a maintenance scheduling system on-chain for our building network), I would use this for the access control layer. The provider pattern is exactly how I think about vendor contracts, and the sealed macro is exactly how fire doors should work. But I would need to add my own maintenance schedule, redundancy, and decommissioning procedures. The tool gives me good locks. I need to supply the rest of the building management system.
