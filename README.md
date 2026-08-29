# UTL-ACS Synthesis Demo

A small Haskell project demonstrating a controlled synthesis workflow with formal interlocks:
- Security Context enforcement (`SANITIZE_INPUT` directive)
- Z_anchor failure triggers Refactoring (F) loop
- Convergence proof at `J=1.0` with final verification

## Structure
- `src/UTLACS/CoreOntology.hs` — Core types and state
- `src/UTLACS/TensorLogic.hs` — Formal operators and Z_anchor enforcement
- `app/Main.hs` — Simulation loop (failure then success)
- `tests/TensorLogicSpec.hs` — Unit, property, and integration tests

## Requirements
- Cabal and GHC (or Stack)

## Setup (Cabal)
```
cabal update
cabal build all --enable-tests
cabal test all
cabal run utl-acs-exe
```

## Setup (Stack)
```
stack setup
stack test
stack run
```

## Runtime Demo
Run `demo.sh` to assert Z_anchor failure first, then success:
```
./demo.sh
```
The script parses the output to prove containment and liveness.

## Docker
Build a hardened image:
```
docker build -t utl-acs:latest .
```
Run:
```
docker run --rm utl-acs:latest
```

## CI
GitHub Actions workflow in `.github/workflows/ci.yml` builds, tests, and runs the simulation.

## License
PolyForm Noncommercial 1.0.0: Free for noncommercial use; commercial use requires a paid license.

---

## 🔷 ARCF Governance

UTL-ACS integrates the [Alexandria Reality-Contact Framework](https://github.com/joshoshfield-a11y/alexandria-os) for disciplined validation of formal proof claims.

### Mechanism Walk

Every formal claim must survive mechanism scrutiny. The complete causal walk for UTL-ACS is documented in:

- [`arcf/mechanism-walk.yaml`](arcf/mechanism-walk.yaml)

This includes: inputs, internal states, all transitions (Se → T → I → Z → F), outputs, assumptions, observed/inferred/unobserved links, failure signatures, and falsifier.

### Metric Card

The zero-error synthesis claim maintains an ARCF metric record:

- [`metrics/zero-error-synthesis.yaml`](metrics/zero-error-synthesis.yaml)

**Status vector**: Semantic 4, Implementation 4, Operational 1

> **Interpretation**: Formal structure is well-defined; implementation verified in simulation; real-world zero-error claim remains unverified outside Haskell domain.

### Authority Boundaries

| Claim | Status | May Automate? |
|-------|--------|---------------|
| Haskell type-safe synthesis | Tested mechanism | T3 (bounded, with monitoring) |
| Zero-error general claim | Unverified observation | T1 only (exploratory) |
| Cross-language transfer | Hypothesis | T2 (reversible pilot) |
