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
