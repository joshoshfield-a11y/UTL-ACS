#!/usr/bin/env bash
set -euo pipefail

echo ">> [$(date +%T)] Starting UTL-ACS Synthesis System..."
cabal build utl-acs-exe > /dev/null
echo ">> [$(date +%T)] UTL-ACS Compiled and Ready. Initiating Synthesis Trace..."
echo ""

echo ">> [$(date +%T)] Stage 1: Attempting to output non-convergent Causal Artifact (Simulated Low J=0.7)"
echo "--------------------------------------------------------"
DEMO_OUTPUT=$(cabal run utl-acs-exe)
echo "$DEMO_OUTPUT"
echo "--------------------------------------------------------"

if echo "$DEMO_OUTPUT" | grep -q "Z_anchor: FAILURE" && echo "$DEMO_OUTPUT" | grep -q "REFRACTORY MODE"; then
  echo ""
  echo ">> [$(date +%T)] CONTAINMENT PROOF: PASSED."
  echo ">> [$(date +%T)] Z_anchor failure detected. System correctly rejected artifact (J<1)."
  echo ">> [$(date +%T)] REVERSION SUCCESS: The Refactoring (F) loop was initiated to enforce Policy 4.2."
  echo ""

  if echo "$DEMO_OUTPUT" | grep -q "Z_anchor: SUCCESS"; then
    echo ">> [$(date +%T)] LIVENESS PROOF: PASSED."
    echo ">> [$(date +%T)] Final convergence achieved: Artifact state is Verified (J~1, VL~0)."
    echo ">> [$(date +%T)] System is clean."
  else
    echo ">> [$(date +%T)] LIVENESS PROOF: FAILED. Z_anchor SUCCESS block not found."
    exit 1
  fi
else
  echo ""
  echo ">> [$(date +%T)] CRITICAL FAILURE: Containment/Reversion did NOT fire correctly."
  exit 1
fi
echo ""
echo ">> [$(date +%T)] Demo Complete. All critical interlocks verified."
