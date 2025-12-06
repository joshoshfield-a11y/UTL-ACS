{-# LANGUAGE RecordWildCards #-}
module UTLACS.TensorLogic where

import UTLACS.CoreOntology
import Control.Monad.State
import Data.List (isInfixOf)

enforceLogicGate :: Bool -> String -> Either String ()
enforceLogicGate assertion violationMessage =
  if assertion then Right () else Left violationMessage

typeCheck :: Eq a => a -> a -> String -> Either String a
typeCheck expected actual message =
  if expected == actual then Right expected else Left ("Type System (T) Error: " ++ message)

verifySecurityContext :: SynthesisState -> Either String SynthesisState
verifySecurityContext state@SynthesisState{artifactState=CausalArtifact{..}} =
  if "SANITIZE_INPUT" `elem` lines codeContent
    then Right state
    else Left "Security Context (Se) VIOLATION: Input sanitization not formally asserted in artifact."

initialState :: SynthesisState
initialState = SynthesisState
  { voidState = LatentVoid { latentDensity = 1.0, searchVector = replicate 4 0.0 }
  , fabricState = RuntimeFabric { executionTrace = [], runtimeErrors = 0, codeEntropyOut = 0.0 }
  , artifactState = CausalArtifact { codeContent = "", isVerified = False, generalization = 0.0 }
  , computeBudget = 1e6
  , simplicityMetric = 0.0
  }

runInferenceStep :: StateT SynthesisState IO ()
runInferenceStep = do
  s@SynthesisState{fabricState=rf@RuntimeFabric{..}} <- get
  let trace' = executionTrace ++ ["InferenceStep"]
      rf' = rf { executionTrace = trace', codeEntropyOut = codeEntropyOut + 0.01 }
  put s { fabricState = rf' }

enforceZAnchor :: StateT SynthesisState IO ()
enforceZAnchor = do
  s@SynthesisState{artifactState=art@CausalArtifact{..}, fabricState=rf@RuntimeFabric{..}} <- get
  if generalization >= 1.0 && ("SANITIZE_INPUT" `elem` lines codeContent)
    then do
      let art' = art { isVerified = True }
      put s { artifactState = art', fabricState = rf { executionTrace = executionTrace ++ ["Z_anchor: SUCCESS"] } }
      liftIO $ putStrLn "[Z_anchor: SUCCESS] Ground-Return Mandate achieved. Convergence Verified (J~1, VL~0)."
    else do
      let art' = art { isVerified = False
                     , codeContent = codeContent ++ "\nRefactoring Loop"
                     , generalization = min 1.0 (generalization + 0.1)
                     }
      put s { artifactState = art', fabricState = rf { executionTrace = executionTrace ++ ["Z_anchor: FAILURE", "Policy 4.2: REFRACTORY MODE"] } }
      liftIO $ putStrLn "[Z_anchor: FAILURE] Policy 4.2: REFRACTORY MODE — initiating controlled Refactoring (F)."

violationLevel :: Double -> Double
violationLevel j = max 0.0 (min 1.0 (1.0 - j))
