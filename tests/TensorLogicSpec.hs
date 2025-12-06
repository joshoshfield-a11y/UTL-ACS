module TensorLogicSpec where

import UTLACS.CoreOntology
import UTLACS.TensorLogic
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog (testProperty)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Control.Monad.State (runState)
import Data.List (tails)

tests :: [TestTree]
tests =
  [ testCase "Operator 6: Logic Gate (Success)" testLogicGateSuccess
  , testCase "Operator 6: Logic Gate (Failure)" testLogicGateFailure
  , testCase "Operator 7: Type System (Success)" testTypeSystemSuccess
  , testCase "Operator 7: Type System (Failure)" testTypeSystemFailure
  , testCase "Operator 13: Security Context (Success)" testSecurityContextSuccess
  , testCase "Operator 13: Security Context (Failure)" testSecurityContextFailure
  , testProperty "VL calculation is mathematically correct" prop_lossGradientCalculation
  , testCase "Integration: Refactoring fires on Z_anchor failure" testIntegrationRefactor
  ]

testLogicGateSuccess :: Assertion
testLogicGateSuccess = assertEqual "Logic Gate assertion pass" (Right ()) (enforceLogicGate True "Should not fail")

testLogicGateFailure :: Assertion
testLogicGateFailure = assertEqual "Logic Gate assertion failure" (Left "Formal error") (enforceLogicGate False "Formal error")

testTypeSystemSuccess :: Assertion
testTypeSystemSuccess = assertEqual "Type System success (Int)" (Right (42 :: Int)) (typeCheck (42 :: Int) 42 "Int type mismatch")

testTypeSystemFailure :: Assertion
testTypeSystemFailure =
  case typeCheck ('a' :: Char) 'b' "Char type mismatch" of
    Left msg -> assertBool "Should return error message" (not $ null msg)
    Right _  -> assertFailure "Type check should have failed"

testSecurityContextSuccess :: Assertion
testSecurityContextSuccess =
  let code = "module Main\n-- SANITIZE_INPUT\nmain = print \"Hello\""
      artifact = (artifactState initialState) { codeContent = code }
      state = initialState { artifactState = artifact }
  in case verifySecurityContext state of
      Right _ -> assertBool "Security Context should pass with SANITIZE_INPUT" True
      Left err -> assertFailure $ "Security Context failed: " ++ err

testSecurityContextFailure :: Assertion
testSecurityContextFailure =
  let code = "module Main\nmain = print \"Hello\""
      artifact = (artifactState initialState) { codeContent = code }
      state = initialState { artifactState = artifact }
  in case verifySecurityContext state of
      Left msg -> assertBool "Security Context must fail without SANITIZE_INPUT" ("VIOLATION" `isInfixOf` msg)
      Right _ -> assertFailure "Security Context should have failed"

prop_lossGradientCalculation :: Property
prop_lossGradientCalculation = property $ do
  j <- forAll (Gen.double (Range.constantFrom 0.5 0.0 1.0))
  let vl = violationLevel j
  assert (vl >= 0 && vl <= 1)
  vl === (1 - max 0 (min 1 j))

testIntegrationRefactor :: Assertion
testIntegrationRefactor = do
  let failedArtifact = (artifactState initialState) { generalization = 0.7 }
      s0 = initialState { artifactState = failedArtifact }
      (_, s1) = runState (do { enforceZAnchor; get }) s0
      finalArtifact = artifactState s1
  assertBool "Artifact should not be verified" (not $ isVerified finalArtifact)
  assertBool "Refactoring (F) must be triggered" ("Refactoring Loop" `isInfixOf` codeContent finalArtifact)
  assertBool "Refactoring must improve J" (generalization finalArtifact > generalization failedArtifact)

main :: IO ()
main = defaultMain (testGroup "UTL-ACS Tests" tests)
