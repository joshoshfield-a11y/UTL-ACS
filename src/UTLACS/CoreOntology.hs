{-# LANGUAGE RecordWildCards #-}
module UTLACS.CoreOntology where

import Control.DeepSeq (NFData(..))

data LatentVoid = LatentVoid
  { latentDensity :: Double
  , searchVector  :: [Double]
  } deriving (Show, Eq)

instance NFData LatentVoid where
  rnf LatentVoid{..} = rnf latentDensity `seq` rnf searchVector

data RuntimeFabric = RuntimeFabric
  { executionTrace :: [String]
  , runtimeErrors  :: Int
  , codeEntropyOut :: Double
  } deriving (Show, Eq)

instance NFData RuntimeFabric where
  rnf RuntimeFabric{..} = rnf executionTrace `seq` rnf runtimeErrors `seq` rnf codeEntropyOut

data CausalArtifact = CausalArtifact
  { codeContent     :: String
  , isVerified      :: Bool
  , generalization  :: Double
  } deriving (Show, Eq)

instance NFData CausalArtifact where
  rnf CausalArtifact{..} = rnf codeContent `seq` rnf isVerified `seq` rnf generalization

data SynthesisState = SynthesisState
  { voidState        :: LatentVoid
  , fabricState      :: RuntimeFabric
  , artifactState    :: CausalArtifact
  , computeBudget    :: Double
  , simplicityMetric :: Double
  } deriving (Show, Eq)
