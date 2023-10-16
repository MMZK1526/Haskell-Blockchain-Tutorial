module Model.BCEnv where

import           Data.Aeson (FromJSON, ToJSON)
import           GHC.Generics
import           Model.Block
import           Model.Transaction

data BCEnv
  = BCEnv { blockchains :: [Block]
          , mempool     :: [Transaction] }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
